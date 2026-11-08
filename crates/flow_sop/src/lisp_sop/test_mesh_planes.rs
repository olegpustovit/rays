//! lib/flow_sop/test_mesh_planes.ml checks(): exact bytes, independent ownership and diagnostic order.
use super::support::geometry_bytes;
use crate::topology_kernel;
use flow_syntax::value::Value;
use rdk_core::{geometry as geo, packed::Float3, topology};
use std::sync::Arc;

fn floats(xs: Vec<f64>) -> Value { Value::Floats(1, Arc::new(xs)) }
fn ints(xs: Vec<i64>) -> Value { Value::Int_array(Arc::new(xs)) }
fn fields(n: usize) -> Vec<(String, Value)> {
    vec![("x".into(), floats((0..n).map(|i| i as f64).collect())),
         ("y".into(), floats((0..n).map(|i| if i & 1 == 0 { -0.0 } else { f64::from_bits(1) }).collect())),
         ("z".into(), floats(vec![0.125; n]))]
}
fn replace(mut xs: Vec<(String, Value)>, key: &str, value: Value) -> Vec<(String, Value)> {
    xs.iter_mut().find(|(name, _)| name == key).unwrap().1 = value;
    xs
}
fn bad(value: &Value, number: f64) -> Value {
    let Value::Floats(1, xs) = value else { panic!("float array expected") };
    let mut xs = xs.as_ref().clone(); xs[0] = number; floats(xs)
}
fn args(points: Value) -> Vec<(String, Value)> {
    vec![("points".into(), points), ("vertex_points".into(), ints(vec![])),
         ("primitive_offsets".into(), ints(vec![0])), ("kinds".into(), ints(vec![]))]
}
fn ownership(n: usize) {
    for domains in [1, 4, 8] {
        parallel::run_unit(Some(domains), &|| {
            let mut input = fields(n);
            let plane = |i: usize| { let Value::Floats(1, xs) = &input[i].1 else { unreachable!() }; xs.as_ref().clone() };
            let expected = geo::create(Float3::of_owned(plane(0), plane(1), plane(2)).unwrap(), topology::empty(n as i64).unwrap(), None, None, None).unwrap();
            let before = geometry_bytes(&expected);
            let actual = topology_kernel::mesh(&args(Value::Record(input.clone()))).unwrap();
            assert_eq!(geometry_bytes(&actual), before);
            let output = Float3::View::view(geo::positions(&actual)).unwrap();
            for ((_, value), output) in input.iter_mut().zip([&output.x, &output.y, &output.z]) {
                let Value::Floats(1, xs) = value else { unreachable!() };
                assert!(n == 0 || xs.as_ptr() != output.as_ptr(), "mesh must own coordinate planes");
                if n > 0 { Arc::make_mut(xs)[0] = 99.0; }
            }
            assert_eq!(geometry_bytes(&actual), before, "caller mutation changed mesh");
            Ok(())
        }).unwrap();
    }
}
#[test] fn mesh_plane_empty() { ownership(0); }
#[test] fn mesh_plane_three() { ownership(3); }
#[test] fn mesh_plane_block() { ownership(65536); }
#[test] fn mesh_plane_block_plus_one() { ownership(65537); }

#[test]
fn mesh_plane_diagnostic_precedence() {
    let n = 65537; let input = fields(n);
    let (x,y,z) = (&input[0].1, &input[1].1, &input[2].1);
    let short = floats(vec![0.0; n-1]);
    let nonfinite = ("E_NONFINITE", "array input produced a nonfinite value.");
    let count = ("E_ARRAY_COUNT", "Mesh coordinate planes need the same element count.");
    let record = ("E_TOPOLOGY_TYPE", "Mesh planes require exactly x, y and z float arrays.");
    let cases = vec![
        ("first-plane", args(Value::Record(replace(input.clone(), "x", bad(x, f64::NAN)))), nonfinite),
        ("second-before-count", args(Value::Record(replace(replace(input.clone(), "z", short.clone()), "y", bad(y, f64::NAN)))), nonfinite),
        ("third-plane", args(Value::Record(replace(input.clone(), "z", bad(z, f64::INFINITY)))), nonfinite),
        ("multiple-before-count", args(Value::Record(vec![("z".into(), bad(z, f64::NEG_INFINITY)), ("y".into(), bad(y, f64::NAN)), ("x".into(), short.clone())])), nonfinite),
        ("finite-count", args(Value::Record(replace(input.clone(), "z", short.clone()))), count),
        ("malformed-before-finite", args(Value::Record(vec![("x".into(), bad(x,f64::NAN)), ("y".into(), ints(vec![])), ("z".into(), z.clone())])), record),
        ("missing-field", args(Value::Record(vec![("x".into(),x.clone()),("y".into(),y.clone())])), record),
        ("duplicate-field", args(Value::Record(vec![("x".into(),bad(x,f64::NAN)),("x".into(),x.clone()),("z".into(),z.clone())])), record),
        ("outer-storage-before-finite", replace(args(Value::Record(replace(input.clone(),"x",bad(x,f64::NAN)))),"vertex_points",floats(vec![])), ("E_TOPOLOGY_TYPE","Mesh requires vec3 points and integer vertex_points, primitive_offsets and kinds arrays.")),
        ("finite-before-kind", replace(args(Value::Record(replace(input.clone(),"x",bad(x,f64::NAN)))),"kinds",ints(vec![3])), nonfinite),
        ("count-before-kind", replace(args(Value::Record(replace(input.clone(),"z",short))),"kinds",ints(vec![3])), count),
        ("kind-before-topology", replace(args(Value::Record(input.clone())),"kinds",ints(vec![3])), ("E_TOPOLOGY_KIND","Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).")),
        ("topology-before-copy", vec![("points".into(),Value::Record(input)),("vertex_points".into(),ints(vec![n as i64])),("primitive_offsets".into(),ints(vec![0,1])),("kinds".into(),ints(vec![1]))], ("E_TOPOLOGY","Topology: vertex references an invalid point")),
    ];
    for domains in [1,4,8] {
        parallel::run_unit(Some(domains), &|| {
            for (name, values, (code,message)) in &cases {
                let error = topology_kernel::mesh(values).err().unwrap_or_else(|| panic!("{name} accepted"));
                assert_eq!((error.code.as_str(),error.message.as_str()),(*code,*message),"{name}");
                assert_eq!(error.cause,None,"{name}");
            }
            Ok(())
        }).unwrap();
    }
}
