open Rdk
open Rdk_test_support


let sign_positive = function Predicates.Positive -> true | _ -> false

let coordinate index salt =
  let value = ((index * 1_103_515_245) + (salt * 12_345) + 97) land max_int in
  float_of_int (value mod 10_007) +. (float_of_int index *. 1e-7)

let test_failures_and_cancellation () =
  (match Delaunay2.build ~x:[|0.|] ~y:[||] () with
   | Error _ -> () | Ok _ -> failf "mismatched coordinates succeeded");
  (match Delaunay2.build ~x:[|Float.nan|] ~y:[|0.|] () with
   | Error _ -> () | Ok _ -> failf "non-finite coordinate succeeded");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Delaunay2.build ~cancel ~x:[|0.;1.;0.|] ~y:[|0.;0.;1.|] () with
  | Error _ -> () | Ok _ -> failf "cancelled triangulation succeeded"

let run () =
  test_failures_and_cancellation ()
