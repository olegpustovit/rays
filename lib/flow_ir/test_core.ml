module C = Flow_ir.Core
let source : C.source = {site=["g";"value"];owner=["g"];instance=2;prefix=[];iter=[Static_index 3];span=Some {start=10;finish=20}}
let s id node : C.statement = {id;source;node}
let program constants locals body results : C.program =
  {constants;locals;body;results;state_slots=[||];regions=[||];call_slots=0}
let check p expected =
  let actual = Format.asprintf "%a" C.pp p in
  if actual <> expected then failwith ("Unexpected core print:\n" ^ actual);
  assert (actual = Format.asprintf "%a" C.pp p)
let () =
  check (program [|C.Int 2|] [|Flow.Ty.Int;Float;Float|]
    [s 0 (Assign (0,Constant 0));s 1 (Assign (1,Frame Time));
     s 2 (Assign (2,Prim (Numeric Mul,["a",0;"b",1])))] [|2|])
{|core
  $0 = 2
  %0 : int
  %1 : float
  %2 : float
  #0 %0 = $0 @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  #1 %1 = frame.t @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  #2 %2 = *("a":%0 "b":%1) @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  return %2
|};
  check (program [|C.Text "yes";C.Text "no";C.Bool true|] [|Flow.Ty.Bool;Text|]
    [s 0 (Assign (0,Constant 2));
     s 1 (If {condition=0;yes=[s 2 (Assign (1,Constant 0))];no=[s 3 (Assign (1,Constant 1))]})] [|1|])
{|core
  $0 = "yes"
  $1 = "no"
  $2 = true
  %0 : bool
  %1 : text
  #0 %0 = $2 @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  #1 if %0 then {
    #2 %1 = $0 @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  } else {
    #3 %1 = $1 @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  } @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  return %1
|};
  let p = program [|C.List [|C.Float 1.;C.Float 2.|];C.Float 0.|]
    [|Flow.Ty.List Float;Int;Float;Float|]
    [s 0 (Assign (0,Constant 0));
     s 1 (State_read {slot=0;result=3;init=[s 2 (Assign (3,Constant 1))];initial=3});
     s 3 (Iterate {sequence=0;index=1;item=2;limit=4096;
       body=[s 4 (Assign (3,Prim (Numeric Add,["a",3;"b",2])))]});
     s 5 (State_write {slot=0;value=3})] [|3|] in
  check {p with state_slots=[|{source;prefix=["call"];zone=["g";"s"];plane=`Frame;ty=Flow.Ty.Float}|]}
{|core
  $0 = list[1.0 2.0]
  $1 = 0.0
  %0 : list:float
  %1 : int
  %2 : float
  %3 : float
  state[0] : float plane=frame prefix=["call"] zone=["g" "s"] @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  #0 %0 = $0 @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  #1 %3 = state[0] init {
    #2 %3 = $1 @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  } value=%3 @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  #3 iterate %0 index=%1 item=%2 limit=4096 do {
    #4 %3 = +("a":%3 "b":%2) @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  } @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  #5 state[0] <- %3 @["g" "value"] owner=["g"] instance=2 iter=[3] span=10..20
  return %3
|}

(* Packed is the validated Core.Region representation itself. *)
let () =
  let packed = Test_program.compile "(map (fn [x] (+ (* x 2) t)) (array/range 4))" in
  let region = packed in
  assert (Atomic.get region.dependent = None);
  assert (Array.length (C.Region.dependent_flags region) = Array.length region.code);
  assert (Option.is_some (Atomic.get region.dependent));
  check {(program [||] [||] [] [||]) with regions = [|region|]}
{|core
  region[0] zip collect count=4 widths=[1] output=[4] uniforms=0 skip=[] stages=1
    r0 = input[0].0/1
    r1 = 2.0
    r2 = *(r0 r1)
    r3 = frame.t
    r4 = +(r2 r3)
  return 
|};
  assert (region == packed)
