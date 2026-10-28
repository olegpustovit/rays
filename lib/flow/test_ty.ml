open Flow
open Ty

let ty source =
  match Syntax.parse source with Ok [ form ] -> of_syntax form | _ -> failwith source

let () =
  (* annotations *)
  assert (ty "float" = Some Float && ty "fn" = Some (Fn None) && ty "geometry" = Some Ty.geometry);
  assert (ty "(list int)" = Some (List Int));
  assert (ty "(array int)" = Some (Array Int));
  assert (of_string "array:int" = Some (Array Int));
  assert (ty "(list (list vec3))" = Some (List (List Vec3)));
  assert (ty "{:shape geometry :y float}" = Some (Record [ ("shape", Ty.geometry); ("y", Float) ]));
  assert (ty "{}" = Some (Record []));
  List.iter
    (fun source -> assert (ty source = None))
    [
      "any";
      "color";
      "flot";
      "(list)";
      "(list int int)";
      "{:a}";
      "{:a int :a int}";
      "{:A int}";
      "{a int}";
      "(vec int)";
      "3";
    ];
  (* the study's string spelling round-trips *)
  let t = Record [ ("a", List (Record [ ("b", Int); ("c", List Float) ])); ("d", Text) ] in
  assert (to_string t = "rec{a:list:rec{b:int,c:list:float},d:text}");
  List.iter
    (fun t -> assert (of_string (to_string t) = Some t))
    [
      Ty.geometry;
      Float;
      Int;
      Bool;
      Vec2;
      Vec3;
      Vec4;
      Array Vec2;
      Array Vec4;
      Text;
      Color;
      Fn None;
      Any;
      Ty.scene;
      Ty.world;
      Ty.settings;
      Ty.panel;
      Ty.editor;
      List Int;
      t;
      Record [];
    ];
  List.iter (fun s -> assert (of_string s = None)) [ "nope"; "list:"; "rec{a:int"; "int," ]

let () =
  let signature = { params = [ Record [ ("p", Vec3) ]; List Text; Array Float ]; result = Float } in
  let typed = Fn (Some signature) in
  List.iter
    (fun t -> assert (of_string (to_string t) = Some t))
    [ typed; Fn (Some { params = []; result = typed }); Record [ ("f", typed) ]; List typed ];
  assert (fits typed (Fn None) && not (fits (Fn None) typed));
  assert (has_fn typed && not (fits typed (Fn (Some { signature with params = [ Vec3 ] }))));
  List.iter
    (fun s -> assert (of_string s = None))
    [ "fn("; "fn(float)"; "fn(float)->"; "fn(nope)->float"; "fn(float,)->float" ]

let () =
  assert (
    has_fn (Fn None) && has_fn (List (Fn None))
    && has_fn (Record [ ("f", Fn None) ])
    && (not (has_fn (List Int)))
    && not (has_fn Any));
  (* fits *)
  assert (fits Int Float && fits Float Int && fits Int Bool && fits Bool Float);
  assert (fits Int Vec3 && (not (fits Bool Vec3)) && not (fits Vec3 Float));
  assert (fits Text Color && fits Vec3 Color && not (fits Float Color));
  assert (fits Any Ty.geometry && fits Ty.geometry Any && not (fits Ty.geometry Float));
  assert (fits (List Int) (List Float) && not (fits (List Ty.geometry) (List Float)));
  assert (fits (List Any) (List Int));
  assert (fits (Record [ ("a", Int); ("b", Text) ]) (Record [ ("a", Float) ]));
  assert (not (fits (Record [ ("a", Int) ]) (Record [ ("a", Int); ("b", Int) ])));
  assert ((not (fits (List Int) Int)) && not (fits Int (List Int)));
  (* coerce *)
  assert (coerce Int Float = Float && coerce Float Int = Int && coerce Float Bool = Bool);
  assert (coerce Int Vec3 = Vec3 && coerce Bool Vec3 = Bool);
  assert (coerce (List Int) (List Float) = List Float);
  assert (
    coerce (Record [ ("a", Int); ("b", Text) ]) (Record [ ("a", Float) ])
    = Record [ ("a", Float); ("b", Text) ]);
  assert (coerce Text Any = Text && coerce Any Float = Any && coerce Ty.geometry Float = Ty.geometry);
  (* join and unify *)
  assert (join Int Int = Some Int && join Int Float = Some Float);
  assert (join Any Text = Some Text && join Vec3 Any = Some Vec3);
  assert (join Text Ty.geometry = None && join Int Bool = None);
  assert (join (List Int) (List Float) = Some (List Float));
  assert (
    join (Record [ ("a", Int); ("b", Text) ]) (Record [ ("b", Text); ("a", Float) ])
    = Some (Record [ ("a", Float); ("b", Text) ]));
  assert (join (Record [ ("a", Int) ]) (Record [ ("b", Int) ]) = None);
  assert (join (Record [ ("a", Int) ]) (Record [ ("a", Text) ]) = None);
  assert (unify Int Bool = Some Int && unify Ty.geometry Text = None);
  assert (elem (List Int) = Some Int && elem Int = None)

let () =
  List.iter
    (fun vector ->
      assert (of_string (to_string vector) = Some vector);
      assert (ty ("(array " ^ to_string vector ^ ")") = Some (Array vector));
      assert (fits Int vector && fits Float vector && not (fits Bool vector));
      assert (coerce Int vector = vector && coerce Float vector = vector);
      assert (join vector Any = Some vector && join vector vector = Some vector);
      List.iter
        (fun other ->
          if vector <> other then begin
            assert ((not (fits vector other)) && not (fits (Array vector) (Array other)));
            assert (coerce vector other = vector && join vector other = None)
          end)
        [ Vec2; Vec3; Vec4 ])
    [ Vec2; Vec4 ]
