let () =
  match Flow_sop.Manifest.generate ~extra:Editor_document.Contexts.descriptors
    Sop.Nodes.factories with
  | Ok (text, _) -> print_string text
  | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)
