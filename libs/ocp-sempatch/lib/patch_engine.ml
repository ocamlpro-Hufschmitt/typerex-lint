open Ast_mapper

type t = mapper

let mkppx patches = {
  (* The resulting mapper basically applies all the patches in (reverse) order *)
  (* Overriding the structure and signature fields should be enough if we assume that we always apply ppxes to entire ASTs *)
  default_mapper with
  structure = (
    fun _ structure ->
      List.fold_left (fun iter elt -> elt.structure elt iter) structure (List.rev patches)
  );

  signature = (
    fun _ signature ->
      List.fold_left (fun iter elt -> elt.signature elt iter) signature (List.rev patches)
  );
}

let register name m = Ast_mapper.register name (fun _ -> mkppx m)
