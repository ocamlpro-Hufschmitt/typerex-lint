open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree


type t = Ast_mapper.mapper

let identity_mapper =
  let discard_first = fun _ x -> x in
  {
    attribute = discard_first;
    attributes = discard_first;
    case = discard_first;
    cases = discard_first;
    class_declaration = discard_first;
    class_description = discard_first;
    class_expr = discard_first;
    class_field = discard_first;
    class_signature = discard_first;
    class_structure = discard_first;
    class_type = discard_first;
    class_type_declaration = discard_first;
    class_type_field = discard_first;
    constructor_declaration = discard_first;
    expr = discard_first;
    extension = discard_first;
    extension_constructor = discard_first;
    include_declaration = discard_first;
    include_description = discard_first;
    label_declaration = discard_first;
    location = discard_first;
    module_binding = discard_first;
    module_declaration = discard_first;
    module_expr = discard_first;
    module_type = discard_first;
    module_type_declaration = discard_first;
    open_description = discard_first;
    pat = discard_first;
    payload = discard_first;
    signature = discard_first;
    signature_item = discard_first;
    structure = discard_first;
    structure_item = discard_first;
    typ = discard_first;
    type_declaration = discard_first;
    type_extension = discard_first;
    type_kind = discard_first;
    value_binding = discard_first;
    value_description = discard_first;
    with_constraint = discard_first;
  }

(** Mapper composition *)
let (>>) patch1 patch2 = patch2 :: patch1

let txt_is loc = (=) loc.txt

let pattern_is_id pattern id =
  match pattern with
  | { ppat_desc = Ppat_var loc } when txt_is loc id -> true
  | _ -> false (* TODO: Is there another pattern to look at ? *)

let binds_id binder id =
  match binder with
  | { pvb_pat = pat } when pattern_is_id pat id -> true
  | _ -> false

(** {2 Patches definition} *)

let rename_var ?(rename_def=true) old_name new_name =
  {
    default_mapper with
    expr =
      begin
        fun mapper expr ->
          match expr with
          | { pexp_desc = Pexp_ident desc; }
            when txt_is desc (Longident.Lident old_name) ->
            { expr with
              pexp_desc = Pexp_ident { desc with txt = Longident.Lident new_name };
            }
          | p -> default_mapper.expr mapper p
      end;
    pat =
      if rename_def then
        begin
          fun mapper pat ->
            match pat.ppat_desc with
            | Ppat_var { txt = id; loc; } when id = old_name ->
              { pat with
                ppat_desc = Ppat_var { txt = new_name; loc }
              }
            | p -> default_mapper.pat mapper pat
        end
      else default_mapper.pat
  }

let add_arg_fun fname arg_name =
  {
    default_mapper with
    value_binding =
      fun mapper binding ->
        if binds_id binding fname then
          let pattern =
            Pat.mk ~loc:binding.pvb_loc (Ppat_var { txt = arg_name; loc = binding.pvb_loc })
          in
          { binding with
            pvb_expr =
              { binding.pvb_expr with
                pexp_desc = Pexp_fun ("", None, pattern, binding.pvb_expr);
              }
          }
        else
          default_mapper.value_binding mapper binding
  }

let make_fun_call var_name default_arg = {
  default_mapper with
  expr = fun mapper expr ->
    match expr with
    | { pexp_desc = Pexp_ident i; pexp_attributes = attrs } when txt_is i (Longident.Lident var_name) ->
      { expr with
        pexp_desc = Pexp_apply (Exp.ident (Location.mkloc (Longident.Lident var_name) expr.pexp_loc), [ "", default_arg ]);
      }
    | e -> default_mapper.expr mapper e
}

let register name m = Ast_mapper.register name (fun _ -> Patch_engine.mkppx m)

let () =
  let patch =
    []
    >> add_arg_fun "f" "x"
    >> rename_var ">>" ">>!"
    >> make_fun_call "f" (Exp.constant (Const_int 2))
    >> rename_var "x" "y"
    >> rename_var "y" "z"
  in register "patch" patch
