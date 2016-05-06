open Parsetree
open Std_utils
open Res.Err_monad_infix

open Parsed_patches.Type

let apply_replacements tree attributes var_replacements =
  (* TODO : Keep location from the original AST *)
  let open! Option.Infix in
  let new_tree = List.find_opt
      (fun x -> (fst x).Asttypes.txt = "__sempatch_replace")
      attributes
    >|= (function
        | _, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> e
        | id, _ -> raise Failure.(SempatchException
                                    (Replacement (id.Asttypes.loc))
                                 )
      )
    |> Option.value tree
  in
  let mapper = Ast_mapper.(
      { default_mapper with
        expr = (fun self e ->
            match e.pexp_desc with
            | Pexp_ident { Asttypes.txt = Longident.Lident i; _} ->
              Environment.get_expr i var_replacements
              >|= (fun expr -> { e with pexp_desc = expr.pexp_desc; })
              |> Option.value e
            | _ -> default_mapper.expr self e
          );
      })
  in
  mapper.Ast_mapper.expr mapper new_tree

let apply patch expr =
  let is_meta_expr e = List.mem e (patch.header.meta_expr)
  and apply_to_list mapper env patch elements =
    List.fold_left (fun mapped elt ->
        let%bind mapped_elts, accu_env = mapped in
        let%map mapped_elt, new_env = mapper env patch elt in
        mapped_elt :: mapped_elts, new_env :: accu_env
      )
      (Res.fail ([], []))
      elements
  and merge_envs = Environment.merge in
  let rec match_at_root =
    let open Ast_maybe_mapper2 in
    let default = mk Environment.merge in
    {
      expr = (fun self env
               ~patch:({ pexp_desc = e2; pexp_attributes = attrs2; _ } as expr2)
               ~expr:({ pexp_desc = e1; _ } as expr1) ->
          let replacements =
            match e1, e2 with
            | Pexp_constant c1, Pexp_constant c2 when c1 = c2 -> Ok (expr1, env)
            | _, Pexp_ident { Asttypes.txt = Longident.Lident j; _ }
              when is_meta_expr j ->
              (* TODO (one day...)
                 treat the case where j is already defined as an expression *)
              Ok (expr1, Environment.add_expr j expr1 env)
            | Pexp_ident i, Pexp_ident j when i.Asttypes.txt = j.Asttypes.txt ->
              Ok (expr1, env)
            | _, Pexp_extension (loc, PStr [
                { pstr_desc = Pstr_eval (e, _); _ }
              ]) when loc.Asttypes.txt = "__sempatch_inside" ->
              apply_to_expr env ~expr:expr1 ~patch:e
            | _ -> default.expr self env ~expr:expr1 ~patch:expr2
          in
          let replacements = Error.bind (fun (e, env) ->
              if
                try
                  Guard_evaluator.eval_union
                    env.Environment.current_match
                    patch.header.guard
                with
                | Guard_evaluator.Undefined_var _ -> true
                | Guard_evaluator.TypeError ->
                  let patch_name = patch.header.name in
                  let msg = Printf.sprintf
                      "The guard of patch %s is incorrect"
                      patch_name
                  in
                  raise (Failure.SempatchException (Failure.Guard msg))
                | Guard_evaluator.Undefined_function f ->
                  let patch_name = patch.header.name in
                  let msg = Printf.sprintf
                      "The function %s in the guard of patch %s is undefined"
                      f patch_name
                  in
                  raise Failure.( SempatchException (Guard msg))
              then
                Ok (e, env)
              else
                Error (e, env)) replacements
          in
          let result = match replacements with
            | Ok (expr, attrs) -> Ok (
                expr,
                Environment.set_matches [
                  (Match.mk patch attrs.Environment.current_match expr.pexp_loc)
                  ]
                  attrs
              )
            | Error (expr, attrs) ->
              Error (expr, Environment.set_matches [] attrs)
          in
          Error.map
            (fun (e, env) -> apply_replacements e attrs2 env, env)
            result
        );
      pattern = (fun self env ~patch:pat2 ~pat:pat1 ->
          let replacements =
            match pat1.ppat_desc, pat2.ppat_desc with
            | Ppat_var { Asttypes.txt = v; _ },
              Ppat_var { Asttypes.txt = v'; _ }
              when is_meta_expr v' ->
              Ok (pat1, Environment.add_ident v'  v env)
            | Ppat_var v,
              Ppat_var v'
              when v.Asttypes.txt = v'.Asttypes.txt ->
              Ok (pat1, env)
            | _ -> default.pattern self env ~patch:pat2 ~pat:pat1
          in replacements
        )
    }
  and apply_to_expr env ~patch ~expr =
    let open Ast_maybe_mapper2 in
    let desc_err =
      match expr.pexp_desc with
      | Pexp_ident _ | Pexp_constant _
      | Pexp_new _ | Pexp_pack _
      | Pexp_object _
        as d -> Error.fail (d, [env])

      | Pexp_apply (fct, [lbl, arg]) ->
        let%bind mapped_expr, env_expr = apply_to_expr env ~expr:fct ~patch in
        let%map mapped_arg, env_arg = apply_to_expr env ~expr:arg ~patch in
        Pexp_apply (mapped_expr, [lbl, mapped_arg]), [env_expr; env_arg]

      | Pexp_apply _ ->
        raise Failure.(SempatchException (Non_implemented (expr.pexp_loc)))

      | Pexp_fun(lbl, default, pat, expr) ->
        let%bind mapped_default, env_default =
          apply_to_maybe_expr env patch default
        in
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_fun (lbl, mapped_default, pat, mapped_expr),
        [ env_expr; env_default]

      | Pexp_let (isrec, bindings, expr) ->
        let%bind mapped_bindings, env_bindings =
          apply_to_bindings env patch bindings
        in
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_let (isrec, mapped_bindings, mapped_expr),
        env_expr :: env_bindings

      | Pexp_tuple expr_list ->
        let%map mapped_list, env_list = apply_to_exprs env patch expr_list in
        Pexp_tuple mapped_list, env_list

      | Pexp_ifthenelse (cif, cthen, celse) ->
        let%bind mapped_cif, env_cif = apply_to_expr env ~expr:cif ~patch in
        let%bind mapped_cthen, env_cthen = apply_to_expr env ~expr:cthen ~patch
        in
        let%map mapped_celse, env_celse = apply_to_maybe_expr env patch celse in
        Pexp_ifthenelse (mapped_cif, mapped_cthen, mapped_celse),
        [ env_cif; env_cthen; env_celse ]

      | Pexp_function cases ->
        let%map mapped_cases, env_cases = apply_to_cases env patch cases in
        Pexp_function mapped_cases, env_cases

      | Pexp_construct (ident, expr) ->
        let%map mapped_expr, env_expr = apply_to_maybe_expr env patch expr in
        Pexp_construct (ident, mapped_expr), [env_expr]

      | Pexp_variant (ident, expr) ->
        let%map mapped_expr, env_expr = apply_to_maybe_expr env patch expr in
        Pexp_variant (ident, mapped_expr), [env_expr]

      | Pexp_match (expr, cases) ->
        let%bind mapped_cases, env_cases = apply_to_cases env patch cases in
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_match (mapped_expr, mapped_cases),
        env_expr :: env_cases

      | Pexp_try (expr, cases) ->
        let%bind mapped_cases, env_cases = apply_to_cases env patch cases in
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_try (mapped_expr, mapped_cases),
        env_expr :: env_cases

      | Pexp_record (fields, base_record) ->
        let%bind mapped_fields, envs_field =
          List.fold_left (fun accu field ->
              let%bind accu_lst, accu_env = accu in
              let%map mapped_field, env_field =
                apply_to_reverse_field env patch field
              in
              mapped_field :: accu_lst, env_field :: accu_env
            )
            (Error ([], []))
            fields
        in
        let%map mapped_base, env_base =
          apply_to_maybe_expr env patch base_record
        in
        Pexp_record (mapped_fields, mapped_base), env_base :: envs_field

      | Pexp_field (expr, ident) ->
        let%map (expr, ident), env = apply_to_field env patch (expr, ident) in
        Pexp_field (expr, ident), [env]

      | Pexp_setfield (expr1, ident, expr2) ->
        let%bind mapped_expr1, env_expr1 =
          apply_to_expr env ~expr:expr1 ~patch
        in
        let%map mapped_expr2, env_expr2 = apply_to_expr env ~expr:expr2 ~patch
        in
        Pexp_setfield (mapped_expr1, ident, mapped_expr2),
        [env_expr1; env_expr2]

      | Pexp_sequence (expr1, expr2) ->
        let%bind mapped_expr1, env_expr1 = apply_to_expr env ~expr:expr1 ~patch
        in
        let%map mapped_expr2, env_expr2 = apply_to_expr env ~expr:expr2 ~patch
        in
        Pexp_sequence (mapped_expr1, mapped_expr2),
        [env_expr1; env_expr2]

      | Pexp_array exprs ->
        let%map exprs, env = apply_to_exprs env patch exprs in
        Pexp_array exprs, env

      | Pexp_assert expr ->
        let%map expr, env = apply_to_expr env ~expr ~patch in
        Pexp_assert expr, [env]

      | Pexp_lazy expr ->
        let%map expr, env = apply_to_expr env ~expr ~patch in
        Pexp_lazy expr, [env]

      | Pexp_while (cond, expr) ->
        let%bind mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        let%map mapped_cond, env_cond = apply_to_expr env ~expr:cond ~patch in
        Pexp_while (mapped_cond, mapped_expr), [env_expr; env_cond]

      | Pexp_for (pat, e1, e2, dir, e3) ->
        let%bind mapped_e1, env_e1 = apply_to_expr env ~expr:e1 ~patch in
        let%bind mapped_e2, env_e2 = apply_to_expr env ~expr:e2 ~patch in
        let%map  mapped_e3, env_e3 = apply_to_expr env ~expr:e3 ~patch in
        Pexp_for (pat, mapped_e1, mapped_e2, dir, mapped_e3),
        [env_e1; env_e2; env_e3]

      | Pexp_constraint (expr, typ) ->
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_constraint (mapped_expr, typ), [env_expr]

      | Pexp_poly (expr, typ_opt) ->
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_poly (mapped_expr, typ_opt), [env_expr]

      | Pexp_coerce (expr, typ_opt, typ) ->
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_coerce (mapped_expr, typ_opt, typ), [env_expr]

      | Pexp_send (expr, met) ->
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_send (mapped_expr, met), [env_expr]

      | Pexp_setinstvar (dest, expr) ->
        let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
        Pexp_setinstvar (dest, mapped_expr), [env_expr]

      | Pexp_override overrides ->
        let apply_to_override env patch (name, expr) =
          let%map expr, env = apply_to_expr env ~patch ~expr in
          (name, expr), env
        in
        let%map overrides, envs =
          apply_to_list apply_to_override env patch overrides
        in
        Pexp_override overrides, envs

      | Pexp_letmodule (name, module_def, expr) ->
        let%map expr, env = apply_to_expr env ~patch ~expr in
        Pexp_letmodule (name, module_def, expr), [env]

      | Pexp_newtype (name, expr) ->
        let%map expr, env = apply_to_expr env ~patch ~expr in
        Pexp_newtype (name,  expr),
        [env]

      | Pexp_open (override, module_id, expr) ->
        let%map expr, env = apply_to_expr env ~patch ~expr in
        Pexp_open (override, module_id, expr), [env]

      | Pexp_extension (name, ext) ->
        let%map ext, env = apply_to_ext env patch (name, ext) in
        Pexp_extension (name, ext), [env]

    in
    let%bind mapped_desc, env_exprs = desc_err in
    let self_expr = { expr with pexp_desc = mapped_desc } in
    let%map mapped_self, env_self = 
      match_at_root.expr match_at_root env ~expr:self_expr ~patch
    in
    mapped_self, (List.fold_left merge_envs env_self env_exprs)

  and apply_to_field env patch (expr, ident) =
    let%map mapped_expr, env_expr = apply_to_expr env ~expr ~patch in
    (mapped_expr, ident), env_expr

  and apply_to_reverse_field env patch (ident, expr) =
    let%map (expr, ident), env = apply_to_field env patch (expr, ident) in
    (ident, expr), env

  and apply_to_case env patch { pc_lhs; pc_guard; pc_rhs } =
    let%bind mapped_guard, env_guard = apply_to_maybe_expr env patch pc_guard in
    let%map mapped_rhs, env_rhs = apply_to_expr env ~expr:pc_rhs ~patch in
    {
      pc_lhs;
      pc_guard = mapped_guard;
      pc_rhs = mapped_rhs;
    },
    merge_envs env_guard env_rhs

  and apply_to_exprs env patch =
    apply_to_list
      (fun env patch expr -> apply_to_expr env ~patch ~expr)
      env
      patch

  and apply_to_cases env patch = apply_to_list apply_to_case env patch

  and apply_to_binding env patch binding =
    let%map expr, env = apply_to_expr env ~expr:binding.pvb_expr ~patch in
    { binding with pvb_expr = expr }, env

  and apply_to_bindings env patch bindings =
    apply_to_list apply_to_binding env patch bindings

  and apply_to_maybe_expr env patch =
    function
    | Some expr ->
      let%map expr, env = apply_to_expr env ~expr ~patch in
      Some expr, env
    | None -> Error (None, env)

  and apply_to_ext env patch (id, payload) =
    match payload with
    | PPat (pat, expr_opt) ->
      let%map expr, env = apply_to_maybe_expr env patch expr_opt in
      PPat (pat, expr), env
    | PStr str ->
      let%map str, env = apply_to_structure env patch str in
      PStr str, env
    | _ -> raise Failure.(SempatchException (Non_implemented (id.Asttypes.loc)))

  and apply_to_structure_item env patch str_item =
    let%map desc, envs =
      match str_item.pstr_desc with
      | Pstr_eval (expr, attributes) ->
        let%map mapped_expr, env_expr = apply_to_expr env ~patch ~expr in
        Pstr_eval (mapped_expr, attributes), [env_expr]
      | Pstr_value (isrec, bindings) ->
        let%map bindings, envs = apply_to_bindings env patch bindings in
        Pstr_value (isrec, bindings), envs

      | _ -> raise Failure.(SempatchException
                              (Non_implemented (str_item.pstr_loc)))
    in
    { str_item with pstr_desc = desc},
    List.fold_left merge_envs Environment.empty envs

  and apply_to_structure env patch structure =
    let%map structure, envs =
      apply_to_list apply_to_structure_item env patch structure
    in
    structure,
    List.fold_left merge_envs Environment.empty envs

  in
  let expr = Parsed_patches.preprocess_src_expr expr
  and patch = Parsed_patches.preprocess patch
  in
  apply_to_expr Environment.empty ~expr ~patch:(patch.body)
  |> Res.map (fun (tree, env) ->
      Parsed_patches.postprocess tree, env.Environment.matches
    )
