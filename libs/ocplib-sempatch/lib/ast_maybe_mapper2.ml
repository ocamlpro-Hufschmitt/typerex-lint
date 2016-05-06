open Std_utils
open Parsetree
open Res.Ok_monad_infix

type 'a t = {
  expr : 'a t -> 'a -> patch:expression -> expr:expression
    -> (expression * 'a, expression * 'a) Error.t;
  pattern : 'a t -> 'a -> patch:pattern -> pat:pattern
    -> (pattern * 'a, pattern * 'a) Error.t;
}

let map_binding merge self env binding patch =
  let%bind mapped_pattern, env_pattern =
    self.pattern self env ~pat:binding.pvb_pat ~patch:patch.pvb_pat
  in
  let%map mapped_expr, env_expr =
    self.expr self env ~expr:binding.pvb_expr ~patch:patch.pvb_expr
  in
  { binding with pvb_pat = mapped_pattern; pvb_expr = mapped_expr; },
  merge env_pattern env_expr

let map_bindings merge self env bindings patch =
  try
    List.fold_left2 (fun accu binding patch_binding ->
        let%bind bind_list, env = accu in
        let%map mapped_binding, new_env =
          map_binding merge self env binding patch_binding
        in
        mapped_binding :: bind_list, merge env new_env
      )
      (Ok ([], env))
      bindings
      patch
  with
    Invalid_argument "List.fold_left2" -> Error (bindings, env)

let map_maybe_expr _merge self env expr_opt patch_opt =
  match expr_opt, patch_opt with
  | Some expr, Some patch ->
    let%map mapped, env = self.expr self env ~expr ~patch in Some mapped, env
  | None, None -> Ok (None, env)
  | _ -> Error (expr_opt, env)

let map_case merge self env
    { pc_lhs = lhs_e; pc_guard = guard_e; pc_rhs = rhs_e }
    { pc_lhs = lhs_patch; pc_guard = guard_patch; pc_rhs = rhs_patch }
  =
  let%bind mapped_lhs, env_lhs =
    self.pattern self env ~patch:lhs_patch ~pat:lhs_e
  in
  let%bind mapped_guard, env_guard =
    map_maybe_expr merge self env_lhs guard_e guard_patch
  in
  let%map mapped_rhs, env_rhs =
    self.expr self env_lhs ~expr:rhs_e ~patch:rhs_patch
  in
  {
    pc_lhs = mapped_lhs;
    pc_guard = mapped_guard;
    pc_rhs = mapped_rhs;
  },
  merge env_lhs (merge env_guard env_rhs)

let map_cases merge self env cases cases_patch =
  try
    List.fold_left2 (fun accu binding patch_binding ->
        let%bind bind_list, env = accu in
        let%map mapped_binding, new_env =
          map_case merge self env binding patch_binding
        in
        mapped_binding :: bind_list, merge env new_env
      )
      (Ok ([], env))
      cases
      cases_patch
  with Invalid_argument "List.fold_left2" -> Error (cases, env)

let map_expr merge self env ~patch ~expr =
  let e = expr in
  let maybe_desc =
  match e.pexp_desc, patch.pexp_desc with
  | Pexp_ident _, Pexp_ident _
  | Pexp_constant _, Pexp_constant _ -> Error (e.pexp_desc, env)
  | Pexp_tuple e1s, Pexp_tuple e2s ->
    begin
      try
        let%map exprs, env =
          List.fold_left2 (fun accu expr patch_expr ->
              let%bind expr_list, accu_env = accu in
              let%map mapped_expr, new_env =
                self.expr self env ~expr ~patch:patch_expr
              in
              mapped_expr :: expr_list, merge accu_env new_env
            )
            (Ok ([], env))
            e1s
            e2s
        in
        Pexp_tuple exprs, env
      with Invalid_argument "List.fold_left2" -> Error.fail (e.pexp_desc, env)
    end

  | Pexp_construct (identl, exprl), Pexp_construct (identr, exprr)
    when identl.Asttypes.txt = identr.Asttypes.txt ->
    map_maybe_expr merge self env exprl exprr
    >|= (fun (mapped_expr, env_expr) ->
        Pexp_construct (identl, mapped_expr), env_expr
      )

  | Pexp_apply (f1, [lbl1, arg1]), Pexp_apply (f2, [_lbl2, arg2]) ->
    let%bind mapped_f, env_f = self.expr self env ~expr:f1 ~patch:f2 in
    let%map mapped_arg, env_arg = self.expr self env ~expr:arg1 ~patch:arg2 in
    Pexp_apply (mapped_f, [lbl1, mapped_arg]), merge env_f env_arg

  | Pexp_fun (lbl1, default1, pat1, expr1),
    Pexp_fun (_lbl2, _default2, pat2, expr2) ->
    (* TODO: handle labels and default values *)
    let mapped_arg = self.pattern self env ~pat:pat1 ~patch:pat2 in
    let mapped_expr =
      let%bind _, env = mapped_arg in
      self.expr self env ~expr:expr1 ~patch:expr2
    in
    begin
      match mapped_arg, mapped_expr with
      | Ok (pat, env_pat), Ok (expr, env_expr) ->
        Ok (Pexp_fun (lbl1, default1, pat, expr), merge env_pat env_expr)
      | _, _ -> Error (e.pexp_desc, env)
    end
  | Pexp_let (isrecl, bindingsl, exprl), Pexp_let (isrecr, bindingsr, exprr)
    when isrecl = isrecr ->
    let%bind mapped_bindings, env_bindings =
      map_bindings merge self env bindingsl bindingsr
    in
    let%map mapped_expr, env_expr = self.expr self env ~expr:exprl ~patch:exprr
    in
    Pexp_let (isrecl, mapped_bindings, mapped_expr),
    merge env_expr env_bindings

  | Pexp_ifthenelse (ifl, thenl, elsel), Pexp_ifthenelse(ifr, thenr, elser) ->
    let%bind mapped_if, env_if = self.expr self env ~expr:ifl ~patch:ifr in
    let%bind mapped_then, env_then =
      self.expr self env ~expr:thenl ~patch:thenr
    in
    let%map (mapped_else, env_else) = map_maybe_expr merge self env elsel elser
    in
    Pexp_ifthenelse (mapped_if, mapped_then, mapped_else),
    merge env_if (merge env_then env_else)

  | Pexp_function casesl, Pexp_function casesr ->
    let%map mapped_cases, env_cases = map_cases merge self env casesl casesr in
    Pexp_function mapped_cases, env_cases

  | Pexp_match (exprl, casesl), Pexp_match (exprr, casesr) ->
    let%bind mapped_cases, env_cases = map_cases merge self env casesl casesr in
    let%map mapped_expr, env_expr = self.expr self env ~expr:exprl ~patch:exprr in
    Pexp_match (mapped_expr, mapped_cases),
    merge env_cases env_expr

  | Pexp_fun _, _ | _, Pexp_fun _
  | Pexp_function _, _ | _, Pexp_function _
  | Pexp_match _, _ | _, Pexp_match _
  | Pexp_ifthenelse _, _ | _, Pexp_ifthenelse _
  | Pexp_let _, _ | _, Pexp_let _
  | Pexp_apply _, _ | _, Pexp_apply _
  | Pexp_ident _, _ | _, Pexp_ident _
  | Pexp_constant _, _ | _, Pexp_constant _
  | Pexp_construct _, _ | _, Pexp_construct _
    -> Error (e.pexp_desc, env)
  | _ -> raise Failure.(SempatchException (Non_implemented (e.pexp_loc)))
  in Res.map (fun (tree, env) -> { e with pexp_desc = tree; }, env) maybe_desc

let map_maybe_pattern _merge self env ~patch ~pat =
  match pat, patch with
  | None, None -> Ok (None, env)
  | Some pat, Some patch ->
    let%map mapped, env = self.pattern self env ~patch ~pat in
    Some mapped, env
  | _ -> Error (pat, env)

let map_pattern merge self env ~patch ~pat =
  let maybe_desc =
    match pat.ppat_desc, patch.ppat_desc with
    | Ppat_construct (constrl, pat_optl), Ppat_construct (constrr, pat_optr)
      when constrl.Asttypes.txt = constrr.Asttypes.txt ->
      let%map mapped, env =
        map_maybe_pattern merge self env ~pat:pat_optl ~patch:pat_optr
      in
      Ppat_construct (constrl, mapped), env
    | Ppat_var _, _ | _, Ppat_var _
    | Ppat_construct _, _ | _, Ppat_construct _
      -> Error (pat.ppat_desc, env)
    |_, _ -> raise Failure.(SempatchException (Non_implemented pat.ppat_loc))
  in Res.map (fun (tree, env) -> { pat with ppat_desc = tree; }, env) maybe_desc

let mk merge = {
  expr = map_expr merge;
  pattern = map_pattern merge;
}

