open Parsetree
open Std_utils

let test_progs = [
  "f x y";
  "x";
]

let in_file = open_in "test/sempatch.md"

let patches = Patch_parser.sempatch (Patch_lexer.read) (Lexing.from_channel in_file)

let results = [
  [ true ; true ; true ; false; true ];
  [ false; true ; true ; false; false]
]

let test_asts = List.map (fun s -> Parser.parse_expression Lexer.token (Lexing.from_string s)) test_progs

let apply ast patch =
  let patch = Ast_pattern_matcher.preprocess patch in
  match Ast_pattern_matcher.match_expression Parsed_patches.(patch.header.expr_variables) (Ast_pattern_matcher.preprocess_src_expr ast) (Ast_traverser2.Expr Parsed_patches.(patch.body.before)) with
  | None -> false
  | Some x -> true

let () =
  List.iter2
    (
      fun ast result ->
        List.map (apply ast) patches
        |> List.map2 (=) result
        |> List.iteri (Printf.printf "test %d : %B\n")
    )
    test_asts
    results;
  close_in in_file
