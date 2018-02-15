
exception Error of Range.t

module I = Parser.MenhirInterpreter


let k_success utast =
  utast


let k_fail chkpt =
  (* print_endline "k_fail";  (* for debug *) *)
  match chkpt with
  | I.HandlingError(penv) ->
      let (lposS, lposE) = I.positions penv in
      let file_name = lposS.Lexing.pos_fname in
      let cnumS = lposS.Lexing.pos_cnum - lposS.Lexing.pos_bol in
      let cnumE = lposE.Lexing.pos_cnum - lposE.Lexing.pos_bol in
      let rng = Range.make file_name lposS.Lexing.pos_lnum cnumS cnumE in
      raise (Error(rng))

  | _ -> assert false


let process lexbuf =
  (* print_endline "parserInterface.process";  (* for debug *) *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.cut_token lexbuf in
    I.loop_handle k_success k_fail supplier (Parser.Incremental.main lexbuf.Lexing.lex_curr_p)
