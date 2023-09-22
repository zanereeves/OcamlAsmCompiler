
(* The type of tokens. *)

type token = 
  | WHILE
  | UMINUS
  | TRUE
  | TIMES
  | THEN
  | SLASH
  | SEMICOLON
  | RPAREN
  | RETURN
  | RBRACE
  | R
  | PLUS
  | NOT
  | NEQ
  | MINUS
  | MAIN
  | LPAREN
  | LET
  | LESST
  | LEQ
  | LBRACE
  | INT of (int)
  | IF
  | ID of (string)
  | GREATT
  | GEQ
  | FUNC
  | FALSE
  | EQUALS
  | EOF
  | ELSE
  | DEQUALS
  | COMMA
  | COLON
  | ASSERT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Abstr.Ast.program)
