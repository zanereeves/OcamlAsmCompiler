{
    open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+



rule read = 
    parse 
        | white { read lexbuf }
        | "true" { TRUE }
        | "false" { FALSE }
        | "<=" { LEQ }
        | ">=" { GEQ }
        | "<" {LESST}
        | ">" {GREATT}
        | "*" { TIMES }
        | "!" {NOT}
        | "-" {MINUS}
        | "/" {SLASH}
        | "!=" {NEQ}
        | "+" { PLUS }
        | "(" { LPAREN }
        | ")" { RPAREN }
        | "{" {LBRACE}
        | "}" {RBRACE}
        | "let" { LET }
        | "==" { DEQUALS }
        | "=" { EQUALS }
        | "if" { IF }
        | "then" { THEN }
        | "else" { ELSE }
        | "func" { FUNC }
        | "return" { RETURN }
        | "while" { WHILE }
        | "assert" {ASSERT}
        | "main" {MAIN}
        | "," { COMMA }
        | ";" {SEMICOLON}
        | id { ID (Lexing.lexeme lexbuf)}
        | int { INT (int_of_string (Lexing.lexeme lexbuf))}
        | eof {EOF}
        