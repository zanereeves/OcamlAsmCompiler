%{
    open Abstr.Ast
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token LESST
%token GREATT
%token TIMES
%token PLUS
%token LPAREN
%token WHILE
%token RPAREN
%token RETURN
%token LET
%token EQUALS
%token IF
%token THEN
%token GEQ
%token ELSE
%token FUNC
%token ASSERT
%token COMMA
%token SEMICOLON
%token COLON
%token LBRACE
%token RBRACE

%token UMINUS
%token NOT
%token NEQ
%token MINUS
%token SLASH
%token DEQUALS
%token MAIN
%token R

%token EOF

%nonassoc ELSE
%right EQUALS
%left GREATT DEQUALS NEQ
%left PLUS MINUS LESST
%left TIMES SLASH
%nonassoc UMINUS NOT


%start <program> prog
%%


prog:
    |  s = list(statement); EOF { Prog (s)}
    ;

function_defn:
    | FUNC; MAIN; LPAREN; RPAREN; block = block_statement {Main (block)}
    | FUNC; name = ID; param_l = params; block = block_statement {FuncA (name, param_l, block)}
    ;

args:
    | LPAREN; args=separated_list(COMMA, expr); RPAREN {args}
    ;

params:
    | LPAREN; params=separated_list(COMMA, param); RPAREN {params}
    ;

param:
    | name = ID; { TParam name }
    ;


block_statement:
    | LBRACE; exprs=list(statement); RBRACE { Block (exprs) }
    ;


statement:
    | IF; LPAREN; e1 = expr; RPAREN; stat = block_statement; ELSE; stat2 = block_statement {If (e1, stat, stat2)}
    | LET; x = ID; EQUALS; e1 = expr; SEMICOLON { Let (x, ExprS (e1)) }
    | RETURN; e1 = expr; SEMICOLON { ReturnS (e1) }
    | WHILE; LPAREN; e1 = expr; RPAREN; cap = block_statement { WhileS (e1, cap) }
    | ASSERT; LPAREN; e = expr; RPAREN; SEMICOLON { Assert (e) }
    | e1 = expr; SEMICOLON  { ExprS(e1) }
    | cap = block_statement { cap }
    | cap = function_defn {cap}
    ;


expr:
    | i = INT { Int i }
    | x = ID { Var x }
    | x = ID; f_args= args {Call(x, f_args)}
    | TRUE {Bool true}
    | FALSE {Bool false}
    | e1 = expr; op=bin_op; e2 = expr { Binop (op, e1, e2)}
    | LPAREN; e=expr; RPAREN {e}
    | MINUS; e = expr %prec UMINUS
        {Uop(Neg, e) }
    | NOT; e = expr { Uop(Not, e)}
    ;

%inline bin_op:
    | LESST { LessT }
    | GREATT {GreatT}
    | DEQUALS { Eq }
    | NEQ { Neq }
    | TIMES { Mult }
    | PLUS { Add }
    | SLASH {Div}
    | MINUS {Minus}
