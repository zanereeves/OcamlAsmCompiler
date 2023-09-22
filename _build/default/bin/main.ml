open Abstr.Ast


let parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in ast;;

let emit s =
  print_string (s^"\n");;

let is_value = function
  | Int _ | Bool _ -> true
  | _ -> false;;

let rec e_emit = function
  | Int i -> emit ("    ldr r0, =#"^string_of_int i)
  | Bool a -> (match a with 
    | true -> emit ("    ldr r0, =#"^string_of_int 1)
    | false -> emit ("    ldr r0, =#"^string_of_int 0))
  | Binop (bop, a,b) -> (match bop, a, b with 
    | (Add, a, b) ->
      e_emit a;
      emit "    push {r0, ip}";
      e_emit b;
      emit "    pop {r1, ip}";
      emit "    add r0, r0, r1"
    | (Minus, a, b) ->
      e_emit a;
      emit "    push {r0, ip}";
      e_emit b;
      emit "    pop {r1, ip}";
      emit "    sub r0, r0, r1"
    | (Mult, a, b) ->
      e_emit a;
      emit "    push {r0, ip}";
      e_emit b;
      emit "    pop {r1, ip}";
      emit "    mul r0, r0, r1"
    | (Div, a, b) ->
      e_emit a;
      emit "    push {r0, ip}";
      e_emit b;
      emit "    pop {r1, ip}";
      emit "    udiv r0, r0, r1"
    | (Eq, a, b) ->
      e_emit a;
      emit "    push {r0, ip}";
      e_emit b;
      emit "    pop {r1, ip}";
      emit "    cmp r0, r1";
      emit "    moveq r0, #0";
      emit "    movne r0, #1"
    | (Neq, a, b) ->
        e_emit a;
        emit "    push {r0, ip}";
        e_emit b;
        emit "    pop {r1, ip}";
        emit "    cmp r0, r1";
        emit "    moveq r0, #0";
        emit "    movne r0, #1"
    | (_, _, _) -> emit "TODO"
    )
  | Uop (op, a) -> (match op, a with
    | (Not, e) -> 
      e_emit e;
      emit "    cmp r0, #0";
      emit "    moveq r0, #0";
      emit "    movne r0, #1"
    | (_, _) -> emit "TODO")
  | _ -> emit "todo";;

let rec emitter state = match state with
  | Block statement -> List.iter emitter statement
  | Main b ->
    emit ".global main";
    emit ".main:";
    emit "    push {fp, lr}:";
    emitter b;
    emit "    mov r0, #0";
    emit "    pop {fp, pc}"
  | ExprS e -> e_emit e
  | Assert e ->
    e_emit e;
    emit "    cmp r0, #1";
    emit "    moveq r0, #'.'";
    emit "    movne r0, #'F'";
    emit "    bl putchar"
  | _ -> emit "todo";;

  

let unravel_prog p = 
  match p with
  | Prog l -> List.iter emitter l;;

unravel_prog (parse "func main() {assert(1); assert(!0); {assert(1); assert(1);}}")