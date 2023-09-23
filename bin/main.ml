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
      emit "    moveq r0, #1";
      emit "    movne r0, #0"
    | (_, _) -> emit "TODO")
  |  Call (s, el) ->
      let lngth = List.length el in 
      if lngth = 0
        then (emit ("   bl "^s))
      else if lngth = 1
        then (e_emit (List.nth el 0); emit ("   bl "^s))
      else if lngth >= 2 && lngth <= 4
        then (emit "   sub sp, sp, #16";
              let rec do_all lst i s = 
                match lst with
                | [] -> emit "    pop {r0, r1, r2, r3}"; emit ("    bl "^s)
                | x :: xs -> e_emit x; emit ("    str r0, [sp, #"^string_of_int(4*i)^"]"); do_all xs (i+1) s; in
              do_all el 0 s)
      else failwith "Invalid number of function arguments"
  | _ -> emit "todo";;

let rec emitter state = match state with
  | Block statement -> List.iter emitter statement
  | Main b ->
    emit ".global main";
    emit "  main:";
    emit "    push {fp, lr}";
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

unravel_prog (parse "func main() {putchar(46);}")