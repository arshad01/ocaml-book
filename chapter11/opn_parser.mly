%{

type ops = Add | Sub | Mul | Div;;

let doop op l = match op with
   | Add -> List.fold_right ( + ) l 0
   | Sub -> List.fold_left  ( - ) (List.hd l) (0::(List.tl l))
   | Mul -> List.fold_right ( * ) l 1
   | Div -> List.fold_left  ( / ) (List.hd l) (1::(List.tl l));;

let parse_error s =
    print_endline s;
    flush stdout;;

%}

%token <int> Lint
%token Ladd Lsub Lmul Ldiv
%token Lminus Lplus
%token Lpar Rpar
%token Leol

%start main
%type <int> main

%%
main:
    exp Leol            { $1 }    
 ;

exp:
    Ladd opr        { doop Add $2 }
  | Lsub opr        { doop Sub $2 }
  | Lmul opr        { doop Mul $2 }
  | Ldiv opr        { doop Div $2 }
  | Lminus exp      { -$2 }
  | Lplus exp       { $2 }
  | Lpar exp Rpar   { $2 }
 ;

opr:
  | Lint            { [$1] }
  | Lminus Lint     { [(-$2)] }
  | Lplus Lint      { [$2] }
  | Lint opr        { [$1] @ $2 }
  | exp opr         { [$1] @ $2 }
  | exp             { [$1] }
  
 ;

%%





