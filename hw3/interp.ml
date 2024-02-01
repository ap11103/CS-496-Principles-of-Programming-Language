open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds

(** Simerjeet Mudhar and Alisha Patel*)
(** I pledge my honor that I have abided by the Stevens Honor System. *)


let rec eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Proc(id,_,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  -> 
    eval_expr e1 >>= 
    clos_of_procVal >>= fun (id,e,en) ->
    eval_expr e2 >>= fun ev ->
    return en >>+
    extend_env id ev >>+
    eval_expr e

  | Abs(e1)      ->
    eval_expr e1  >>=
    int_of_numVal >>= fun n ->
    return @@ NumVal (abs n)

 

(* Adds an second element to the first list *)
  | Cons(e1, e2) ->
    eval_expr e1 >>= fun l1 ->
    eval_expr e2 >>= fun l2 ->
    (match l2 with
    | ListVal l -> (return @@ ListVal(l1::l))
    | _ -> error "Second argument should be a list")
(* If the list is empty, outputs an error, or else returns the head of the list *)
  | Hd(e1) ->
    eval_expr e1 >>= list_of_listVal >>= fun l ->
    if l = [] 
    then error "The list can't be empty"
    else return @@ (List.hd l)
(* If the list is empty, outputs an error, or else returns the tail (in ListVal) of the list *)
  | Tl(e1) ->
    eval_expr e1 >>= list_of_listVal >>= fun l ->
    if l = []  
    then error "The list can't be empty"
    else return @@ ListVal (List.tl l)
(* Checks if the given list is empty; or else outputs an error through list_of_listVal *)
  | IsEmpty (e1)  ->
    eval_expr e1 >>= list_of_listVal >>= fun l ->
    if l = []
    then return @@ (BoolVal true)
    else return @@ (BoolVal false)
(* Returns an empty list *)
  | EmptyList    ->
    return @@ ListVal ([])

(* Creates a tuple from the given elements *)
  | Tuple(es) ->
    sequence(List.map eval_expr es) >>= fun t ->
    return @@ (TupleVal t)

(* Creates a list from the given tuple based on the env *) 
  | Untuple(ids,e1,e2) ->
    eval_expr e1 >>=
    tuple_of_tupleVal >>= fun tuple ->
    extend_env_list ids tuple >>+
    eval_expr e2

  
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | _ -> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c

