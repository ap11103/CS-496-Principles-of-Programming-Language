open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser

(**Alisha Patel and Simerjeet Mudhar *)
(**I pledge my honor that I have abided by the Stevens Honor System. *)
       
let rec chk_expr : expr -> texpr tea_result =
  fun e ->
  match e with
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error "LetRec: Type of recursive function does not match
declaration")
   | Pair(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    return @@ PairType(t1,t2)
  | Unpair(id1,id2,e1,e2) ->
    chk_expr e1 >>= fun t ->
    (match t with
     | PairType(t1,t2) ->
    extend_tenv id1 t1 >>+
    extend_tenv id2 t2 >>+
    chk_expr e2
     | _ -> error "unpair: expected a pair")
      
  (* EXPLICIT-REFS *)
  (**return the type *)
  | BeginEnd([]) ->
    return UnitType
  (**based on the interp file, but with chk_expr *)
  | BeginEnd(es) ->
    List.fold_left (fun c e -> c >>= fun _ -> chk_expr e) (return UnitType) es
  (**return a new ref type *)
  | NewRef(e) ->
    chk_expr e >>= fun ref ->
    return @@ (RefType ref)
  (**check if e is a ref type *)
  | DeRef(e) ->
    chk_expr e >>= fun def -> 
    (match def with 
    | RefType(n)-> return n
    | _ -> error "Expected a RefType")
  (**if both are ref type; return unit type *)
  | SetRef(e1,e2) ->
    chk_expr e1 >>= fun ref1 ->
    chk_expr e2 >>= fun ref2 ->
    (match (ref1,ref2) with
    | (RefType(n), es) -> 
      if n = es then return UnitType
      else error "Expected a reference type"
    | (_, _) -> error "Expected a reference type")

  (* list *)
  | EmptyList(None) ->
    error "No type for no arguments"

  | EmptyList(Some t) ->
    return @@ ListType(t)
  (**check if the h and t is a ListType; and return*)
  | Cons(h, t) ->
    chk_expr h >>= fun e1 -> 
    chk_expr t >>= fun e2 ->
    (match (e1,e2) with 
    | (hd, ListType(es)) ->
      if hd = es then return @@ ListType(es)
      else error "cons: type of head and tail do not match"
    | (_,_) -> error "Expected a ListType")
  (**check if it's either ListType and TreeType *)
  | IsEmpty(e) ->
    chk_expr e >>= fun lst ->
    (match lst with
    | ListType _ -> return @@ (BoolType)
    | TreeType _ -> return @@ (BoolType)
    | _ -> error "Expected a ListType or TreeType")
  (**check if e is a ListType *)
  | Hd(e) ->
    chk_expr e >>= fun e1 ->
    (match (e1) with
    | ListType(e1) -> return @@ (e1)
    | _ -> error "Expected a ListType")

  (**check if e is a ListType *)
  | Tl(e) ->
    chk_expr e >>= fun e1 ->
    (match (e1) with
    | ListType(e1) -> return @@ ListType(e1)
    | _ -> error "Expected a ListType")

  (* tree *)
  | EmptyTree(None) ->
    error "TreeType t is expected."

  | EmptyTree(Some t) ->
    return @@ TreeType(t)
  (**check if the first one is IntType, TreeType, TreeType*)
  | Node(de, le, re) ->
    chk_expr de >>= fun e1 ->
    chk_expr le >>= fun e2 ->
    chk_expr re >>= fun e3 ->
    (match (e1,e2,e3) with
    | (IntType , TreeType(t), TreeType(_)) -> return @@ TreeType(t)
    | _ -> error "Expected Node(IntType, TreeType, TreeType)")

  (**based on the interp file; extend the env; and check the expression type are the same *)
  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->
    chk_expr emptycase >>= fun e1 ->
    chk_expr target >>= fun e2 ->
    (match e2 with
    | TreeType(x) -> 
      extend_tenv id1 x >>+
      extend_tenv id2 e2 >>+
      extend_tenv id3 e2 >>+
      chk_expr nodecase >>= fun e3 ->
      if (e1=e3)
      then return @@ e3
      else error "Types are not the same"
    | _ -> error "Expected a Treetype")


    
  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> failwith "chk_expr: implement"    
and
  chk_exprs =
  fun es ->
  match es with
  | [] -> return []
  | h::tl -> chk_expr h >>= fun t ->
    chk_exprs tl >>= fun ts ->
    return (t::ts)
and
  chk_prog (AProg(_,e)) =
  chk_expr e

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)



