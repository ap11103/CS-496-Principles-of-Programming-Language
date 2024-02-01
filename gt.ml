(* 
   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place your name here:
    Name: Alisha Patel
    Pledge: I pledge my honor that I have abided by the STevens Honor System. 
*)


type 'a gt = Node of 'a*('a gt) list
let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

(**
let t2 : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, []);
                     Node (15, []);
                     Node (16, []);
                     Node (17, []);
                     Node (346, [])]); 
               Node (48, []); 
               Node (103, []); 
               Node (55, [])])
       ])
*)


(**recursively finds the length of the longest (max) path from root to leaf *)
let rec height : 'a gt -> int =
  fun t ->
  match t with
  | Node (x, []) -> 1
  | Node (x, y) -> List.fold_right(fun a b -> max a b) (List.map height y) 0 + 1


(**recusively goes through the nodes of the trees and increments *)
let rec size : 'a gt -> int =
  fun t ->
  match t with
  | Node (x, []) -> 1
  | Node (x, y) -> List.fold_right (fun a b -> a + b) (List.map size y) 0 + 1


(**Helper function for index *)
let ptl_helper index l = 
  match l with
  | [] -> []
  | h::t -> List.map (fun x -> index :: x) l


(**recusively goes through the tree and returns the paths to each leaf *)
(** use list.flatten to return list of lists *)
let rec paths_to_leaves : 'a gt -> int list list =
  fun t -> 
  match t with
  | Node (x, []) -> [[]]
  | Node (x, y) -> List.flatten (List.mapi ptl_helper (List.map paths_to_leaves y))

(**helper function to check the subtrees if they are perfect *)
let rec ilp_helper l =
  match l with
  | [] -> true
  | [x] -> true
  | h1::h2::t -> 
    if (List.length h1) == (List.length h2)
    then (ilp_helper (h2::t))
    else false

(**calls the helper function and path to leaves to to check if it is perfect *)
let rec is_leaf_perfect : 'a gt -> bool =
  fun t ->
  match t with
  | Node (x, []) -> true
  | Node (x, y) -> ilp_helper (paths_to_leaves t)


(**recusively(map) goes through the tree and appends the nodes in preorder manner *)
let rec preorder : 'a gt -> int list =
  fun (Node(d,ch)) ->
  match ch with
  | [] -> [d]
  | h::t -> [d] @ List.fold_right(fun a b -> a @ b) (List.map preorder ch) []


(**returns the tree by iterating on the reverse list of children  *)                       
let rec mirror : 'a gt -> 'a gt =
  fun (Node(d,ch)) ->
  match ch with
  | [] -> Node(d, [])
  | h::t -> Node(d, List.map mirror (List.rev ch))


(**creates aa map function by applying the f on d *)  
let rec map : ('a -> 'b) -> 'a gt -> 'b gt =
  fun f (Node(d, ch)) ->
  match ch with
  | [] -> Node(f d, [])
  | h::t -> Node(f d, List.map (map f) ch)


(**creates a fold function using list.map  *) 
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
  match ch with
  | [] -> f d []
  | h::t -> f d (List.map (fold f) ch)

  
let sum t =
  fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t



let mem t e = 
  fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t

(**functions  the same as the mirror, using fold *)
let mirror' : 'a -> 'a  = 
  fun t ->
  match t with
  | Node (x, []) -> Node(x, [])
  | Node (x, y) -> fold(fun i rs -> Node(i, List.rev rs)) t


(**recursively returns the max number of children the node has by comapring the length and fold *)
let rec degree : 'a -> int =
  fun t ->
  match t with
  | Node (x, []) -> 0
  | Node (x, y) -> max(List.length y)(List.fold_right (fun a b -> max a b) (List.map degree y) 0)