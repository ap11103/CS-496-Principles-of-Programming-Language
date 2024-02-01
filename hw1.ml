(** Name: Alisha Patel
Pledge: I pledge my honor that I have abided by the Stevens Honor System. *)


(**predefined variables/list/type *)
type program = int list

let square : program = [0; 2; 2; 3; 4; 4; 5; 1]

let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

(**helper function for mirror; matches the element for this element *)
let mirror_image_helper : int  -> int  = 
    fun l ->
    match l with
    | 0 -> 0
    | 2 -> 4
    | 3 -> 5
    | 5 -> 3
    | 4 -> 2
    | 1 -> 1
    | _ -> failwith "invalid input"

(**maps it to the entire list *)
let mirror_image : int list -> int list = 
    fun l -> 
    match l with 
    | [] -> []
    | h::t -> List.map mirror_image_helper l


(**helper function of rotate; matches the appropriate numbers *)
let rotate_90_helper : int -> int = 
    fun l ->
    match l with
    | 0 -> 0
    | 2 -> 3
    | 3 -> 4
    | 5 -> 2
    | 4 -> 5
    | 1 -> 1
    | _ -> failwith "invalid input"

(**maps the elements of list *)
let rotate_90_letter : int list -> int list =
    fun l -> 
    match l with 
    | [] -> []
    | h::t -> List.map rotate_90_helper l

(**uses the output of rotate_90_letter to map the list *)
let rotate_90_word : int list list -> int list list =
    fun l -> 
    match l with 
    | [] -> []
    | h::t -> List.map rotate_90_letter l

(**recursively repeats the 'a n  times *)
let rec repeat : int -> 'a -> 'a list = 
    fun n x -> 
    match n with 
    | 0 -> []
    | 1 -> [x]
    | _ -> x :: repeat (n-1) x


(**pantograph helper function  *)

(** this is repeat specialized for pantogrpah; if 0 or 1 repeat e times *)
let panto_repeat e n =
    match n with 
    | 0 -> [0]
    | 1 -> [1]
    | _ -> repeat e n

(**concatenating list p and and nesting with map *)
let pantograph : int -> int list -> int list =
    fun n p ->
    match p with   
    | [] -> []
    | h::t -> List.concat(List.map (panto_repeat n) p)

(**recursively performs pantogrpah through repeat/concatenation *)
let rec pantograph_nm : int -> int list -> int list =
    fun n p -> 
    match p with
    | [] -> []
    | h::t -> 
        if h = 0 || h = 1
        then h :: pantograph_nm n t
        else
            (repeat n h) @ pantograph_nm n t

(**folds list *)
let pantograph_f : int -> int list -> int list =
    fun n p -> 
    match p with
    | [] -> []
    | h::t -> List.fold_left(fun x y -> x @ y) [] (List.map (panto_repeat n) p)



(**matches output with specific combination of a and b *)
let coverage_helper (a,b) c =
    match c with
    | 0 -> (a,b)
    | 1 -> (a,b)
    | 2 -> (a, b+1)
    | 3 -> (a+1, b)
    | 4 -> (a, b-1)
    | 5 -> (a-1, b)
    | _ -> failwith "invalid input"

(**maps out the points *)
let rec coverage_points (a,b) c = 
    match c with
    | [] -> []
    | h::t -> coverage_helper (a, b) h :: coverage_points(coverage_helper (a, b) h)t
(** appends the points from the list *)
let coverage : int * int -> int list -> (int*int) list = 
    fun (a, b) l -> 
    match l with 
    | [] -> []
    | h::t -> [(a,b)] @ coverage_points(a, b) l

(** creates tuple based on the amount of time x *)
let rec compress_helper item count l = 
    match l with
    | [] -> [(count, item)]
    | h::t -> 
        if h = count
        then compress_helper (item+1) count t
        else (count, item) :: compress_helper 1 h t

(** calls on the helper function *)
let compress : int list -> (int*int) list = 
    fun l ->
    match l with 
    | [] -> []
    | h::t -> compress_helper 0 h l

(**recursively calls uncompress and repeats head based on input *)
let rec uncompress : (int*int) list -> int list =
    fun l ->
    match l with 
    | [] -> []
    | (a,b) :: t -> (repeat b a) @ uncompress t

(**repeat function for uncompress *)
let uncompress_helper =
    fun (a, b) -> repeat b a

(**nested loop to concatenate and apply map *)
let uncompress_m l =
    match l with 
    | [] -> []
    | h::t -> List.concat (List.map uncompress_helper l)

let uncompress_f l =
    match l with 
    | [] -> []
    | h::t -> List.fold_right(fun (a, b) t -> (repeat b a) @ t ) l []

(**recursively gets the head of the tail and examines the penstate *)
let rec optimize_helper l penstate =
    match l with 
    | [] -> []
    | h :: t -> 
        if h = penstate
        then [] @ optimize_helper t penstate
        else [h] @ optimize_helper t h
        
(** calls the helper function *)
let optimize : int list -> int list= 
    fun l -> 
    match l with 
    | [] -> []
    | h::t -> optimize_helper l 1









