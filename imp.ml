(* From concurrency course *)
(* Abstract syntax of ParImp *)

(* As expressions we just have natural numbers in unary notation *)

type exp = 
| Id of int        (* we code a variable as an integer *) 
| Zero             
| Suc of exp
| Dec of exp ;;    

(* A boolean expression tests whether an expression is not zero *)

type bexp = NotZero of exp ;;

(* The syntax of commands is enriched with Spawn and Atomic which are used
   to simulate parallel composition and the await synchronisation, respectively *)

type com = 
| Skip
| Assign of int * exp
| Seq of com * com
| Ite of  bexp * com * com
| While of bexp * com 
| Var of int * exp * com 
| Par of com * com
| Spawn of com               
| Await of bexp * com 
| Atomic of com ;;     

(* Functions to compute the largest variable occurring in a program *)

let max n m =  if n<m then m else n ;;

let rec max_exp e = match e with
Id(n) -> n
| Zero -> 0
| Suc(e1) -> max_exp e1
| Dec(e1) -> max_exp e1 ;;

let max_bexp b = match b with NotZero(e) -> max_exp e ;;

let rec max_com c = match c with
| Skip         -> 0
| Assign(x,e)  -> max (max_exp (Id(x))) (max_exp e)
| Seq(c1,c2)   -> max (max_com c1) (max_com c2)
| Ite(b,c1,c2) -> max (max_bexp b) (max (max_com c1) (max_com c2))
| While(b,c1)  -> max (max_bexp b) (max_com c1)
| Var(x,e,c1)  -> max x (max (max_exp e) (max_com(c1)))
| Par(c1,c2)   -> max (max_com c1) (max_com c2)
| Spawn(c1)    -> (max_com c1)
| Await(b,c1)  -> max (max_bexp b) (max_com c1)
| Atomic(c1)   -> (max_com c1) ;;

(* new_var returns a fresh variable *)

let new_var c = let n = (max_com c) in (n+1);;

(* We reduce Par to Spawn and Await to Atomic *)

let rec t c count = match c with
  Skip -> Skip
| Assign(x,e)  -> Assign(x,e) 
| Seq(c1, c2)  -> Seq ((t c1 count), (t c2 count))
| Ite(b,c1,c2) -> Ite(b, (t c1 count), (t c2 count))
| While(b,c)   -> While(b,(t c count))
| Var(x,e,c)   -> Var(x,e, (t c count))
| Atomic(c)    -> Atomic(t c count)
| Spawn(c)     -> Spawn(t c count) 
| Par(c1,c2)   -> let t1 = (t c1 count) in 
                  let t2 = (t c2 count) in 
                  let x = !(count) in 
                  count:= !(count)+1;
                  Var(x, (Suc Zero), 
                  Seq( Spawn(Seq(t1, 
                       Assign(x, Zero))),
                  Seq( t2, 
                       While(NotZero(Id(x)),Skip))))
| Await(b,c)   -> let t1= (t c count) in 
                  let x = (! count) in 
                  count:= (! count)+1;
                  Var(x, (Suc Zero) , 
                  While(NotZero(Id(x)), 
                        Atomic(Ite(b,
                                   Seq( t1,  Assign(x,Zero)),
                                   Skip)))) ;;

let transform c = let count = ref (new_var c) in (t c count);;


(* We represent a store as a list with operations to 
read a value, update (write), extend, and restrict. 

Important: it is assumed that in a  program all bound variables 
are distinct and not intersecting the free ones *)

type mem = (int*exp) list;;

let rec value m x = match m with 
[]  -> failwith "read memory error"
| (y,v)::m1 -> if x=y then v else (value m1 x) ;;

let rec update m x v  = match m with
[] -> failwith "write memory error" 
| (y,v1)::m1 -> if x=y then (x,v)::m1
                       else (y,v1):: (update m1 x v);;

let extend m x v  = (x,v)::m ;;

let rec restrict m x  = match m with
[] -> failwith "restriction memory error"
| (y,v1)::m1 -> if x=y then m1 else (y,v1):: (restrict m1 x) ;;

(* Evaluator for (boolean) expressions *)

let rec eval_exp e m =
match e with
  Id(x) -> (value m x) 
| Zero -> Zero
| Suc(e) -> Suc (eval_exp e m)
| Dec(e) -> match (eval_exp e m) with 
            Zero -> Zero 
           | Suc(v) -> v 
           | _ -> failwith "eval_exp failed to return a value" ;;

let eval_bool (NotZero(e)) m  = let v = (eval_exp  e m) in 
match v with Zero -> false | _ -> true ;;

(* Evaluator for atomic execution of commands with no while/await/parallel *)

let rec  eval_com c m = 
match c with
  Skip -> m
| Assign(x,e) ->  let v=(eval_exp e m) in (update m x v)
| Seq(c1,c2) ->   let m'= (eval_com c1 m) in (eval_com c2 m')
| Ite(b,c1,c2) -> let x=(eval_bool b m) in 
                  ( if x then (eval_com c1 m) else (eval_com c2 m))
| Var(x,e,c) -> let v=(eval_exp e m) in (restrict (eval_com c (extend m x v)) x)
| _  ->           failwith "illegal atomic command";;

(* Continuations to handle sequential composition and variable declaration *)
(* Continuations to handle sequential composition and variable declaration *)

type cont = 
  Halt
| Cons of com*cont 
| Restrict of int * cont;;

type program = Prog of (com * cont) list  ;;

let step c k m = match c with
  Skip -> (match k with 
           Cons(c1,k1) -> ([(c1,k1)],m) 
          | Restrict(x,k1) -> ([(Skip,k1)],(restrict m x))
          | Halt -> ([],m) ) 
| Assign(x,e) -> let v=(eval_exp e m) in ([(Skip, k)], (update m x v))
| Seq(c1,c2) -> ([(c1, Cons(c2,k))],m) 
| Ite(b,c1,c2) -> let x=(eval_bool b m) in 
                (if x then ([(c1,k)],m) else ([(c2,k)],m))
| While(b,c1) -> let x=(eval_bool b m) in 
                if x then ([(c1,Cons(While(b,c1),k))],m)
                     else ([(Skip,k)],m) 
| Var(x,e,c)   -> let v= (eval_exp e m) 
                  in  ([(c, Restrict(x,k))], (extend m x v))
| Spawn(c)     -> ([(c,Halt); (Skip,k)], m) 
| Atomic(c)    -> ([(Skip,k)], (eval_com c m))
| _ -> failwith "illegal command step" ;;

let rec append cl1 cl2 = match cl1 with
[] -> cl2
|  y::cl3 -> y:: (append cl3 cl2);;

(* This scheduler is round-robin but you should not count on it *)

let rec run (Prog(cl)) m  = match cl with 
[] -> m
| (c1,k1)::cl1 -> let (cl2,m2) = (step c1 k1 m) in 
                  (run (Prog(append cl1 cl2)) m2) ;;

(* SOME TESTS *)

(* A.H.: Store stuff so cj.ml can read it *)
let programs = Hashtbl.create 100;;
let add s p = Hashtbl.add programs s p;;
let get s = Hashtbl.find programs s;;

(* x0:=1 *)
let m = [(0,Zero)];;   
add "c0" (Assign(0, Suc(Zero))) ;;

Hashtbl.iter (fun k p -> let p = (Prog[(transform p, Halt)]) in ignore(run p m)) programs;;

(* x0:=x0+1 *)
add "c1" (Assign(0, Suc (Id(0)))) ;;     

(* x0:=x0-1+1 *)
add "c2" (Assign(0,Suc(Dec(Id(0))))) ;;  

(* (x0:=x0+1; x0:=x0-1+1);x0:=x0-1+1 *)
add "c3" (Seq(Seq((get "c1"),(get "c2")),(get "c2")));;           

(* x0:=x0+1;(x0:=x0-1+1;x0:=x0-1+1) *)
add "c4" (Seq((get "c1"),Seq((get "c2"),(get "c2"))));;           

(* c1; (skip;(c2;c3)) *)
add "c5" (Seq((get "c1"),Seq(Skip, Seq((get "c2"),(get "c3")))));;

(* Boolean conditions and if then else *)
add "c6" (Ite(NotZero(Id(0)), Skip, (get "c1")));;
add "c7" (Ite(NotZero(Id(0)), (get "c1"), (get "c1")));;

(* Local variables *)
add "c8" (Assign(1, Suc(Id(1))));;
add "c9" (Var(1,Suc(Zero), Seq((get "c8"), Assign(0,Id(1)))));;

(* While condition *)
add "c10" (Var(1,Suc(Zero), While(NotZero(Id(1)), Assign(1,Dec(Id(1))))));;
add "c11" (Var(1,Suc(Zero),While(NotZero(Id(1)), Skip)));; 
(* run p m; This loops *)

(* Parallel and sequential *)
add "c12" (Par((get "c1"),Par((get "c1"),(get "c1"))));;
add "c13" (Par (Par((get "c1"),(get "c1")), (get "c1")));;
add "c14" (Seq((get "c2"),Par((get "c1"),(get "c1"))));;
add "c15" (Seq(Par((get "c1"),(get "c2")),(get "c1")));;

(* Await *)
add "c16" (Par((get "c0"), Await(NotZero(Id(0)), Seq((get "c1"),(get "c1")))));;
add "c17" (Par(Skip, Await(NotZero(Id(0)), Skip)));; 
(* run p m;;  This loops *)

(* A.H: utility: letter for var. index *)
let letter n = Char.escaped ("abcdefhijklmnopqrstuvwxyz".[n])

let rec prt_b = fun (NotZero e) -> (prt_e e)^" != 0"

and prt_e e =
  let rec count k = function
    | Id n -> (letter n)^k
    | Zero -> k
    | Suc n -> count ("+1"^k) n
    | Dec n -> count ("-1"^k) n
  in count "" e

and prt_c = 
  let prt = Printf.sprintf in function
  | Skip -> "skip"
  | Assign(x,v) -> prt "%s := %s" (letter x) (prt_e v)
  | Seq (c1,c2) -> prt "%s ; %s" (prt_c c1) (prt_c c2)
  | Ite (b,c1,c2) -> prt "(%s) ? %s : %s" (prt_b b) (prt_c c1) (prt_c c2)
  | While (b,c) -> prt "while %s do { %s }" (prt_b b) (prt_c c)
  | Var (n,e,c) -> prt "var %s=%s in { %s }" (letter n) (prt_e e) (prt_c c)
  | Par (c1,c2) -> prt "(%s  |  %s)" (prt_c c1) (prt_c c2)
  | Spawn c -> prt "spawn { %s }" (prt_c c)
  | Await (b,c) -> prt "await %s do { %s }" (prt_b b) (prt_c c)
  | Atomic c -> prt "atomic { %s }" (prt_c c)

