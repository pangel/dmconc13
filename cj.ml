type decl =
 | Class of string * string * decl list  (* name, parent, decls *)
 | Method of string * string * string list * expr (* retclass, name, args, body *)
 | Field of string * string (* class, name *)

and expr =
| Skip
| Id of string
| Value of string
| New of string * expr list (* class, args *)
| FRead of expr * string (* object, field *)
| FWrite of expr * string * expr (* object, field, new_value *)
| Cast of string * expr
| MCall of expr * string * expr list (* object, method, args *)
| Seq of expr * expr
| Atomic of expr

(*Store class declarations as we go through the Imp program *)
let decls = ref []
let decl_class d = decls := d::(!decls)

(*Declare new thread*)
let thread_with_name name e =
  decl_class (Class(name,"T",
  [Method("public",name,["Lock l";"List s"],Id("super(l,s)"));
   Method("public void", "run", [], e)])); name

(*Named thread generator *)
let thread_name = 
  let counter = ref 0 in
  fun () -> incr counter; ("T"^(string_of_int !counter))

let thread e = thread_with_name (thread_name ()) e

(*Declare and instantiate new thread *)
let new_thread ?(store=Id("this.store")) ?(lock=Id("this.lock")) e =
  New(thread e,[lock;store])


(* Compile Imp to cJ *)

let rec exp_of_int i = 
  if i<0 
    then failwith "Variable address cannot be negative" 
    else if i=0 
      then Imp.Zero 
      else Imp.Suc(exp_of_int (i-1))

let rec read_store n = MCall(Id("this.store"), "get", [(Id "this.lock");cj_int n])
and expand_store id v = MCall(Id("this.store"),"expand",[(Id "this.lock");id;v])
and write_store key value = MCall(Id("this.store"), "set", [(Id "this.lock");key;value])

and cj_e e = match e with
   Imp.Id(x) -> read_store x
 | Imp.Zero -> New("Zero", [])
 | Imp.Suc(e) -> MCall(cj_e e, "succ", [])
 | Imp.Dec(e) ->  MCall(cj_e e, "pred", [])

and cj_int n = cj_e (exp_of_int n)

let cj_b (Imp.NotZero(e)) = MCall(cj_e e, "notzero", [])

let noop = Id "new T()"
let this = Id("this")
let ite b e1 e2 = MCall(cj_b b, "ite", [e1; e2])
let run e = MCall(e, "run", [])
let start e = MCall(e, "start", [])

let rec cj_c imp_c = match imp_c with
| Imp.Skip -> Skip
| Imp.Assign(n,e) -> write_store (cj_int n) (cj_e e)
| Imp.Seq(c1,c2) -> Seq(cj_c c1, cj_c c2)
| Imp.Ite(b,c1,c2) -> 
    run (Cast ("T", ite b (new_thread (cj_c c1)) (new_thread (cj_c c2))))
| Imp.Var(n,e,c) -> 
    run (new_thread ~store:(expand_store (cj_int n) (cj_e e)) (cj_c c))
| Imp.Atomic c  -> Atomic(cj_c c)
| Imp.While (b,c) -> 
    let thread_loop = new_thread (Seq(cj_c c, run (Cast("T", ite b this noop))))
    in run (Cast("T", ite b thread_loop noop))
| Imp.Spawn c -> start (new_thread (cj_c c))
| Imp.Par _ -> failwith "Par must be reduced to Spawn before translation to cJ"
| Imp.Await _ -> failwith "Await must be reduced to Atomic before translation to cJ"


(* Print cJ to Java *)


let fmtp pad = Printf.ksprintf (fun s -> ((String.make pad ' ')^s))
let map = List.map
let lines = String.concat "\n"
let commas = String.concat ","
let curlies pad s = "{\n"^s^"\n"^(String.make pad ' ')^"}"

let rec prt_expr pad  =
  function
  | Skip -> ""
  | Id str | Value str -> fmtp pad "%s" str
  | New (klass,args) -> 
    fmtp pad "(new %s(%s))" 
      klass 
      (commas (map (prt_expr 0) args))
  | FRead (obj, field) -> fmtp pad "%s.%s" (prt_expr 0 obj) field
  | FWrite (obj,field,value) -> 
      fmtp pad "%s.%s = %s" 
        (prt_expr 0 obj) 
        field 
        (prt_expr 0 value)
  | Cast (klass, expr) -> fmtp pad "((%s) %s)" klass (prt_expr 0 expr)
  | MCall (obj, meth, args) -> 
      fmtp pad "%s.%s(%s)"
        (prt_expr 0 obj)
        meth
        (commas (map (prt_expr 0) args))
  | Seq (e1, e2) ->
      let p1 = prt_expr pad e1
      and p2 = prt_expr pad e2
      in if p1 = "" then p2 else (p1^";\n"^p2)
  | Atomic e ->
     (* Will produce
      * synchronized (this.lock) { this.lock = new Lock(); e } *)
      fmtp pad "synchronized(this.lock) %s"
      (curlies (pad+1)
        ((prt_expr (pad+1)
        (new_thread ~lock:(Id("new Lock()")) e))^";"))

and prt_decl pad = 
  function
| Class (name, parent, decls) ->
  let ext p = if p = "" then "" else ("extends "^p^" ") in
  fmtp pad "class %s %s%s"
    name 
    (ext parent) 
    (curlies pad (lines (map (prt_decl (pad+1)) decls)))
| Method (klass,name,args,body) ->
  fmtp pad "%s %s(%s) %s"
    klass
    name
    (commas args)
    (curlies pad (if body = Skip then "" else ((prt_expr (pad+1) body)^";")))
| Field (klass,name) -> fmtp pad "%s %s" klass name

let prt_decls decls =
  lines (map (prt_decl 0) decls)

let init_store n =
  let pe n = prt_expr 0 (cj_int n) in
  let rec aux n =
    if n < 0 then ""
    else (".expand(new Lock(),"^(pe n)^", "^(pe 0)^")"^(aux (n-1)))
  in ("new Nil(new Lock())"^(aux n))


(* Dirty work *)


let cj_prog p =
  let program_name = "Compiled" in
  print_string ((Imp.prt_c p)^"\n"); (* Show Imp for debugging *)
  let c = Imp.transform p in
  let store = init_store ((Imp.new_var c)-1) in
  let name = thread_with_name (thread_name ()) (cj_c c) in
  let dcls = prt_decls !decls in
  let booter = (New(name, [(Id "new Lock()"); (Id "st")]))
  in let f = open_out (program_name^".java") in
  Printf.fprintf f "%s" (dcls^
  "\n public class "^program_name^" { 
    public static void main (String[] a) {
    List st = "^store^";
    T t = "^(prt_expr 0 booter)^";
    System.out.println(\"Store before:\");
    st.print();
    t.run();
    System.out.println(\"Store after:\");
    t.store.print();
    }}")
  ; close_out f

let () =
  if (Array.length Sys.argv) < 2 then begin
    print_string "Give me an Imp program to compile. e.g. c5. Look in imp.ml\n";
    exit 1 end
  else
    cj_prog (Imp.get Sys.argv.(1))