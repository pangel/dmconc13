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
let new_thread ?(store=Id("this.store")) ?(lock=Id("this.lock")) e = New(thread e,[lock;store])

(* Compile Imp to cJ *)

let rec read_store n = MCall ((Id "this.store"), "get", [cj_int n])
and expand_store id v = MCall ((Id "this.store"),"expand",[id;v])
and write_store key value = MCall ((Id "this.store"), "set", [key;value])
(* Default store sets all variables to Zero *)
and default_store c = 
  let rec build_store = function
  | n when n<0 -> []
  | n -> (n,Imp.Zero)::(build_store (n-1))
  in build_store ((Imp.new_var c)-1)
and patch_store s = function
  | (k,v)::patch -> patch_store (Imp.update s k v) patch
  | [] -> s

and cj_e e = match e with
   Imp.Id(x) -> read_store x
 | Imp.Zero -> New("Zero", [])
 | Imp.Suc(e) -> MCall(cj_e e, "succ", [])
 | Imp.Dec(e) ->  MCall(cj_e e, "pred", [])

and cj_int n = cj_e (Imp.exp_of_int n)

let cj_b (Imp.NotZero(e)) = MCall(cj_e e, "notzero", [])

let noop = Id "new T()"
let this = Id "this"
let ite b e1 e2 = MCall(cj_b b, "ite", [e1; e2])
let run e = MCall(e, "run", [])
let start e = MCall(e, "start", [])

let rec cj_c imp_c = match imp_c with
| Imp.Skip -> Skip
| Imp.Assign(n,e) -> Atomic (write_store (cj_int n) (cj_e e))
| Imp.Seq(c1,c2) -> Seq(cj_c c1, cj_c c2)
| Imp.Ite(b,c1,c2) -> 
    run (Cast ("T", ite b (new_thread (cj_c c1)) (new_thread (cj_c c2))))
| Imp.Var(n,e,c) -> 
    let prg = expand_store (cj_int n) (cj_e e) in
    run (new_thread ~store:prg (cj_c c))
| Imp.Atomic c  -> Atomic(cj_c c)
| Imp.While (b,c) -> 
    let thread_loop = new_thread (Seq(cj_c c, run (Cast("T", ite b this noop))))
    in run (Cast("T", ite b thread_loop noop))
| Imp.Spawn c -> start (new_thread (cj_c c))
| Imp.Par _ -> failwith "Par must be reduced to Spawn before translation to cJ"
| Imp.Await _ -> failwith "Await must be reduced to Atomic before translation to cJ"


(* Some notes
 * - we create a new thread instance inside atomic() statements because
 *   we want to spawn new thread inside the atomic execution
 * - if we had local variables we could just do local var lock = new Lock();
 *   inside the synchronized {} statement
 *   instead of creating thread objects we could have a lock stack for each
     thread, then pop those when we exit a synchronized {} statement
 * - atomic() around store access needn't create a new thread instance, but
 *   we do it for consistency's sake (so that "Atomic" in cJ has only one
 *   meaning). Otherwise we could make Atomic translate to synchronized {}
 *   and transform Imp.Atomic expr to Atomic((run (new_thread expr)))
 *   and store accesses would just be Atomic(access)
 *)

(*...*)
(*synchronized (this.lock) {*)
  (*(new T(new Lock(), this.store)).run()*)
(*}*)
(*...*)

(* Print cJ to Java *)

let fmtp pad = Printf.ksprintf (fun s -> ((String.make pad ' ')^s))
let pad_str pad str = (String.make pad ' ')^str
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
      fmtp pad "synchronized(this.lock) %s"
      (curlies pad
        (lines [pad_str (pad+1) "Lock old_lock = this.lock; this.lock = new Lock();"
               ;((prt_expr (pad+1) e)^";")
               ;pad_str (pad+1) "this.lock = old_lock;"]))

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

(* Keep printing declarations until no new class is found *)
let rec prt_decls () =
  let d = !decls
  in decls := [];
  let str = lines (map (prt_decl 0) d) in
  let rest = if (List.length !decls) > 0 then (prt_decls ()) else "" in
  str^"\n"^rest

let rec prt_store s = 
  let rec aux = function
  | (k,v)::rest -> 
      fmtp 0 ".expand(%s,%s)%s" 
        (prt_expr 0 (cj_int k))  (prt_expr 0 (cj_e v)) (aux rest)
  | [] -> ""
  in fmtp 0 "new Nil()%s;" (aux s)

module Parser : sig 
    val parse_patch : string -> (int * Imp.exp) list
  end = struct
    (* From ["b";"3"] to [(1,3)] *)
    let rec pairs = function
      | k::v::xs -> 
          let ki = Imp.num k 
          and ve = Imp.exp_of_int (int_of_string v) 
          in (ki,ve)::(pairs xs)
      | [] -> []
      | _ -> failwith "pairs must get an even list as input"

    let format = Str.regexp "^\\([a-z]:[0-9]+ ?\\)*$"
    let delims = Str.regexp ":\\| "

    let parse_patch p =
      if Str.string_match format p 0 
      then pairs (Str.split delims p)
      else failwith ("Invalid store specifier string. "^
                     "Format is space-separated list of name:value pairs, e.g.  a:3 c:0")
end

let program_name = "Compiled"

(* Dirty work *)

let cj_prog c patch_s =
  print_string ((Imp.prt_c c)^"\n"); (* Show Imp for debugging *)
  let c = Imp.transform c in
  let store = patch_store (default_store c) (Parser.parse_patch patch_s) in
  let bootstrap = new_thread ~lock:(Id "new Lock()") ~store:(Id "st") (cj_c c) in
  let f = open_out (program_name^".java") in
  Printf.fprintf f 
    " %s\n public class %s {
    public static void main (String[] a) {
    List st = %s;
    System.out.println(\"Store before:\" +st.repr());
    T t = %s;
    t.run();
    System.out.println(\"Store  after:\" + t.store.repr());
    }}"
    (prt_decls ())
    program_name
    (prt_store store)
    (prt_expr 0 bootstrap)
  ; close_out f

let () =
  if (Array.length Sys.argv) < 2 then begin
    print_string "Give me an Imp program to compile. e.g. c5. Look in imp.ml
    Optionally, specify a store with a space-separated list of name:value, e.g.  a:3 c:0";
    exit 1 end
  else
    let patch_s = if (Array.length Sys.argv < 3) then "" else Sys.argv.(2) in
    cj_prog (Imp.get Sys.argv.(1)) patch_s
