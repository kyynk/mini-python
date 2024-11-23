
open Ast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(* Type environments *)
module VarEnv = Map.Make(String)
module FnEnv = Map.Make(String)

type tenv = {
  vars: var VarEnv.t;  (* Maps variable names to their metadata *)
  fns: fn FnEnv.t;     (* Maps function names to their metadata *)
}

(* Utility functions to look up variables and functions *)
let lookup_var env name loc =
  try VarEnv.find name env.vars
  with Not_found -> error ~loc "unbound variable %s" name

let lookup_fn env name loc =
  try FnEnv.find name env.fns
  with Not_found -> error ~loc "unbound function %s" name

(* Typing expressions *)
let rec type_expr (env: tenv) (expr: expr) : texpr =
  match expr with
  | Ecst c -> TEcst c
  | Eident id ->
      let v = lookup_var env id.id id.loc in
      TEvar v
  | Ebinop (op, e1, e2) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      TEbinop (op, te1, te2)
  | Eunop (op, e) ->
      let te = type_expr env e in
      TEunop (op, te)
  | Ecall (id, args) ->
      let f = lookup_fn env id.id id.loc in
      let targs = List.map (type_expr env) args in
      TEcall (f, targs)
  | Elist el ->
      let tel = List.map (type_expr env) el in
      TElist tel
  | Eget (e1, e2) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      TEget (te1, te2)

(* Typing statements *)
let rec type_stmt (env: tenv) (stmt: stmt) : tstmt =
  match stmt with
  | Sif (e, s1, s2) ->
      let te = type_expr env e in
      let ts1 = type_stmt env s1 in
      let ts2 = type_stmt env s2 in
      TSif (te, ts1, ts2)
  | Sreturn e ->
      let te = type_expr env e in
      TSreturn te
  | Sassign (id, e) ->
      let v = lookup_var env id.id id.loc in
      let te = type_expr env e in
      TSassign (v, te)
  | Sprint e ->
      let te = type_expr env e in
      TSprint te
  | Sblock stmts ->
      let tstmts = List.map (type_stmt env) stmts in
      TSblock tstmts
  | Sfor (id, e, s) ->
      let te = type_expr env e in
      let v = { v_name = id.id; v_ofs = 0 } in
      let env' = { env with vars = VarEnv.add id.id v env.vars } in
      let ts = type_stmt env' s in
      TSfor (v, te, ts)
  | Seval e ->
      let te = type_expr env e in
      TSeval te
  | Sset (e1, e2, e3) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      let te3 = type_expr env e3 in
      TSset (te1, te2, te3)

(* Typing function definitions *)
let type_def (env: tenv) (id, params, body: def) : fn * tstmt =
  let vars =
    List.fold_left
      (fun acc param ->
         let v = { v_name = param.id; v_ofs = 0 } in
         VarEnv.add param.id v acc)
      env.vars params
  in
  let f = { fn_name = id.id; fn_params = List.map (fun p -> VarEnv.find p.id vars) params } in
  let env' = { vars; fns = FnEnv.add id.id f env.fns } in
  let tbody = type_stmt env' body in
  (f, tbody)


(* Typing a file *)
let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;
  (* failwith "TODO" *)
  let (defs, main) = p in
  let env = { vars = VarEnv.empty; fns = FnEnv.empty } in
  let tdefs = List.map (type_def env) defs in
  let tmain = type_stmt env main in
  tdefs @ [ ({ fn_name = "main"; fn_params = [] }, tmain) ]

