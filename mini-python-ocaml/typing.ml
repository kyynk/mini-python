
open Ast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(* Type environment *)
module Env = Map.Make(String)

type tenv = {
  vars: var Env.t;  (* Maps variable names to their metadata *)
  fns: fn Env.t;     (* Maps function names to their metadata *)
}

(* Utility functions to look up variables and functions *)
let lookup_var env name loc =
  try Env.find name env.vars
  with Not_found -> error ~loc "unbound variable %s" name

let lookup_fn env name loc =
  try Env.find name env.fns
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
      let expected = List.length f.fn_params in
      let actual = List.length args in
      if expected <> actual then
        error ~loc:id.loc
          "function %s expects %d arguments but got %d"
          id.id expected actual;
      let targs = List.map (type_expr env) args in
      TEcall (f, targs)
  | Elist elst ->
      let telst = List.map (type_expr env) elst in
      TElist telst
  | Eget (e1, e2) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      TEget (te1, te2)


(* Helper function to merge two environments *)
let merge_envs (env1: tenv) (env2: tenv) : tenv =
  let merged_vars =
    Env.union
      (fun _ v1 _ -> Some v1)
      env1.vars env2.vars
  in
  { env1 with vars = merged_vars }

(* Typing statements *)
let rec type_stmt_with_env (env: tenv) (stmt: stmt) : tenv * tstmt =
  print_endline "statement";
  match stmt with
  | Sif (e, s1, s2) ->
      let te = type_expr env e in
      let env1, ts1 = type_stmt_with_env env s1 in
      let env2, ts2 = type_stmt_with_env env s2 in
      let merged_env = merge_envs env1 env2 in
      merged_env, TSif (te, ts1, ts2)
  | Sreturn e ->
      let te = type_expr env e in
      env, TSreturn te
  | Sassign (id, e) ->
      print_endline ("assign " ^ id.id);
      let v =
        try lookup_var env id.id id.loc
        with _ ->
          let v = { v_name = id.id; v_ofs = 0 } in
          v
      in
      let te = type_expr env e in
      let new_env = { env with vars = Env.add id.id v env.vars } in
      new_env, TSassign (v, te)
  | Sprint e ->
      let te = type_expr env e in
      env, TSprint te
  | Sblock stmts ->
      (* let local_env = { env with vars = env.vars } in
      let tstmts = List.map (type_stmt local_env) stmts in *)
      let local_env, tstmts =
        List.fold_left
          (fun (acc_env, acc_stmts) stmt ->
            let new_env, tstmt = type_stmt_with_env acc_env stmt in
            new_env, acc_stmts @ [tstmt])
          (env, []) stmts
      in
      local_env, TSblock tstmts
  | Sfor (id, e, s) ->
      (* let te = type_expr env e in
      let v = { v_name = id.id; v_ofs = 0 } in
      let env' = { env with vars = Env.add id.id v env.vars } in
      let ts = type_stmt env' s in
      TSfor (v, te, ts) *)
      let te = type_expr env e in
      let v = { v_name = id.id; v_ofs = 0 } in
      let env' = { env with vars = Env.add id.id v env.vars } in
      let new_env, ts = type_stmt_with_env env' s in
      new_env, TSfor (v, te, ts)
  | Seval e ->
      let te = type_expr env e in
      env, TSeval te
  | Sset (e1, e2, e3) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      let te3 = type_expr env e3 in
      env, TSset (te1, te2, te3)

and type_stmt (env: tenv) (stmt: stmt) : tstmt =
  let _, tstmt = type_stmt_with_env env stmt in
  tstmt

(* Typing function definitions *)
let type_def (env: tenv) (id, params, body: def) : fn * tstmt =
  let vars =
    List.fold_left
      (fun acc param ->
        if Env.mem param.id acc then
          error ~loc:param.loc "duplicate parameter name %s in function %s" param.id id.id;
        let v = { v_name = param.id; v_ofs = 0 } in
        Env.add param.id v acc)
      env.vars params
  in
  let f = { fn_name = id.id; fn_params = List.map (fun p -> Env.find p.id vars) params } in
  let env' = { vars; fns = Env.add id.id f env.fns } in
  let tbody = type_stmt env' body in
  (f, tbody)


(* Typing a file *)
let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;
  (* failwith "TODO" *)
  let (defs, main) = p in
  let env = { vars = Env.empty; fns = Env.empty } in
  let tdefs = List.map (type_def env) defs in
  let tmain = type_stmt env main in
  tdefs @ [ ({ fn_name = "main"; fn_params = [] }, tmain) ]

