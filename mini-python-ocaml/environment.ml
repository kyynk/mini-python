open Ast
module StringMap = Map.Make (String)

type offset = int

type env_t =
  { mutable vars : offset StringMap.t
  ; funcs : fn StringMap.t
  ; mutable stack_offset : int
  ; mutable counters : int StringMap.t
  }

let create_env () =
  { vars = StringMap.empty
  ; funcs = StringMap.empty
  ; stack_offset = 0
  ; counters = StringMap.empty
  }
;;
