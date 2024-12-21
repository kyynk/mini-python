open Ast

module StringMap = Map.Make(String)

type ty = [
  | `none 
  | `int 
  | `bool 
  | `string of int
  | `list of int
]

type env_t = {
  mutable vars: (var * int * ty) StringMap.t;
  funcs: fn StringMap.t;
  mutable stack_offset: int;
  mutable counters: int StringMap.t;
}

let empty_env = {
  vars = StringMap.empty;
  funcs = StringMap.empty;
  stack_offset = 0;
  counters = StringMap.empty;
}
