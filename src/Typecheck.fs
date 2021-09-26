module Typecheck

#nowarn "40"
open Combinator
open Common
open Parse

// === Vtable ===
let lookup vtab id = List.tryFind (fst >> (=) id) vtab |> Option.map snd
let insert vtab id ty = (id, ty) :: vtab
let check vtab id ty =
  match lookup vtab id with
  | Some v -> v, vtab
  | None -> ty, insert vtab id ty
let ensure vtab id ty =
  match lookup vtab id with
  | Some v -> v = ty
  | None -> false

// === Typed AST ===
type Type =
  | TUnion of Type * Type list
  | TVar of string
  | TVec of int
  | TVoid

type BlockT = StmtT list

and StmtT =
  | ScopeT of BlockT
  | LetT of Type * string * Expr
  | IfT of Expr * StmtT * StmtT option
  | WhileT of Expr * StmtT
  | ForT of StmtT * Expr * StmtT * StmtT
  | FunT of Type * string * (Type * string) list * StmtT
  | ReturnT of Type * Expr

// === Type equivalence classes ===
let rep l =
  match l with
  | TUnion (r, _) -> r
  | r -> r

let unify l r =
  match l, r with
  | TUnion (r1, u), TUnion (_, v) -> TUnion (r1, u @ v) // Union 2 sets with either rep
  | TVar _, TVar _ -> TUnion (l, [l; r])                // Two type var, make union
  | TVec u, TVec v -> TVec (max u v)                    // Same basic type, return implicit cast
  | TVec 1, TVar _ -> TUnion (r, [r])                   // Basic type and var, make union (handle cast)
  | TVar _, TVec 1 -> TUnion (l, [l])           
  | TVec 1, TUnion _ -> r                               // Basic type and union, replace rep (handle cast)
  | TUnion _, TVec 1 -> l
  | TVar _, TUnion (r1, u) -> TUnion (r1, l :: u)       // Var and union, merge
  | TUnion (r1, u), TVar _ -> TUnion (r1, r :: u)       
  | TVec _, TVar _ -> TUnion (l, [r])                   // Basic type and var, make union
  | TVar _, TVec _ -> TUnion (r, [l])           
  | TVec _, TUnion (_, u) -> TUnion (l, u)              // Basic type and union, replace rep
  | TUnion (_, u), TVec _ -> TUnion (r, u)
  | _ -> TVoid                                          // TODO: maybe error here

let equiv l r =
  match l, r with
  | u, v when u = v || rep u = rep v -> true
  | TUnion (_, u), v | v, TUnion (_, u) -> List.contains v u
  | _ -> false

// === Builtins ===
let getBuiltinType id =
  match id with
  | "log" | "log2" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan"
  | "exp" | "exp2" | "sqrt" | "rsqrt" | "abs" | "sign" | "floor" | "ceil"
  | "frac" | "normalize" | "round" -> Some (TVar "a", [TVar "a"])
  | "pow" | "mod" | "min" | "max" | "step" | "reflect" | "cross" -> Some (TVar "a", [TVar "a"; TVar "a"])
  | "clamp" | "lerp" | "smoothstep" -> Some (TVar "a", [TVar "a"; TVar "a"; TVar "a"])
  | "refract" -> Some (TVar "a", [TVar "a"; TVar "a"; TVec 1])
  | "float2" -> Some (TVec 2, [TVec 1; TVec 1])
  | "float3" -> Some (TVec 3, [TVec 1; TVec 1; TVec 1])
  | "float4" -> Some (TVec 4, [TVec 1; TVec 1; TVec 1; TVec 1])
  | "uv" | "xy" | "resolution" -> Some (TVec 2, [])
  | "time" | "self" | "deltatime" | "button" | "axis" -> Some (TVec 4, [])
  | "camera" | "video" -> Some (TVec 4, [TVec 2])
  | "dot" -> Some (TVec 1, [TVar "a"])
  | "distance" -> Some (TVec 1, [TVar "a"; TVar "a"])
  | "length" | "all" | "any" -> Some (TVec 1, [TVar "a"])
  | _ -> None

// === Typechecking ===
let rec extractRetType stmt =
  match stmt with
  | ScopeT (stmts) ->
    stmts
    |> List.tryLast
    |> Option.map extractRetType
    |> Option.defaultValue TVoid
  | LetT _ -> TVoid
  | IfT (_, body, _) -> extractRetType body // TODO: Check alternate
  | WhileT _ -> TVoid
  | ForT _ -> TVoid
  | FunT _ -> TVoid 
  | ReturnT (ty, _) -> ty 

let inferRetType parms fty =
  let ret, pty = fty
  (pty, parms)
  ||> List.zip
  |> List.tryFind (fun (a, _) -> rep a = rep ret)
  |> Option.map snd
  |> Option.defaultValue ret

let rec inferExprType ftab vtab expr =
  match expr with
  | Literal _ -> Some (TVec 1)
  | Var ident -> lookup vtab ident
  | BinOp (l, _, r) ->
    match inferExprType ftab vtab l, inferExprType ftab vtab r with
    | Some l, Some r -> Some (unify l r)
    | _ -> None
  | UnOp (_, r) -> inferExprType ftab vtab r
  | Call (ident, parms) ->
    let parmsT = List.map (inferExprType ftab vtab) parms
    if List.forall Option.isSome parmsT then
      let parmsT = List.choose id parmsT
      lookup ftab ident
      |> Option.orElse (getBuiltinType ident)
      |> Option.map (inferRetType parmsT)
    else None

let rec inferStmtType ftab vtab stmt =
  match stmt with
  | Let (id, init) ->
    inferExprType ftab vtab init
    |> Option.map (fun x -> LetT (x, id, init))
  | If (cond, body, alt) ->
    let bodyT = inferStmtType ftab vtab body
    let altT = Option.bind (inferStmtType ftab vtab) alt
    match bodyT with
    | Some bodyT -> Some (IfT (cond, bodyT, altT))
    | _ -> None
  | While (cond, body) ->
    let bodyT = inferStmtType ftab vtab body
    match bodyT with
    | Some bodyT -> Some (WhileT (cond, bodyT))
    | _ -> None
  | For (decl, cond, iter, body) ->
    let declT = inferStmtType ftab vtab decl
    let iterT = inferStmtType ftab vtab iter
    let bodyT = inferStmtType ftab vtab body
    match declT, iterT, bodyT with
    | Some declT, Some iterT, Some bodyT -> Some (ForT (declT, cond, iterT, bodyT))
    | _ -> None
  | Return e ->
    match inferExprType ftab vtab e with
    | Some ty -> Some (ReturnT (ty, e))
    | None -> None
  | Scope body ->
    let bodyT = 
      body
      |> List.fold (fun (ftab, vtab, stmts) stmt ->
        match stmt with
        | Fun (name, _, _) -> 
          let ret = inferStmtType ftab vtab stmt
          match ret with
          | Some (FunT (retT, _, parms, _)) -> insert ftab name (retT, List.map fst parms), vtab, ret :: stmts
          | a -> ftab, vtab, a :: stmts
        | Let (id, _) -> 
          let ty = inferStmtType ftab vtab stmt
          match ty with
          | Some (LetT (tyT, _, _)) -> ftab, insert vtab id tyT, ty :: stmts
          | a -> ftab, vtab, a :: stmts
        | _ -> 
          ftab, vtab, inferStmtType ftab vtab stmt :: stmts
      ) (ftab, vtab, [])
      |> fun (_, _, stmts) -> List.rev stmts
    if List.forall Option.isSome bodyT then Some (ScopeT (List.choose id bodyT))
    else None
  | Fun (name, parms, body) ->
    let parmsT = List.map (fun x -> x, TVar (name + "_" +  x)) parms
    let vtab = parmsT @ vtab
    let bodyT = inferStmtType ftab vtab body
    match bodyT with
    | Some (ScopeT _ as block) ->
      let retT = extractRetType block
      let parmsT = parmsT |> List.map (fun (ident, ty) -> if equiv ty retT then retT, ident else ty, ident)
      Some (FunT (retT, name, parmsT, block))
    | _ -> None

let typecheck ast = inferStmtType [] [] ast