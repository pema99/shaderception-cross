module Typecheck

#nowarn "40"
open Combinator
open Common
open Parse
open Util

// === Vtable ===
let lookup vtab id = List.tryFind (fst >> (=) id) vtab |> Option.map snd
let exists vtab id = List.exists (fst >> (=) id) vtab
let insert vtab id ty = (id, ty) :: vtab
let update vtab id ty = List.map (fun (x, y) -> if x = id then ty else y) vtab
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
  | TUnion of Type * Set<Type>
  | TVar of string
  | TVec of int
  | TVoid

type BlockT = StmtT list

and StmtT =
  | ScopeT of BlockT
  | LetT of Type * string * string option * Expr
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
  | TUnion (r1, u), TUnion (_, v) -> TUnion (r1, Set.union u v) // Union 2 sets with either rep
  | TVar _, TVar _ -> TUnion (l, Set.ofList [l; r])             // Two type var, make union
  | TVec u, TVec v -> TVec (max u v)                            // Same basic type, return implicit cast
  | TVec 1, TVar _ -> TUnion (r, Set.ofList [r])                // Basic type and var, make union (handle cast)
  | TVar _, TVec 1 -> TUnion (l, Set.ofList [l])           
  | TVec 1, TUnion _ -> r                                       // Basic type and union, replace rep (handle cast)
  | TUnion _, TVec 1 -> l
  | TVar _, TUnion (r1, u) -> TUnion (r1, Set.add l u)          // Var and union, merge
  | TUnion (r1, u), TVar _ -> TUnion (r1, Set.add r u)       
  | TVec _, TVar _ -> TUnion (l, Set.ofList [r])                // Basic type and var, make union
  | TVar _, TVec _ -> TUnion (r, Set.ofList [l])           
  | TVec _, TUnion (_, u) -> TUnion (l, u)                      // Basic type and union, replace rep
  | TUnion (_, u), TVec _ -> TUnion (r, u)
  | _ -> TVoid                                                  // TODO: maybe error here

let equiv l r =
  match l, r with
  | u, v when u = v || rep u = rep v -> true
  | TUnion (_, u), v | v, TUnion (_, u) -> Set.contains v u
  | _ -> false

let dedup ctab =
  ctab
  |> List.groupBy fst
  |> List.map (fun (x, y) -> 
    if List.length y <= 1 then List.head y
       else
         let _, tys = List.unzip y
         let rets, parms = List.unzip tys
         let ret = List.reduce unify rets
         let parm = List.reduce (fun acc u -> List.zip acc u |> List.map (uncurry unify)) parms
         (x, (ret, parm)))

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
  | "dot" | "distance" -> Some (TVec 1, [TVar "a"; TVar "a"])
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

let rec inferExprType ctab ftab vtab expr =
  match expr with
  | Literal _ -> Some (TVec 1, ctab)
  | Var ident -> lookup vtab ident |> Option.map (fun x -> x, ctab)
  | Swizzle (_, i) -> Some (TVec (String.length i), ctab)
  | BinOp (l, _, r) ->
    match inferExprType ctab ftab vtab l, inferExprType ctab ftab vtab r with
    | Some (l, ctab1), Some (r, ctab2) ->
      Some (unify l r, ctab1 @ ctab2 |> dedup)
    | _ -> None
  | UnOp (_, r) -> inferExprType ctab ftab vtab r
  | Call (ident, parms) ->
    let parmsT = List.map (inferExprType ctab ftab vtab) parms
    if List.forall Option.isSome parmsT then
      let parmsT = List.choose id parmsT
      let parmsT, ctabs = List.unzip parmsT
      let ctabs = List.concat ctabs
      lookup ftab ident
      |> Option.orElse (getBuiltinType ident)
      |> Option.map (inferRetType parmsT)
      |> Option.map (fun x -> x, insert ctabs ident (x, parmsT) |> dedup)
    else None

let rec inferStmtType ctab ftab vtab stmt =
  match stmt with
  | Let (id, swizzle, init) ->
    inferExprType ctab ftab vtab init
    |> Option.map (fun (ty, ctab) -> LetT (ty, id, swizzle, init), ctab)
  | If (cond, body, alt) ->
    let condT = inferExprType ctab ftab vtab cond
    let bodyT = inferStmtType ctab ftab vtab body
    let altT = Option.bind (inferStmtType ctab ftab vtab) alt
    match condT, bodyT, altT with
    | Some (_, ctab1), Some (bodyT, ctab2), Some (altT, ctab3) -> Some (IfT (cond, bodyT, Some altT), ctab1 @ ctab2 @ ctab3 |> dedup)
    | Some (_, ctab1), Some (bodyT, ctab2), None -> Some (IfT (cond, bodyT, None), ctab1 @ ctab2 |> dedup)
    | _ -> None
  | While (cond, body) ->
    let condT = inferExprType ctab ftab vtab cond
    let bodyT = inferStmtType ctab ftab vtab body
    match condT, bodyT with
    | Some (_, ctab1), Some (bodyT, ctab2) -> Some (WhileT (cond, bodyT), ctab1 @ ctab2 |> dedup)
    | _ -> None
  | For (decl, cond, iter, body) ->
    let condT = inferExprType ctab ftab vtab cond
    let declT = inferStmtType ctab ftab vtab decl
    let vtab =
      match declT with
      | Some (LetT (ty, ident, _, _), _) -> insert vtab ident ty
      | _ -> vtab
    let iterT = inferStmtType ctab ftab vtab iter
    let bodyT = inferStmtType ctab ftab vtab body
    match declT, condT, iterT, bodyT with
    | Some (declT, ctab1), Some(_, ctab2), Some (iterT, ctab3), Some (bodyT, ctab4) ->
      Some (ForT (declT, cond, iterT, bodyT), ctab1 @ ctab2 @ ctab3 @ ctab4 |> dedup)
    | _ -> None
  | Return e ->
    match inferExprType ctab ftab vtab e with
    | Some (ty, ctab) -> Some (ReturnT (ty, e), ctab)
    | None -> None
  | Scope body ->
    let (bodyT, ctab) = 
      body
      |> List.fold (fun (ctab, ftab, vtab, stmts : list<option<StmtT>>) stmt ->
        match stmt with
        | Fun (name, _, _) -> 
          let ret = inferStmtType ctab ftab vtab stmt
          match ret with
          | Some (FunT (retT, _, parms, _), ctab) ->
            ctab, insert ftab name (retT, List.map fst parms), vtab, Option.map fst ret :: stmts
          | Some (a, ctab) ->
            ctab, ftab, vtab, Some a :: stmts
          | None ->
            ctab, ftab, vtab, None :: stmts
        | Let (id, _, _) -> 
          let ty = inferStmtType ctab ftab vtab stmt
          match ty with
          | Some (LetT (tyT, _, _, _), ctab) ->
            ctab, ftab, insert vtab id tyT, Option.map fst ty :: stmts
          | Some (a, ctab) ->
            ctab, ftab, vtab, Some a :: stmts
          | None ->
            ctab, ftab, vtab, None :: stmts
        | _ -> 
          let ty = inferStmtType ctab ftab vtab stmt
          match ty with
          | Some (a, ctab) -> ctab, ftab, vtab, Some a :: stmts
          | None -> ctab, ftab, vtab, None :: stmts
      ) (ctab, ftab, vtab, [])
      |> fun (ctab, _, _, stmts) -> List.rev stmts, ctab
    if List.forall Option.isSome bodyT then Some (ScopeT (List.choose id bodyT), ctab)
    else None
  | Fun (name, parms, body) ->
    match lookup ctab name with
    | Some (retT, parmsT) ->
      let parmsT = List.zip parms parmsT
      let vtab = parmsT @ vtab // TODO: Is this right?
      let bodyT = inferStmtType ctab ftab vtab body
      match bodyT with
      | Some (ScopeT _ as block, ctab) ->
        let parmsT = parmsT |> List.map (fun (ident, ty) -> if equiv ty retT then retT, ident else ty, ident)
        Some (FunT (retT, name, parmsT, block), ctab)
      | _ -> None
    | None -> 
      let parmsT = List.map (fun x -> x, TVar (name + "_" +  x)) parms
      let vtab = parmsT @ vtab // TODO: Is this right?
      let bodyT = inferStmtType ctab ftab vtab body
      match bodyT with
      | Some (ScopeT _ as block, ctab) ->
        let retT = extractRetType block
        let parmsT = parmsT |> List.map (fun (ident, ty) -> if equiv ty retT then retT, ident else ty, ident)
        Some (FunT (retT, name, parmsT, block), ctab)
      | _ -> None
      
let typecheck ast = 
  inferStmtType [] [] [] ast                                     // gather function types
  |> Option.bind (fun (_, ctab) -> inferStmtType ctab [] [] ast) // reinfer with new information
  |> Option.map fst
  