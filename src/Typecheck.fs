#nowarn "40"

open Combinator
open Common
open Parse
open Util

// === Typed AST ===
type Type =
  | Void
  | Hole
  | Float
  | Float2
  | Float3
  | Float4

let promote a b =
  match a, b with
  | Hole, _ -> Hole
  | _, Hole -> Hole
  | Float4, _ | _, Float4 -> Float4
  | Float3, _ | _, Float3 -> Float3
  | Float2, _ | _, Float2 -> Float2
  | Float, _  | _, Float  -> Float
  | _ -> Void 

type BlockT = StmtT list

and StmtT =
  | ScopeT of BlockT
  | LetT of Type * string * Expr
  | IfT of Expr * StmtT * StmtT option
  | WhileT of Expr * StmtT
  | ForT of StmtT * Expr * StmtT * StmtT
  | FunT of Type * string * (Type * string) list * StmtT
  | ReturnT of Type * Expr

let rec extractRetType stmt =
  match stmt with
  | ScopeT (stmts) ->
    stmts
    |> List.tryLast
    |> Option.map extractRetType
    |> Option.defaultValue Void
  | LetT _ -> Void
  | IfT (_, body, _) -> extractRetType body // TODO: Check alternate
  | WhileT _ -> Void
  | ForT _ -> Void
  | FunT _ -> Void 
  | ReturnT (ty, _) -> ty 

// === Builtins ===
let getBuiltinType id =
  match id with
  | "log" | "log2" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan"
  | "pow" | "exp" | "exp2" | "sqrt" | "rsqrt" | "abs" | "sign" | "floor"
  | "ceil" | "frac" | "mod" | "min" | "max" | "clamp" | "lerp" | "step"
  | "smoothstep" | "normalize" | "reflect" | "refract" | "round" | "cross" -> Some Hole
  | "float2" -> Some Float2
  | "float3" -> Some Float3
  | "float4" -> Some Float4
  | "uv" | "xy" | "resolution" -> Some Float2
  | "time" | "self" | "camera" | "deltatime" | "video" | "button" | "axis" -> Some Float4
  | "dot" | "distance" | "length" | "all" | "any" -> Some Float
  | _ -> None

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

// === Typechecking ===
let rec getExprType ftab vtab expr =
  match expr with
  | Literal _ -> Some Float
  | Var id -> lookup vtab id
  | BinOp (l, _, r) ->
    match getExprType ftab vtab l, getExprType ftab vtab r with
    | Some l, Some r -> Some (promote l r)
    | _ -> None
  | UnOp (_, r) -> getExprType ftab vtab r
  | Call (idt, parms) ->
    let mgu =
      parms
      |> List.map (getExprType ftab vtab)
      |> List.choose id
      |> List.fold promote Hole
    let ret = 
      lookup ftab idt
      |> Option.orElse (getBuiltinType idt)
    match ret with
    | Some Hole -> Some mgu
    | _ -> ret

let rec getStmtType ftab vtab stmt =
  match stmt with
  | Scope body ->
    let bodyT = 
      body
      |> List.fold (fun (ftab, vtab, stmts) stmt ->
        match stmt with
        | Fun (name, _, _) -> 
          let ret = getStmtType ftab vtab stmt
          match ret with
          | Some (FunT (retT, _, _, _)) -> insert ftab name retT, vtab, ret :: stmts
          | a -> ftab, vtab, a :: stmts
        | Let (id, _) -> 
          let ty = getStmtType ftab vtab stmt
          match ty with
          | Some (LetT (tyT, _, _)) -> ftab, insert vtab id tyT, ty :: stmts
          | a -> ftab, vtab, a :: stmts
        | _ -> 
          ftab, vtab, getStmtType ftab vtab stmt :: stmts
      ) (ftab, vtab, [])
      |> fun (_, _, stmts) -> List.rev stmts
    if List.forall Option.isSome bodyT then Some (ScopeT (List.choose id bodyT))
    else None
  | Let (id, init) ->
    getExprType ftab vtab init
    |> Option.map (fun x -> LetT (x, id, init))
  | If (cond, body, alt) ->
    let bodyT = getStmtType ftab vtab body
    let altT = Option.bind (getStmtType ftab vtab) alt
    match bodyT with
    | Some bodyT -> Some (IfT (cond, bodyT, altT))
    | _ -> None
  | While (cond, body) ->
    let bodyT = getStmtType ftab vtab body
    match bodyT with
    | Some bodyT -> Some (WhileT (cond, bodyT))
    | _ -> None
  | For (decl, cond, iter, body) ->
    let declT = getStmtType ftab vtab decl
    let iterT = getStmtType ftab vtab iter
    let bodyT = getStmtType ftab vtab body
    match declT, iterT, bodyT with
    | Some declT, Some iterT, Some bodyT -> Some (ForT (declT, cond, iterT, bodyT))
    | _ -> None
  | Fun (name, parms, body) ->
    let vtab = (List.map (fun x -> (x, Hole)) parms) @ vtab
    let bodyT = getStmtType ftab vtab body
    match bodyT with
    | Some (ScopeT stmts) ->
      let retT =
        stmts
        |> List.tryLast
        |> Option.map extractRetType
      let parmsT =
        stmts
        |> List.map (fun x -> match x with LetT (ty, id, _) -> Some (ty, id) | _ -> None)
        |> List.choose id
      match retT with
      | Some retT -> Some (FunT (retT, name, parmsT, ScopeT stmts))
      | None -> None
    | _ -> None
  | Return e ->
    match getExprType ftab vtab e with
    | Some ty -> Some (ReturnT (ty, e))
    | None -> None

let fillHole ty =
  if ty = Hole then Float4
  else ty

let rec fillHoles stmt : StmtT =
  match stmt with
  | ScopeT body -> 
    ScopeT (body |> List.map (fillHoles))
  | LetT (ty, id, init) -> 
    LetT (fillHole ty, id, init)
  | IfT (cond, body, alt) -> 
    IfT (cond, fillHoles body, Option.map (fillHoles) alt)
  | WhileT (cond, body) -> 
    WhileT (cond, fillHoles body)
  | ForT (decl, cond, iter, body) -> 
    ForT (fillHoles decl, cond, fillHoles iter, fillHoles body)
  | FunT (ret, id, parms, body) -> 
    FunT (fillHole ret, id, parms |> List.map (fun (t, s) -> fillHole t, s), fillHoles body)
  | ReturnT (ty, e) ->
    ReturnT (fillHole ty, e)

let typecheck ast =
  getStmtType [] [] ast
  |> Option.map (fillHoles)

let dataSrc = 
  //System.IO.File.ReadAllText("test.psl")
  "fun smin(d1, d2)
{
    dot(d1, d2) + float2(0,0)
}"
  |> mkMultiLineParser

// TODO: Handle globals oof
let result, state = blockP dataSrc
let ast = 
  match result with
  | Success v ->
    //printfn "Result: %A\n\nState: %A" v state
    v
  | _ -> failwith (sprintf "Error: %A" state)

let funs, stmts = List.partition (fun x -> match x with Fun _ -> true | _ -> false) ast
funs
|> List.head
|> typecheck
|> printfn "%A"