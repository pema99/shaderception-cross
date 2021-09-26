#nowarn "40"
open Combinator
open Common
open Parse
open Typecheck
open Util

let genOp op =
  match op with
  | Plus       -> "+"
  | PlusEq     -> "+="
  | Minus      -> "-"
  | MinusEq    -> "-="
  | Star       -> "*"
  | StarEq     -> "*="
  | Slash      -> "/"
  | SlashEq    -> "/="
  | Not        -> "!"
  | NotEq      -> "!="
  | Greater    -> ">"
  | GreaterEq  -> ">="
  | Less       -> "<"
  | LessEq     -> "<="
  | PlusPlus   -> "++"
  | MinusMinus -> "--"
  | Equal      -> "=="
  | And        -> "&&"
  | Or         -> "||"

let genType ty =
  let rec cont ty level = 
    if level > 3 then "[Union]"
    else
      match ty with
      | TVec i when i = 1 -> "float"
      | TVec i -> sprintf "float%i" i
      | TVoid -> "void"
      | TVar a -> sprintf "[%s]" a
      | _ -> cont (rep ty) (level + 1)
  cont ty 0

let rec genExpr expr =
  match expr with
  | Literal v -> string v
  | Var id -> id
  | BinOp (l, op, r) -> sprintf "%s %s %s" (genExpr l) (genOp op) (genExpr r)
  | UnOp (op, r) -> sprintf "%s %s" (genOp op) (genExpr r)
  | Call (idt, parms) -> sprintf "%s(%s)" idt (parms |> List.map genExpr |> String.concat ", ")

let indent level =
  Seq.init level (konst "\t") |> String.concat ""

let rec genStmt level stmt =
  match stmt with
  | ScopeT body ->
    sprintf "%s{\n%s\n%s}\n" (indent level) (body |> List.map (genStmt (level + 1)) |> String.concat "\n") (indent level)
  | LetT (ty, id, init) ->
    sprintf "%s%s %s = %s;" (indent level) (genType ty) id (genExpr init)
  | IfT (cond, body, alt) ->
    let rest = 
      match alt with
      | Some alt -> sprintf "%selse\n%s" (indent level) (genStmt level alt)
      | None -> "" 
    sprintf "%sif (%s)\n%s%s" (indent level) (genExpr cond) (genStmt level body) rest
  | WhileT (cond, body) ->
    sprintf "%swhile (%s)\n%s" (indent level) (genExpr cond) (genStmt level body)
  | ForT (decl, cond, iter, body) ->
    sprintf "%sfor (%s %s; %s)\n%s" (indent level) (genStmt level decl) (genExpr cond) (genStmt level iter) (genStmt level body)
  | FunT (ret, id, parms, body) ->
    let parms =
      parms
      |> List.map (fun (ty, id) -> sprintf "%s %s" (genType ty) id)
      |> String.concat ", "
    sprintf "%s%s %s(%s)\n%s" (indent level) (genType ret) id parms (genStmt level body)
  | ReturnT (_, e) ->
    sprintf "%sreturn %s;" (indent level) (genExpr e)

let dataSrc = 
  //System.IO.File.ReadAllText("test.psl")
  "fun smin(d1, d2, k)
{
    let h = clamp(0.5 + 0.5 * (d2 - d1) / k, 0.0, 1.0);
    lerp(d2, d1, h) - k * h * (1.0 - h)
}

fun map(p)
{
    let d1 = length(p) - 0.3;
    let d2 = length(p + float3(0.2, sin(time())*0.3, 0)) - 0.3;
    smin(d1, d2, 0.05)
}

fun march(ro, rd)
{
    let t = 0;
    let i = 0;
    while (i < 15)
    {
        let dist = map(ro + t * rd);
        let t = t + dist;
        let i = i + 1;
    }
    t
}

let p = 2.0 * (uv() - 0.5);
let ro = float3(0.0, 0.0, -1.0);
let rd = normalize(float3(p, p, 1));
"
  |> mkMultiLineParser

// TODO: Handle globals oof
let result, state = blockP dataSrc
let ast = 
  match result with
  | Success v -> v
  | _ -> failwith (sprintf "Error: %A" state)
//printfn "%A" ast
Scope ast
|> typecheck
|> Option.map (genStmt 0)
|> printfn "%A"

