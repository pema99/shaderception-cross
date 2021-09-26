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
  Seq.init (max level 0) (konst "\t") |> String.concat ""

let rec genStmt level (vtab: Set<string>) stmt =
  match stmt with
  | ScopeT body ->
    let bodyG = // handle multiple declarations (only assignments exist in PSL)
      body
      |> List.fold (fun (stmts, vtab) stmt ->
        let vtabn =
          match stmt with
          | LetT (_, id, _) -> Set.add id vtab
          | _ -> vtab
        genStmt (level + 1) vtab stmt :: stmts, vtabn
      ) ([], vtab)
      |> fst
      |> List.rev
      |> String.concat "\n"
    sprintf "%s{\n%s\n%s}\n" (indent level) bodyG (indent level)
  | LetT (ty, id, init) ->
    if Set.contains id vtab then sprintf "%s%s = %s;" (indent level) id (genExpr init)
    else sprintf "%s%s %s = %s;" (indent level) (genType ty) id (genExpr init)
  | IfT (cond, body, alt) ->
    let rest = 
      match alt with
      | Some alt -> sprintf "%selse\n%s" (indent level) (genStmt level vtab alt)
      | None -> "" 
    sprintf "%sif (%s)\n%s%s" (indent level) (genExpr cond) (genStmt level vtab body) rest
  | WhileT (cond, body) ->
    sprintf "%swhile (%s)\n%s" (indent level) (genExpr cond) (genStmt level vtab body)
  | ForT (decl, cond, iter, body) ->
    sprintf "%sfor (%s %s; %s)\n%s" (indent level) (genStmt level vtab decl) (genExpr cond) (genStmt level vtab iter) (genStmt level vtab body)
  | FunT (ret, id, parms, body) ->
    let parms =
      parms
      |> List.map (fun (ty, id) -> sprintf "%s %s" (genType ty) id)
      |> String.concat ", "
    sprintf "%s%s %s(%s)\n%s" (indent level) (genType ret) id parms (genStmt level vtab body)
  | ReturnT (_, e) ->
    sprintf "%sreturn %s;" (indent level) (genExpr e)

let genSegment block =
  let gen = genStmt -1 Set.empty (ScopeT block)
  gen.[1 .. Seq.length gen - 3]

let genCode ast =
  match ast with
  | ScopeT v ->
    let funs, main = List.partition (fun x -> match x with FunT _ -> true | _ -> false) v
    let funsC = genSegment funs
    let main = FunT (extractRetType (ScopeT main), "main", [], (ScopeT main))
    let mainC = genStmt 0 Set.empty main
    funsC + mainC
  | _ -> genStmt 0 Set.empty ast

let dataSrc = 
  "sin(time().y)*0.3"
  //System.IO.File.ReadAllText("test.psl")
  (*"fun smin(d1, d2, k)
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

let d = march(ro, rd);
if (d < 1)
{
    let hit = ro + d * rd;
    let a = map(hit+float3(0.01, 0, 0)) - map(hit-float3(0.01, 0, 0));
    let b = map(hit+float3(0, 0.01, 0)) - map(hit-float3(0, 0.01, 0));
    let c = map(hit+float3(0, 0, 0.01)) - map(hit-float3(0, 0, 0.01));
    normalize(float3(a, b, c)) * 0.5 + 0.5
}
else
{
    0
}
"*)
  |> mkMultiLineParser

// TODO: Handle globals oof
let result, state = blockP dataSrc
let ast = 
  match result with
  | Success v -> printfn "%A" v; v
  | _ -> failwith (sprintf "Error: %A" state)
//printfn "%A" ast
Scope ast
|> typecheck
|> Option.map genCode
|> printfn "%A"

