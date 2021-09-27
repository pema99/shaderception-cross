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
  | Swizzle (e, i) -> sprintf "(%s).%s" (genExpr e) i
  | BinOp (l, op, r) -> sprintf "(%s %s %s)" (genExpr l) (genOp op) (genExpr r)
  | UnOp (op, r) -> sprintf "%s %s" (genOp op) (genExpr r)
  | Call (idt, parms) -> sprintf "%s(%s)" idt (parms |> List.map genExpr |> String.concat ", ")

let indent level =
  Seq.init (max level 0) (konst "    ") |> String.concat ""

let rec genStmt level (vtab: Set<string>) stmt =
  match stmt with
  | ScopeT body ->
    let bodyG = // handle multiple declarations (only assignments exist in PSL)
      body
      |> List.fold (fun (stmts, vtab) stmt ->
        let vtabn =
          match stmt with
          | LetT (_, id, _, _) -> Set.add id vtab
          | _ -> vtab
        genStmt (level + 1) vtab stmt :: stmts, vtabn
      ) ([], vtab)
      |> fst
      |> List.rev
      |> String.concat "\n"
    sprintf "%s{\n%s\n%s}\n" (indent level) bodyG (indent level)
  | LetT (ty, id, swizzle, init) ->
    let swizzle = swizzle |> Option.map ((+) ".") |> Option.defaultValue "" 
    if Set.contains id vtab then sprintf "%s%s%s = %s;" (indent level) id swizzle (genExpr init)
    else sprintf "%s%s %s%s = %s;" (indent level) (genType ty) id swizzle (genExpr init)
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


// TODO: Handle globals oof
// TODO: Remove redundant parenthesis by checking precedence
[<EntryPoint>]
let main args =
  if args.Length < 1 then
    printfn "No supplied file path to shader"
  else
    let result, state = 
      System.IO.File.ReadAllText(args.[0])
      |> mkMultiLineParser
      |> blockP
    let ast = 
      match result with
      | Success v -> v
      | _ -> failwith (sprintf "Error: %A" state)
    let template = System.IO.File.ReadAllText "src/Template.shader"
    Scope ast
    |> typecheck
    |> Option.map genCode
    |> Option.map (fun x -> template.Replace("__CODE_GOES_HERE__", x))
    |> Option.map (fun x -> System.IO.File.WriteAllText("test.shader", x))
    |> ignore
  0