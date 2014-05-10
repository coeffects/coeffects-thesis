open Microsoft.FSharp.Quotations

// --------------------------------------------------------------------------------------
// Untyped and typed AST & definition of types
// --------------------------------------------------------------------------------------

type Expr = 
  | Var of string
  | Const of int
  | App of Expr * Expr
  | Abs of string * Expr
  override x.ToString() = 
    match x with 
    | Var(v) -> v
    | Const(v) -> sprintf "%d" v
    | App(App(Var "+", e1), e2) -> sprintf "(%O + %O)" e1 e2
    | App(e1, e2) -> sprintf "(%O %O)" e1 e2
    | Abs(v, e) -> sprintf "(λ%s → %O)" v e

type Typ = 
  | Int 
  | Var of string
  | Fun of Typ * Typ
  override x.ToString() =
    match x with 
    | Var(v) -> v
    | Int -> "int"
    | Fun(t1, t2) -> sprintf "(%O → %O)" t1 t2

type Tyexpr = 
  | Var of string
  | Const of int
  | App of Tyexpr * Tyexpr
  | Abs of string * Typ * Tyexpr
  override x.ToString() = 
    match x with 
    | Var(v) -> v
    | Const(v) -> sprintf "%d" v
    | App(App(Var "+", e1), e2) -> sprintf "(%O + %O)" e1 e2
    | App(e1, e2) -> sprintf "(%O %O)" e1 e2
    | Abs(v, t, e) -> sprintf "(λ%s:%O → %O)" v t e

// --------------------------------------------------------------------------------------
// Dealing with F# quotations
// --------------------------------------------------------------------------------------

let rec asExpr = function
  | Patterns.Value(:? int as n, _) -> Expr.Const(n)
  | Patterns.Application(e1, e2) -> Expr.App(asExpr e1, asExpr e2)
  | Patterns.Var(v) -> Expr.Var(v.Name)
  | Patterns.Lambda(v, e) -> Expr.Abs(v.Name, asExpr e)
  | Patterns.Call(None, mi, [l; r]) when mi.Name = "op_Addition" -> Expr.App(Expr.App(Expr.Var "+", asExpr l), asExpr r)
  | e -> failwithf "%A" e

// --------------------------------------------------------------------------------------
// Constraint solving
// --------------------------------------------------------------------------------------

let rec substType f = function
  | Typ.Var(v) -> f v
  | Typ.Fun(t1, t2) -> Typ.Fun(substType f t1, substType f t2)
  | Typ.Int -> Typ.Int

let substTypeEq v t =
  let subst v' = if v = v' then t else Typ.Var(v')
  substType subst

let substEnv v t = 
  List.map (fun (tl, tr) -> substTypeEq v t tl, substTypeEq v t tr)

let rec solve cs res = 
  match cs with 
  | [] -> res
  | (Typ.Int, Typ.Int)::cs -> solve cs res
  | (Typ.Fun(t1, t2), Typ.Fun(t1', t2'))::cs -> solve ((t1, t1')::(t2, t2')::cs) res
  | (t, Typ.Var(v) | Typ.Var(v), t)::cs -> 
      match Map.tryFind v res with
      | Some t' -> solve ((t, t')::cs) res
      | None -> solve (substEnv v t cs) (Map.add v t res)
  | (t1, t2)::cs -> failwithf "Type '%O' does not match type '%O'." t1 t2

let expand res = 
  let map = Map.ofSeq res
  let rec subst v = 
    match Map.tryFind v map with 
    | Some t -> substType subst t 
    | _ -> Typ.Var(v) 
  [ for v, t in res -> v, substType subst t ]

// --------------------------------------------------------------------------------------
// Type checking
// --------------------------------------------------------------------------------------

let rec typeCheck fv ctx e : Tyexpr * Typ * list<Typ * Typ> =
  match e with 
  | Expr.Const(n) -> Tyexpr.Const(n), Typ.Int, []
  | Expr.Var(v) -> Tyexpr.Var(v), Map.find v ctx, []
  | Expr.App(e1, e2) -> 
      let e1, t1, cs1 = typeCheck fv ctx e1
      let e2, t2, cs2 = typeCheck fv ctx e2 
      let t3 = fv()
      Tyexpr.App(e1, e2), t3, cs1 @ cs2 @ [t1, Typ.Fun(t2, t3)]
  | Expr.Abs(v, e) ->
      let t0 = fv()
      let e, t, cs = typeCheck fv (Map.add v t0 ctx) e
      Tyexpr.Abs(v, t0, e), Fun(t0, t), cs

// --------------------------------------------------------------------------------------
// Testing...
// --------------------------------------------------------------------------------------

let rec prettify f = function 
  | Tyexpr.Abs(s, typ, te) -> Tyexpr.Abs(s, f typ, prettify f te)
  | Tyexpr.App(e1, e2) -> Tyexpr.App(prettify f e1, prettify f e2)
  | te -> te

let rec lookupTyp map = function
  | Typ.Var(v) -> 
      match Map.tryFind v map with 
      | Some t -> lookupTyp map t
      | None -> Typ.Var(v)
  | Typ.Fun(t1, t2) -> Typ.Fun(lookupTyp map t1, lookupTyp map t2)
  | Typ.Int -> Typ.Int

let test q = 
  printfn "------------------------------------"
  let fv = let c = ref 0 in fun () -> incr c; Typ.Var(sprintf "t%d" c.Value)
  let e = asExpr q
  printfn "Untyped: %O" e
  let ctx = Map.ofSeq [ "+", Typ.Fun(Typ.Int, Typ.Fun(Typ.Int, Typ.Int))]
  let te, t, cs = typeCheck fv ctx e
  printfn "Typed:   %O" te
  printfn "Typ:     %O" t
  printfn "\nConstraints:" 
  for t1, t2 in cs do printfn "  * %O = %O"  t1 t2
  printfn "\nSolutions:"
  let s = solve cs Map.empty 
  for v, t in s |> Map.toSeq do printfn "  * %s = %O" v t
  printfn "\nPretified:"
  let tep = prettify (lookupTyp s) te
  printfn "   %O" tep

test <@ fun x -> fun y -> x @>
test <@ fun f -> fun x -> f x @>
test <@ fun f -> fun g -> fun h -> (h f) + (h g) + f 1 @>

