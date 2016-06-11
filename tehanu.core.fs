namespace Tehanu.Core           

type Result<'a> =
  | Success of 'a
  | Nothing
  | Fail of 'a
  | Hint of string * Result<'a>

  override this.ToString() =
    match this with                           
    | Success x -> "success :: " + x.ToString()
    | Nothing   -> "nothing"
    | Fail    x ->    "fail :: " + x.ToString()
    | Hint (s,x)->    "hint :: " + s + "\n" + x.ToString() 

  member this.CombineHints(self: Result<'b>, result: Result<'c>) =
    match this with
    | Hint (x, xs) -> Hint (x, xs.CombineHints(self, result))
    | _ ->
      let rec getHints s =
        match s with
        | Hint (x, xs) -> Hint (x, getHints xs)
        | _ -> result
      getHints self

  member this.GetResult(): option<'a> =
    match this with 
    | Success a | Fail a -> Some(a)
    | Nothing -> None
    | Hint (_, r) -> r.GetResult()

  member this.AndResult(self: Result<'b>, f: option<'a> * option<'b> -> option<'c>, ispos: bool): Result<'c> =
    match this with
    | Hint (x, xs) -> Hint (x, xs.AndResult(self, f, ispos))
    | Nothing ->
      let rec getHints s =
        match s with
        | Hint (x, xs) -> Hint (x, getHints xs)
        | Success rs ->
          match f (None, Some (rs)) with   
          | None -> Nothing         
          | Some (c) -> if ispos then Success c else Fail c
        | Fail rs ->
          match f (None, Some (rs)) with   
          | None -> Nothing                                
          | Some (c) -> Fail c
        | Nothing ->
          match f (None, None) with   
          | None -> Nothing
          | Some (c) -> if ispos then Success c else Fail c
      getHints self    
    | Success rt ->
      let rec getHints s =
        match s with
        | Hint (x, xs) -> Hint (x, getHints xs)
        | Success rs ->
          match f (Some (rt), Some (rs)) with   
          | None -> Nothing
          | Some (c) -> Success c
        | Fail rs ->
          match f (Some (rt), Some (rs)) with   
          | None -> Nothing
          | Some (c) -> Fail c 
        | Nothing ->
          match f (Some (rt), None) with   
          | None -> Nothing             
          | Some (c) -> if ispos then Success c else Fail c
      getHints self
    | Fail rt ->
      let rec getHints s =
        match s with
        | Hint (x, xs) -> Hint (x, getHints xs)   
        | Success rs | Fail rs ->
          match f (Some (rt), Some (rs)) with   
          | None -> Nothing
          | Some (c) -> Fail c 
        | Nothing ->
          match f (Some (rt), None) with   
          | None -> Nothing
          | Some (c) -> if ispos then Success c else Fail c
      getHints self  
                                   
type Tree =
  | Pair of ref<Tree> * ref<Tree>
  | Atom of string
  | Error of string
  
  override this.ToString() =
    match this with
    | Error t -> "#(error " + t + ")#"
    | Atom "nil" -> "nil"
    | Atom text -> text
    | Pair (x, y) ->
      match !y with
      | Atom "nil" ->
        "(" + (string !x) + ")"
      | Pair (_, _) ->
        let ystr = string !y
        "(" + (string !x) + " " + (ystr.Substring(1, ystr.Length - 1))
      | Atom text -> "(" + (string !x) + "." + text + ")"
              
module Generators =
  let rec genList (xs: list<Tree>): Tree =
    match xs with
    | [] -> Atom "nil"
    | x::s -> Pair(ref x, ref <| genList s)

  let genId (name: string): Tree =
    Atom <| "``" + name + "``"
                         
  let genInt (value: int): Tree =
    Atom <| string value

  let genString (value: string): Tree =
    Atom <| "\"" + value + "\""

  let genModule (name: string) (moduleitems: Tree): Tree =
    genList [Atom "module"; genId name; moduleitems]

  let genLet (attrs: Tree) (name: string) (argss: Tree) (expr: Tree): Tree =
    genList [attrs; Atom "let"; genId name; argss; expr]

  let genArg (name: string) (typ: Tree): Tree =
    genList [Atom "arg"; genId name; typ]

  let genAttr (name: string) (expr: Tree): Tree =
    genList [Atom "attr"; genId name; expr]

  let genExprIf (cond: Tree) (th: Tree) (el: Tree): Tree =
    genList [Atom "expr"; Atom "if"; cond; th; el]

  let genExprApp (left: Tree) (right: Tree): Tree =
    genList [Atom "expr"; Atom "app"; left; right]

  let genExprId (name: string): Tree =
    genList [Atom "expr"; Atom "id"; genId name]

  let genTypeId (name: string): Tree =
    genList [Atom "type"; Atom "id"; genId name]
                                                                    
  let genExprConstInt (value: int): Tree =
    genList [Atom "expr"; Atom "const"; Atom "int"; genInt value]

  let genExprConstString (value: string): Tree =
    genList [Atom "expr"; Atom "const"; Atom "string"; genString value]

  let genExprConstUnit (): Tree =
    genList [Atom "expr"; Atom "const"; Atom "unit"]

  let genExprForall (names: Tree) (expr: Tree): Tree =
    genList [Atom "expr"; Atom "forall"; names; expr]

  let genExprExist (names: Tree) (expr: Tree): Tree =
    genList [Atom "expr"; Atom "exist"; names; expr]

  let genExprTyped (expr: Tree) (typ: Tree): Tree =
    genList [Atom "expr"; Atom "typed"; expr; typ]

module Patterns =
  let rec (|List|_|) (tree: Tree): option<list<Tree>> =
    match tree with
    | Atom "nil" -> Some([])
    | Pair(l, r) ->
      match !r with
      | List s -> Some(!l::s)
      | _ -> None
    | _ -> None
                
  let (|Id|_|) (tree: Tree): option<string> =
    match tree with
    | Atom text when (text.Length >= 4 &&
                      text.[0] = '`' &&
                      text.[1] = '`' &&
                      text.[text.Length - 2] = '`' &&
                      text.[text.Length - 1] = '`') ->
      Some(text.Substring(2, text.Length - 4))
    | _ -> None

  let (|Str|_|) (tree: Tree): option<string> =
    match tree with
    | Atom text when (text.Length >= 2 &&
                      text.[0] = '"' &&                
                      text.[text.Length - 1] = '"') ->
      Some(text.Substring(1, text.Length - 2))
    | _ -> None

  let (|Int|_|) (tree: Tree): option<int> =
    let value = ref 0
    match tree with
    | Atom text when System.Int32.TryParse(text, value) ->
      Some(!value)
    | _ -> None

  let (|Module|_|) (tree: Tree): option<string * Tree> =
    match tree with
    | List ((Atom "module") :: (Id name) :: mits :: _) ->
      Some((name, mits))
    | _ -> None

  let (|Arg|_|) (tree: Tree): option<string * Tree> =
    match tree with
    | List ((Atom "arg") :: (Id name) :: typ :: _) ->
      Some((name, typ))
    | _ -> None

  let (|Attr|_|) (tree: Tree): option<string * Tree> =
    match tree with
    | List ((Atom "attr") :: (Id name) :: expr :: _) ->
      Some((name, expr))
    | _ -> None

  let (|Let|_|) (tree: Tree): option<Tree * string * Tree * Tree> =
    match tree with
    | List (attrs :: (Atom "let") :: (Id name) :: argss :: expr :: _) ->
      Some((attrs, name, argss, expr))
    | _ -> None

  let (|TypeId|_|) (tree: Tree): option<string> =
    match tree with
    | List ((Atom "type") :: (Atom "id") :: (Id name) :: _) ->
      Some(name)
    | _ -> None

  let (|ExprId|_|) (tree: Tree): option<string> =
    match tree with
    | List ((Atom "expr") :: (Atom "id") :: (Id name) :: _) ->
      Some(name)
    | _ -> None
                                                 
  let (|ExprConstInt|_|) (tree: Tree): option<int> =
    match tree with
    | List ((Atom "expr") :: (Atom "const") :: (Atom "int") :: (Int value) :: _) ->
      Some(value)
    | _ -> None

  let (|ExprConstString|_|) (tree: Tree): option<string> =
    match tree with
    | List ((Atom "expr") :: (Atom "const") :: (Atom "string") :: (Str value) :: _) ->
      Some(value)
    | _ -> None

  let (|ExprConstUnit|_|) (tree: Tree): option<unit> =
    match tree with
    | List ((Atom "expr") :: (Atom "const") :: (Atom "unit") :: _) ->
      Some(())
    | _ -> None

  let (|ExprIf|_|) (tree: Tree): option<Tree * Tree * Tree> =
    match tree with
    | List ((Atom "expr") :: (Atom "if") :: cond :: th :: el :: _) ->
      Some((cond, th, el))
    | _ -> None

  let (|ExprApp|_|) (tree: Tree): option<Tree * Tree> =
    match tree with
    | List ((Atom "expr") :: (Atom "app") :: left :: right :: _) ->
      Some((left, right))
    | _ -> None

  let (|ExprForall|_|) (tree: Tree): option<Tree * Tree> =
    match tree with
    | List ((Atom "expr") :: (Atom "forall") :: names :: expr :: _) ->
      Some((names, expr))
    | _ -> None

  let (|ExprExist|_|) (tree: Tree): option<Tree * Tree> =
    match tree with
    | List ((Atom "expr") :: (Atom "exist") :: names :: expr :: _) ->
      Some((names, expr))
    | _ -> None

  let (|ExprTyped|_|) (tree: Tree): option<Tree * Tree> =
    match tree with
    | List ((Atom "expr") :: (Atom "typed") :: expr :: typ :: _) ->
      Some((expr, typ))
    | _ -> None
