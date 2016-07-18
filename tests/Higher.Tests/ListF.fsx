#r "bin/release/Higher.Core.dll"

open Higher.Core

type ListF<'A, 'B> = 
  | NilF
  | ConsF of 'A * 'B

type ListF private () =    
  static let token = new ListF()    
  static member Inj (value : ListF<'A, 'B>) : App2<ListF, 'A, 'B> =
    let app = new App<ListF, 'A>(token, value)
    new App2<ListF, 'A, 'B>(AppToken<ListF, 'A>.Token token, app)
  static member Prj (app2 : App2<ListF, 'A, 'B>) : ListF<'A, 'B> = 
    let app = app2.Apply(AppToken<ListF, 'A>.Token token) :?> App<ListF, 'A>
    app.Apply(token) :?> _

type ListFFunctor<'a> () =  
  inherit Functor<App<ListF, 'a>> () with
    override __.Map (f:'b -> 'c) (l:App2<ListF, 'a, 'b>) : App2<ListF, 'a, 'c> =
      match ListF.Prj l with
      | NilF -> NilF
      | ConsF (a,b) -> ConsF (a, f b) 
      |> ListF.Inj


type ListF<'a> = App<Fix, App<ListF, 'a>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ListF =

  let empty<'a> : ListF<'a> =
    NilF |> ListF.Inj |> Fix.Fix |> Fix.Inj
    
  let cons (a:'a) (l:ListF<'a>) : ListF<'a> =
    Fix.Fix (ConsF (a, Fix.Prj l) |> ListF.Inj) |> Fix.Inj
    
  let (|Nil|Cons|) (l:ListF<'a>) : Choice<unit, 'a * ListF<'a>> =
    match l |> Fix.Prj |> Fix.un |> ListF.Prj with
    | NilF -> Nil
    | ConsF (a,tl) -> Cons (a, tl |> Fix.Inj)

  let singleton (a:'a) : ListF<'a> =
    cons a empty
  
  let cata (f:ListF<'a, 'b> -> 'b) (l1:ListF<'a>) : 'b =
    Fix.cata
      (new ListFFunctor<_>())
      (ListF.Prj >> f)
      (Fix.Prj l1)

  let ana (f:'a -> ListF<'b, 'a>) : 'a -> ListF<'b> =
    Fix.ana
      (new ListFFunctor<_>())
      (f >> ListF.Inj)     
    >> Fix.Inj

  let append (l1:ListF<'a>) (l2:ListF<'a>) : ListF<'a> =
    cata (function NilF -> l2 | ConsF (a,tl) -> cons a tl) l1

  let collect<'a, 'b> (f:'a -> ListF<'b>) : ListF<'a> -> ListF<'b> =
    cata (function NilF -> empty | ConsF (a,tl) -> append (f a) tl)

  let join<'a> : ListF<ListF<'a>> -> ListF<'a> =
    cata (function
      | NilF -> empty
      | ConsF (a,tl) -> append a tl)    

  let map (f:'a -> 'b) : ListF<'a> -> ListF<'b> =
    cata (function
      | NilF -> empty
      | ConsF (a,tl) -> cons (f a) tl)

  let foldMap (M:Monoid<'M>) (f:'a -> 'M) : ListF<'a> -> 'M = 
    cata (function
      | NilF -> M.Empty
      | ConsF (a,tl) -> M.Append (f a) tl)

  let fold (f:'b -> 'a -> 'b) (z:'b) : ListF<'a> -> 'b =
    cata (function
      | NilF -> z
      | ConsF (a,tl) -> f tl a)

  let unfold (f:'s -> ('a * 's) option) : 's -> ListF<'a> =
    ana (fun s ->
      match f s with
      | None -> NilF
      | Some (a,s) -> ConsF (a, s))

  let choose (f:'a -> 'b option) : ListF<'a> -> ListF<'b> =
    cata (function
      | NilF -> empty
      | ConsF (a,tl) -> 
        match f a with 
        | Some b -> cons b tl 
        | None -> tl)

  let tails<'a> : ListF<'a> -> ListF<ListF<'a>> =
    ana (fun ls ->
      match ls with
      | Nil -> NilF
      | Cons (a,tl) -> ConsF (cons a ls, tl))

  let length<'a> : ListF<'a> -> int =
    cata (function NilF -> 0 | ConsF (_,a) -> a + 1)

  let inline sum (l:ListF<'a>) : 'a =
    l |> cata (function 
      | NilF -> LanguagePrimitives.GenericZero 
      | ConsF (a,s) -> a + s)

  let toList<'a> : ListF<'a> -> list<'a> =
    cata (function
      | NilF -> []
      | ConsF (a,tl) -> a::tl)

  let ofList<'a> : list<'a> -> ListF<'a> =
    ana (function
      | [] -> NilF
      | x::xs -> ConsF (x, xs))
