namespace Higher

type Coproduct<'F,'G,'A> = Cp of Choice<App<'F,'A>,App<'G,'A>>

type Coproduct private () =
  static let token = new Coproduct()

  static member Inj (value: Coproduct<'F,'G,'A>) : App3<Coproduct,'F,'G,'A> =
    App3<Coproduct,'F,'G,'A>.Create(AppToken2<Coproduct,'F,'G>.Token(token), value)
  static member Prj (app3: App3<Coproduct,'F,'G,'A>) : Coproduct<'F,'G,'A> =
    app3.Apply(AppToken2<Coproduct,'F,'G>.Token(token)) :?> _


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Coproduct =

  let inline coproduct
    (f: App<'F,'A> -> 'B)
    (g: App<'G,'A> -> 'B)
    (cp: App3<Coproduct,'F,'G,'A>) : 'B =

    match Coproduct.Prj cp with
    | Cp (Choice1Of2 x) -> f x
    | Cp (Choice2Of2 x) -> g x

  let inline left
    (fa: App<'F,'A>)
    : App3<Coproduct,'F,'G,'A> =

    Coproduct.Inj << Cp << Choice1Of2 <| fa

  let inline right
    (ga: App<'G,'A>)
    : App3<Coproduct,'F,'G,'A> =
    Coproduct.Inj << Cp << Choice2Of2 <| ga


type CoproductFunctor<'F,'G>
  (functorF: Functor<'F>, functorG: Functor<'G>) =

  inherit Functor<App2<Coproduct,'F,'G>>()

  override self.Map
    (f: 'A -> 'B)
    (app: App3<Coproduct, 'F, 'G, 'A>)
    : App3<Coproduct,'F,'G,'B> =

    Coproduct.coproduct
      (Coproduct.left   << functorF.Map f)
      (Coproduct.right  << functorG.Map f)
      app


type CoproductTraversable<'F,'G>
  (travF: Traversable<'F>, travG: Traversable<'G>) =

  inherit Traversable<App2<Coproduct,'F,'G>>()

  override self.Traverse<'H, 'T, 'R>
    (app : Applicative<'H>)
    (f : 'T -> App<'H, 'R>)
    (trav : App3<Coproduct,'F,'G,'T>)
    : App<'H,App3<Coproduct,'F,'G,'R>> =

    Coproduct.coproduct
      (app.Map Coproduct.left   << travF.Traverse app f)
      (app.Map Coproduct.right  << travG.Traverse app f)
      trav


type CoproductComonad<'F,'G>
  (comonadF: Comonad<'F>, comonadG: Comonad<'G>) =

  inherit Comonad<App2<Coproduct,'F,'G>>()

  override self.Extend
      (f: App3<Coproduct,'F,'G,'A> -> 'B)
      (cp:App3<Coproduct,'F,'G,'A>)
      : App3<Coproduct,'F,'G,'B> =

    Coproduct.coproduct
      (Coproduct.left   << comonadF.Extend (f << Coproduct.left))
      (Coproduct.right  << comonadG.Extend (f << Coproduct.right))
      cp

  override self.Extract
    (cp: App3<Coproduct,'F,'G,'A>)
    : 'A =

    Coproduct.coproduct comonadF.Extract comonadG.Extract cp


type CoproductContraFunctor<'F,'G>
  (contraF : ContraFunctor<'F>, contraG : ContraFunctor<'G>) =

  inherit ContraFunctor<App2<Coproduct,'F,'G>>()

  override self.ContraMap
    (f : 'A -> 'B)
    (cp : App3<Coproduct,'F,'G,'B>)
    : App3<Coproduct,'F,'G,'A> =
    Coproduct.coproduct
      (Coproduct.left << contraF.ContraMap f)
      (Coproduct.right << contraG.ContraMap f)
      cp
