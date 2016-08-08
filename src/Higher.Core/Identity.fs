namespace Higher.Core

type Identity<'A> = Id of 'A

type Identity private () =  
  static let token = Identity()
  static member inline un (Id(a)) = a
  static member Inj (value : Identity<'A>) : App<Identity, 'A> =
    App<Identity, 'A>(token, value)
  static member Prj (app : App<Identity, 'A>) : Identity<'A> =
    app.Apply(token) :?> _
  static member Run (app : App<Identity, 'A>) : 'A =
    let (Id a) = Identity.Prj app
    a
    
type IdentityFunctor() =
  inherit Functor<Identity>() with
    override self.Map (f : 'A -> 'B) (app : App<Identity, 'A>) : App<Identity, 'B> =
      app |> Identity.Prj |> Identity.un |> f |> Id |> Identity.Inj

type IdentityApplicative() =
  inherit Applicative<Identity>() with
    override self.Pure (a : 'A) : App<Identity, 'A> =
      Id a |> Identity.Inj
    override self.Apply (fab : App<Identity, 'A -> 'B>) (a : App<Identity, 'A>) : App<Identity, 'B> =
      let f = fab |> Identity.Prj |> Identity.un in
      f (Identity.Prj a |> Identity.un) |> Id |> Identity.Inj

type IdentityMonad() =
  inherit Monad<Identity>() with
    override self.Return (a : 'A) : App<Identity, 'A> =
      Id a |> Identity.Inj
    override self.Bind (ma : App<Identity, 'A>, f : 'A -> App<Identity, 'B>) : App<Identity, 'B> =
      ma |> Identity.Prj |> Identity.un |> f

type IdentityComonad() =
  inherit Comonad<Identity>() with
    override self.Extract (app : App<Identity, 'A>) : 'A =
      app |> Identity.Prj |> Identity.un
    override self.Extend (f : App<Identity, 'A> -> 'B) (app : App<Identity, 'A>) : App<Identity, 'B> =
      app |> f |> Id |> Identity.Inj