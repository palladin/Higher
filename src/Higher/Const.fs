namespace Higher

type Const<'A, 'B> = C of 'A

type Const private () =
  static let token = Const ()
  static member Inj (value : Const<'A, 'B>) : App2<Const, 'A, 'B> =
    App2<Const, 'A, 'B>.Create(AppToken<Const, 'A>.Token(token), value)
  static member Prj (app : App2<Const, 'A, 'B>) : Const<'A, 'B> =
    app.Apply(AppToken<Const, 'A>.Token(token)) :?> _

type ConstFunctor<'K>() =
  inherit Functor<App<Const, 'K>>() with
    override self.Map (_ : 'A -> 'B) (app : App2<Const, 'K, 'A>) : App2<Const, 'K, 'B> =
      let (C k) = app |> Const.Prj
      (C k) |> Const.Inj
