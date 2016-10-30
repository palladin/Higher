namespace Higher

type Kleisli<'M, 'A, 'B> = K of ('A -> App<'M, 'B>)

type Kleisli private () =
    static let token = new Kleisli()
    static member Inj (value : Kleisli<'M, 'A, 'B>) : App3<Kleisli, 'M, 'A, 'B> =
        App3<Kleisli, 'M, 'A, 'B>.Create(AppToken2<Kleisli, 'M, 'A>.Token(token), value)
    static member Prj (app3 : App3<Kleisli, 'M, 'A, 'B>) : Kleisli< 'M, 'A, 'B> =
        app3.Apply(AppToken2<Kleisli, 'M, 'A>.Token(token)) :?> _
    static member Run (app : App3<Kleisli, 'M, 'A, 'B>) =
        let (K f) = Kleisli.Prj app in f


type KleisliArrow<'M>(monad : Monad<'M>) =
    inherit Arrow<App<Kleisli, 'M>>()
    override self.Arr(f : ('A -> 'B)) : App2<App<Kleisli, 'M>, 'A, 'B> =
        Kleisli.Inj <| K (fun a -> monad { return f a })
    override self.First(f : App2<App<Kleisli, 'M>, 'A, 'B>) : App2<App<Kleisli, 'M>, 'A * 'C, 'B * 'C> =
        let (K f') = Kleisli.Prj f
        Kleisli.Inj <| K (fun (a, c) -> monad { let! b = f' a in return (b, c) })
    override self.Ident<'A>() : App2<App<Kleisli, 'M>, 'A, 'A> =
        Kleisli.Inj <| K (fun a -> monad { return a })
    override self.Compose(f : App2<App<Kleisli, 'M>, 'A, 'B>) (g : App2<App<Kleisli, 'M>, 'B, 'C>) : App2<App<Kleisli, 'M>, 'A, 'C> =
        let (K f') = Kleisli.Prj f
        let (K g') = Kleisli.Prj g
        Kleisli.Inj <| K (Monad.compose monad f' g')
