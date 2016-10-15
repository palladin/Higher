namespace Higher

/// type Ran g h α = R (∀β. (α → g β) → h β)
///
/// type Codensity m a = Ran m m a
///
/// type Yoneda f a = Ran Id f a
type Ran<'G, 'H, 'A> =
    abstract Invoke<'B> : ('A -> App<'G, 'B>) -> App<'H, 'B>


type Ran private () =
    static let token = new Ran()
    static member Inj (value : Ran<'G, 'H, 'A>) : App3<Ran, 'G, 'H, 'A> =
        let app = new App<Ran, 'G>(token, value)
        let app2 = new App2<Ran, 'G, 'H>(AppToken<Ran, 'G>.Token token, app)
        new App3<Ran, 'G, 'H, 'A>(AppToken<App<Ran, 'G>, 'H>.Token app, app2)
    static member Prj (app3 : App3<Ran, 'G, 'H, 'A>) : Ran<'G, 'H, 'A> =
        let token' = AppToken<Ran, 'G>.Token token
        let token'' = AppToken<App<Ran, 'G>, 'H>.Token token'
        let app2 = app3.Apply(token'') :?> App2<Ran, 'G, 'H>
        let app = app2.Apply(token') :?> App<Ran, 'G>
        app.Apply(token) :?> _

type RanFunctor<'G, 'H>() =
    inherit Functor<App2<Ran, 'G, 'H>>()

    override self.Map (f : 'A -> 'B) (c : App3<Ran, 'G, 'H, 'A>) =
        Ran.Inj <|
            { new Ran<'G, 'H, 'B> with
                member self.Invoke<'R> (k : 'B -> App<'G, 'R>) =
                    (Ran.Prj c).Invoke (k << f) }
