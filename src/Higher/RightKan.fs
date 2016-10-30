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
        App3<Ran, 'G, 'H, 'A>.Create(AppToken2<Ran, 'G, 'H>.Token(token), value)
    static member Prj (app3 : App3<Ran, 'G, 'H, 'A>) : Ran<'G, 'H, 'A> =
        app3.Apply(AppToken2<Ran, 'G, 'H>.Token(token)) :?> _

type RanFunctor<'G, 'H>() =
    inherit Functor<App2<Ran, 'G, 'H>>()

    override self.Map (f : 'A -> 'B) (c : App3<Ran, 'G, 'H, 'A>) =
        Ran.Inj <|
            { new Ran<'G, 'H, 'B> with
                member self.Invoke<'R> (k : 'B -> App<'G, 'R>) =
                    (Ran.Prj c).Invoke (k << f) }
