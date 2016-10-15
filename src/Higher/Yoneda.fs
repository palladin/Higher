namespace Higher

/// type Yoneda f α = Y (∀β. (α → β) → f β)
type Yoneda<'F, 'A> =
    abstract Invoke<'B> : ('A -> 'B) -> App<'F, 'B>


type Yoneda private () =
    static let token = new Yoneda()
    static member Inj (value : Yoneda<'F, 'A>) : App2<Yoneda, 'F, 'A> =
        let app = new App<Yoneda, 'F>(token, value)
        new App2<Yoneda, 'F, 'A>(AppToken<Yoneda, 'F>.Token token, app)
    static member Prj (app2 : App2<Yoneda, 'F, 'A>) : Yoneda<'F, 'A> =
        let app = app2.Apply(AppToken<Yoneda, 'F>.Token token) :?> App<Yoneda, 'F>
        app.Apply(token) :?> _


type YonedaFunctor<'F>() =
    inherit Functor<App<Yoneda, 'F>>()

    override self.Map (f : 'A -> 'B) (c : App2<Yoneda, 'F, 'A>) =
        Yoneda.Inj <|
            { new Yoneda<'F, 'B> with
                member self.Invoke<'R> (k : 'B -> 'R) =
                    (Yoneda.Prj c).Invoke (k << f) }
