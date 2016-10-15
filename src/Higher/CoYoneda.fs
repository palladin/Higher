namespace Higher

type Lambda<'F, 'A, 'R> =
    abstract Invoke<'B> : ('B -> 'A) -> App<'F, 'B> -> 'R

/// type CoYoneda f α = ∃β. (β → α) * f β
[<AbstractClass>]
type CoYoneda<'F, 'A>() =
    abstract Invoke<'R> : Lambda<'F, 'A, 'R> -> 'R

type CoYonedaConstr<'F, 'B, 'A>(f : 'B -> 'A, app : App<'F, 'B>) =
    inherit CoYoneda<'F, 'A>()
    override self.Invoke lambda =
        lambda.Invoke f app


type CoYoneda private () =
    static let token = new CoYoneda()
    static member Inj (value : CoYoneda<'F, 'A>) : App2<CoYoneda, 'F, 'A> =
        let app = new App<CoYoneda, 'F>(token, value)
        new App2<CoYoneda, 'F, 'A>(AppToken<CoYoneda, 'F>.Token token, app)
    static member Prj (app2 : App2<CoYoneda, 'F, 'A>) : CoYoneda<'F, 'A> =
        let app = app2.Apply(AppToken<CoYoneda, 'F>.Token token) :?> App<CoYoneda, 'F>
        app.Apply(token) :?> _


type CoYonedaFunctor<'F>() =
    inherit Functor<App<CoYoneda, 'F>>()

    override self.Map (f : 'A -> 'B) (app : App2<CoYoneda, 'F, 'A>) =
        CoYoneda.Inj <|
            (CoYoneda.Prj app).Invoke<CoYoneda<'F, 'B>>
                { new Lambda<'F, 'A, CoYoneda<'F, 'B>> with
                    member self.Invoke<'C> (k : 'C -> 'A) (app : App<'F, 'C>) =
                        new CoYonedaConstr<'F, 'C, 'B>(k >> f, app) :> CoYoneda<'F, 'B> }
