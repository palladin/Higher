namespace Higher


/// Function type
type Fun private () =
    static let token = new Fun()
    static member Inj (value : 'A -> 'B) : App2<Fun, 'A, 'B> =
        let app = new App<Fun, 'A>(token, value)
        new App2<Fun, 'A, 'B>(AppToken<Fun, 'A>.Token token, app)
    static member Prj (app2 : App2<Fun, 'A, 'B>) : 'A -> 'B =
        let app = app2.Apply(AppToken<Fun, 'A>.Token token) :?> App<Fun, 'A>
        app.Apply(token) :?> _


/// Function Functor
type FunFunctor<'E>() =
    inherit Functor<App<Fun, 'E>>()
    override self.Map (f : 'A -> 'B) (v : App2<Fun, 'E, 'A>) : App2<Fun, 'E, 'B> =
        Fun.Inj <| fun e -> f (Fun.Prj v e)

/// Function Category instance
type FunCategory() =
    inherit Category<Fun>() with
    override self.Ident() = Fun.Inj id
    override self.Compose f g =
        Fun.Inj (fun x -> Fun.Prj g (Fun.Prj f x))
