namespace Higher

/// Continuation Monad type
type Cont<'R, 'T> = C of (('T -> 'R) -> 'R)

type Cont private () =
    static let token = new Cont()
    static member Inj (value : Cont<'R, 'T>) : App2<Cont, 'R, 'T> =
        let app = new App<Cont, 'R>(token, value)
        new App2<Cont, 'R, 'T>(AppToken<Cont, 'R>.Token token, app)
    static member Prj (app2 : App2<Cont, 'R, 'T>) : Cont<'R, 'T> =
        let app = app2.Apply(AppToken<Cont, 'R>.Token token) :?> App<Cont, 'R>
        app.Apply(token) :?> _
    static member Run(cont : App2<Cont, 'R, 'T>) =
        let (C f) = Cont.Prj cont in f

/// Continuation Monad instance
type ContMonad<'R>() =
    inherit Monad<App<Cont, 'R>>() with
    override self.Return x = Cont.Inj <| C (fun k -> k x)
    override self.Bind (m, f) =
        Cont.Inj <| C (fun k ->
                            let contF = Cont.Run m
                            contF (fun x -> Cont.Run (f x) k))
