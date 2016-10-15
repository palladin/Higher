namespace Higher

type Flip<'F, 'A, 'B> = F of App2<'F, 'B, 'A>

type Flip private () =
    static let token = new Flip()
    static member Inj (value : Flip<'F, 'A, 'B>) : App3<Flip, 'F, 'A, 'B> =
        let app = new App<Flip, 'F>(token, value)
        let app2 = new App2<Flip, 'F, 'A>(AppToken<Flip, 'F>.Token token, app)
        new App3<Flip, 'F, 'A, 'B>(AppToken<App<Flip, 'F>, 'A>.Token app, app2)
    static member Prj (app3 : App3<Flip, 'F, 'A, 'B>) : Flip<'F, 'A, 'B> =
        let token' = AppToken<Flip, 'F>.Token token
        let token'' = AppToken<App<Flip, 'F>, 'A>.Token token'
        let app2 = app3.Apply(token'') :?> App2<Flip, 'F, 'A>
        let app = app2.Apply(token') :?> App<Flip, 'F>
        app.Apply(token) :?> _
    static member Run (app3 : App3<Flip, 'F, 'A, 'B>) : App2<'F, 'B, 'A> =
        let (F app) = Flip.Prj app3 in app
