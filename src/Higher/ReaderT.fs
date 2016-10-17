namespace Higher

/// ReaderT Monad Transformer type
type ReaderT<'R, 'M, 'T> = RT of ('R -> App<'M, 'T>)

type ReaderT private () =
    static let token = new ReaderT()
    static member Inj (value : ReaderT<'R, 'M, 'T>) : App3<ReaderT, 'R, 'M, 'T> =
        let app = new App<ReaderT, 'R>(token, value)
        let app2 = new App2<ReaderT, 'R, 'M>(AppToken<ReaderT, 'R>.Token token, app)
        new App3<ReaderT, 'R, 'M, 'T>(AppToken<App<ReaderT, 'R>, 'M>.Token app, app2)
    static member Prj (app3 : App3<ReaderT, 'R, 'M, 'T>) : ReaderT<'R, 'M, 'T> =
        let token' = AppToken<ReaderT, 'R>.Token token
        let token'' = AppToken<App<ReaderT, 'R>, 'M>.Token token'
        let app2 = app3.Apply(token'') :?> App2<ReaderT, 'R, 'M>
        let app = app2.Apply(token') :?> App<ReaderT, 'R>
        app.Apply(token) :?> _
    static member Run (readerT : App3<ReaderT, 'R, 'M, 'T>) =
        let (RT reader) = ReaderT.Prj readerT in reader


/// ReaderT Monad Transformer instance
type ReaderTMonadTrans<'R>() =
    inherit MonadTrans<App<ReaderT, 'R>>() with
    override self.Lift monad m =
        ReaderT.Inj <| RT (fun _ -> monad { let! x = m in return x })


/// ReaderT Monad instance
type ReaderTMonad<'R, 'M>(monad : Monad<'M>) =
    inherit Monad<App2<ReaderT, 'R, 'M>>() with
    override self.Return x = ReaderT.Inj <| RT (fun _ -> monad { return x })
    override self.Bind (m, f) =
        ReaderT.Inj <| RT (fun env ->
                                monad {
                                    let! x = ReaderT.Run m env
                                    return! ReaderT.Run (f x) env
                                })

    member self.Get() : App3<ReaderT, 'R, 'M, 'R> =
        ReaderT.Inj <| RT (fun env -> monad { return env })
