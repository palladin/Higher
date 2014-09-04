namespace Higher.Core

// Continuation Monad Transformer type
type ContT<'R, 'M, 'T> = CT of (('T -> App<'M, 'R>) -> App<'M, 'R>) 
type ContT private () =    
    static let token = new ContT()
    static member Inj (value : ContT<'R, 'M, 'T>) : App3<ContT, 'R, 'M, 'T> = 
        let app = new App<ContT, 'R>(token, value)
        let app2 = new App2<ContT, 'R, 'M>(AppToken<ContT, 'R>.Token token, app)
        new App3<ContT, 'R, 'M, 'T>(AppToken<App<ContT, 'R>, 'M>.Token app, app2)
    static member Prj (app3 : App3<ContT, 'R, 'M, 'T>) : ContT<'R, 'M, 'T> =
        let token' = AppToken<ContT, 'R>.Token token
        let token'' = AppToken<App<ContT, 'R>, 'M>.Token token'
        let app2 = app3.Apply(token'') :?> App2<ContT, 'R, 'M>
        let app = app2.Apply(token') :?> App<ContT, 'R>
        app.Apply(token) :?> _
    static member UnWrap (contT : ContT<'R, 'M, 'T>) = 
        let (CT cont) = contT in cont



// ContT Monad Transformer instance
type ContTMonadTrans<'R>() = 
    inherit MonadTrans<App<ContT, 'R>>() with
    override self.Lift monad m = 
        ContT.Inj <| CT (fun k -> monad { let! x = m in return! k x })


// ContT Monad instance
type ContTMonad<'R, 'M>(monad : Monad<'M>) = 
    inherit Monad<App2<ContT, 'R, 'M>>() with
    override self.Return x = ContT.Inj <| CT (fun k -> monad { return! k x })
    override self.Bind (m, f) = 
        ContT.Inj <| CT (fun k ->
                                let cont = ContT.UnWrap (ContT.Prj m) 
                                cont (fun x -> 
                                        monad {
                                            let cont' = ContT.UnWrap (ContT.Prj (f x))
                                            return! cont' k
                                        })) 