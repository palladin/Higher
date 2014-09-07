namespace Higher.Core

// StateT Monad Transformer type
type StateT<'S, 'M, 'T> = ST of ('S -> App<'M, ('T * 'S)>)
type StateT private () =    
    static let token = new StateT()
    static member Inj (value : StateT<'S, 'M, 'T>) : App3<StateT, 'S, 'M, 'T> = 
        let app = new App<StateT, 'S>(token, value)
        let app2 = new App2<StateT, 'S, 'M>(AppToken<StateT, 'S>.Token token, app)
        new App3<StateT, 'S, 'M, 'T>(AppToken<App<StateT, 'S>, 'M>.Token app, app2)
    static member Prj (app3 : App3<StateT, 'S, 'M, 'T>) : StateT< 'S, 'M, 'T> =
        let token' = AppToken<StateT, 'S>.Token token
        let token'' = AppToken<App<StateT, 'S>, 'M>.Token token'
        let app2 = app3.Apply(token'') :?> App2<StateT, 'S, 'M>
        let app = app2.Apply(token') :?> App<StateT, 'S>
        app.Apply(token) :?> _
    static member UnWrap (stateT : StateT<'S, 'M, 'T>) = 
        let (ST state) = stateT  in state



// StateT Monad Transformer instance
type StateTMonadTrans<'S>() = 
    inherit MonadTrans<App<StateT, 'S>>() with
    override self.Lift monad m = 
        StateT.Inj <| ST (fun s -> monad { let! x = m in return (x, s) })


// StateT Monad instance
type StateTMonad<'S, 'M>(monad : Monad<'M>) = 
    inherit Monad<App2<StateT, 'S, 'M>>() with
    override self.Return x = StateT.Inj <| ST (fun s -> monad { return (x, s) })
    override self.Bind (m, f) = 
        StateT.Inj <| ST (fun s -> 
                                monad {
                                    let! (x, s') = StateT.UnWrap (StateT.Prj m) s 
                                    return! StateT.UnWrap (StateT.Prj (f x)) s'
                                })

    member self.Get() : App3<StateT, 'S, 'M, 'S> = 
        StateT.Inj <| ST (fun s -> monad { return (s, s) })
    member self.Put (newState : 'S) : App3<StateT, 'S, 'M, unit> = 
        StateT.Inj <| ST (fun s -> monad { return ((), newState) })
