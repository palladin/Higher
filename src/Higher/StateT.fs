namespace Higher

/// StateT Monad Transformer type
type StateT<'S, 'M, 'T> = ST of ('S -> App<'M, ('T * 'S)>)

type StateT private () =
    static let token = new StateT()
    static member Inj (value : StateT<'S, 'M, 'T>) : App3<StateT, 'S, 'M, 'T> =
        App3<StateT, 'S, 'M, 'T>.Create(AppToken2<StateT, 'S, 'M>.Token(token), value)
    static member Prj (app3 : App3<StateT, 'S, 'M, 'T>) : StateT< 'S, 'M, 'T> =
        app3.Apply(AppToken2<StateT, 'S, 'M>.Token(token)) :?> _
    static member Run (stateT : App3<StateT, 'S, 'M, 'T>) =
        let (ST state) = StateT.Prj stateT  in state


/// StateT Monad Transformer instance
type StateTMonadTrans<'S>() =
    inherit MonadTrans<App<StateT, 'S>>() with
    override self.Lift monad m =
        StateT.Inj <| ST (fun s -> monad { let! x = m in return (x, s) })


/// StateT Monad instance
type StateTMonad<'S, 'M>(monad : Monad<'M>) =
    inherit Monad<App2<StateT, 'S, 'M>>() with
    override self.Return x = StateT.Inj <| ST (fun s -> monad { return (x, s) })
    override self.Bind (m, f) =
        StateT.Inj <| ST (fun s ->
                                monad {
                                    let! (x, s') = StateT.Run m s
                                    return! StateT.Run (f x) s'
                                })

    member self.Get() : App3<StateT, 'S, 'M, 'S> =
        StateT.Inj <| ST (fun s -> monad { return (s, s) })
    member self.Put (newState : 'S) : App3<StateT, 'S, 'M, unit> =
        StateT.Inj <| ST (fun _ -> monad { return ((), newState) })
