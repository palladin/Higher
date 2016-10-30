namespace Higher

/// Continuation Monad Transformer type
type ContT<'R, 'M, 'T> = CT of (('T -> App<'M, 'R>) -> App<'M, 'R>)

type ContT private () =
    static let token = new ContT()
    static member Inj (value : ContT<'R, 'M, 'T>) : App3<ContT, 'R, 'M, 'T> =
        App3<ContT, 'R, 'M, 'T>.Create(AppToken2<ContT, 'R, 'M>.Token(token), value)
    static member Prj (app3 : App3<ContT, 'R, 'M, 'T>) : ContT<'R, 'M, 'T> =
        app3.Apply(AppToken2<ContT, 'R, 'M>.Token(token)) :?> _
    static member Run (contT : App3<ContT, 'R, 'M, 'T>) =
        let (CT cont) = ContT.Prj contT in cont


/// ContT Monad Transformer instance
type ContTMonadTrans<'R>() =
    inherit MonadTrans<App<ContT, 'R>>() with
    override self.Lift monad m =
        ContT.Inj <| CT (fun k -> monad { let! x = m in return! k x })


/// ContT Monad instance
type ContTMonad<'R, 'M>(monad : Monad<'M>) =
    inherit Monad<App2<ContT, 'R, 'M>>() with
    override self.Return x = ContT.Inj <| CT (fun k -> monad { return! k x })
    override self.Bind (m, f) =
        ContT.Inj <| CT (fun k ->
                                let cont = ContT.Run m
                                cont (fun x ->
                                        monad {
                                            return! ContT.Run (f x) k
                                        }))
