namespace Higher

/// OptionT Monad Transformer type
type OptionT<'M, 'T> = OT of App<'M, 'T option>

type OptionT private () =
    static let token = new OptionT()
    static member Inj (value : OptionT<'M, 'T>) : App2<OptionT, 'M, 'T> =
        App2<OptionT, 'M, 'T>.Create(AppToken<OptionT, 'M>.Token(token), value)
    static member Prj (app2 : App2<OptionT, 'M, 'T>) : OptionT<'M, 'T> =
        app2.Apply(AppToken<OptionT, 'M>.Token(token)) :?> _
    static member Run (optionT : App2<OptionT, 'M, 'T>) =
        let (OT appOption) = OptionT.Prj optionT in appOption


/// OptionT Monad Transformer instance
type OptionMonadTrans() =
    inherit MonadTrans<OptionT>() with
    override self.Lift monad m =
        OptionT.Inj <| OT (monad { let! x = m in return Some x })


/// OptionT Monad instance
type OptionTMonad<'M>(monad : Monad<'M>) =
    inherit Monad<App<OptionT, 'M>>() with
    override self.Return x = OptionT.Inj <| OT (monad { return Some x })
    override self.Bind (m, f) =
        OptionT.Inj <| OT (monad {
                              let! option = OptionT.Run m
                              match option with
                              | Some x -> return! OptionT.Run <| f x
                              | None -> return None
                           })
