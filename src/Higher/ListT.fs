namespace Higher

/// ListT Monad Transformer type
type ListT<'M, 'T> = LT of App<'M, 'T list>

type ListT private () =
    static let token = new ListT()
    static member Inj (value : ListT<'M, 'T>) : App2<ListT, 'M, 'T> =
        App2<ListT, 'M, 'T>.Create(AppToken<ListT, 'M>.Token(token), value)
    static member Prj (app2 : App2<ListT, 'M, 'T>) : ListT<'M, 'T> =
        app2.Apply(AppToken<ListT, 'M>.Token(token)) :?> _
    static member Run (listT : App2<ListT, 'M, 'T>) =
        let (LT appList) = ListT.Prj listT in appList


/// ListT Monad Transformer instance
type ListTMonadTrans() =
    inherit MonadTrans<ListT>() with
    override self.Lift monad m =
        ListT.Inj <| LT (monad { let! x = m in return [x] })


/// ListT Monad instance
type ListTMonad<'M>(monad : Monad<'M>) =
    inherit Monad<App<ListT, 'M>>() with
    override self.Return x = ListT.Inj <| LT (monad { return [x] })
    override self.Bind (m, f) =
        ListT.Inj <| LT (monad {
                              let! xs = ListT.Run m
                              let! yss = Monad.mapM monad (ListT.Run << f) xs
                              return List.concat yss
                           })
