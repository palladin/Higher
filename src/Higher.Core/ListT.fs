namespace Higher.Core

// ListT Monad Transformer type
type ListT<'M, 'T> = OT of App<'M, 'T list>
type ListT private () =    
    static let token = new ListT()
    static member Inj (value : ListT<'M, 'T>) : App2<ListT, 'M, 'T> = 
        let app = new App<ListT, 'M>(token, value)
        new App2<ListT, 'M, 'T>(AppToken<ListT, 'M>.Token token, app)
    static member Prj (app2 : App2<ListT, 'M, 'T>) : ListT<'M, 'T> = 
        let app = app2.Apply(AppToken<ListT, 'M>.Token token) :?> App<ListT, 'M>
        app.Apply(token) :?> _
    static member Run (listT : App2<ListT, 'M, 'T>) = 
        let (OT appList) = ListT.Prj listT in appList



// ListT Monad Transformer instance
type ListTMonadTrans() = 
    inherit MonadTrans<ListT>() with
    override self.Lift monad m = 
        ListT.Inj <| OT (monad { let! x = m in return [x] })


// ListT Monad instance
type ListTMonad<'M>(monad : Monad<'M>) = 
    inherit Monad<App<ListT, 'M>>() with
    override self.Return x = ListT.Inj <| OT (monad { return [x] })
    override self.Bind (m, f) = 
        ListT.Inj <| OT (monad {    
                              let! xs = ListT.Run m
                              let! yss = Monad.mapM monad (ListT.Run << f) xs 
                              return List.concat yss
                           })
