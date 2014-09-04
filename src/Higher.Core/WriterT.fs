namespace Higher.Core

// WriterT Monad Transformer type
type WriterT<'W, 'M, 'T> = WT of App<'M, ('T * 'W)>
type WriterT private () =    
    static let token = new WriterT()
    static member Inj (value : WriterT<'W, 'M, 'T>) : App3<WriterT, 'W, 'M, 'T> = 
        let app = new App<WriterT, 'W>(token, value)
        let app2 = new App2<WriterT, 'W, 'M>(AppToken<WriterT, 'W>.Token token, app)
        new App3<WriterT, 'W, 'M, 'T>(AppToken<App<WriterT, 'W>, 'M>.Token app, app2)
    static member Prj (app3 : App3<WriterT, 'W, 'M, 'T>) : WriterT<'W, 'M, 'T> =
        let token' = AppToken<WriterT, 'W>.Token token
        let token'' = AppToken<App<WriterT, 'W>, 'M>.Token token'
        let app2 = app3.Apply(token'') :?> App2<WriterT, 'W, 'M>
        let app = app2.Apply(token') :?> App<WriterT, 'W>
        app.Apply(token) :?> _
    static member UnWrap (writerT : WriterT<'W, 'M, 'T>) = 
        let (WT writer) = writerT in writer



// WriterT Monad Transformer instance
type WriterTMonadTrans<'W>(monoid : Monoid<'W>) = 
    inherit MonadTrans<App<WriterT, 'W>>() with
    override self.Lift monad m = 
        WriterT.Inj <| WT (monad { let! x = m in return (x, monoid.Empty) })


// WriterT Monad instance
type WriterTMonad<'W, 'M>(monoid : Monoid<'W>, monad : Monad<'M>) = 
    inherit Monad<App2<WriterT, 'W, 'M>>() with
    override self.Return x = WriterT.Inj <| WT (monad { return (x, monoid.Empty) })
    override self.Bind (m, f) = 
        WriterT.Inj <| WT (monad {
                                    let! (x, v) = WriterT.UnWrap (WriterT.Prj m) 
                                    let! (y, v') = WriterT.UnWrap (WriterT.Prj (f x))
                                    return (y, monoid.Append v v')
                                })
    