namespace Higher

/// WriterT Monad Transformer type
type WriterT<'W, 'M, 'T> = WT of App<'M, ('T * 'W)>

type WriterT private () =
    static let token = new WriterT()
    static member Inj (value : WriterT<'W, 'M, 'T>) : App3<WriterT, 'W, 'M, 'T> =
        App3<WriterT, 'W, 'M, 'T>.Create(AppToken2<WriterT, 'W, 'M>.Token(token), value)
    static member Prj (app3 : App3<WriterT, 'W, 'M, 'T>) : WriterT<'W, 'M, 'T> =
        app3.Apply(AppToken2<WriterT, 'W, 'M>.Token(token)) :?> _
    static member Run (writerT : App3<WriterT, 'W, 'M, 'T>) =
        let (WT writer) = WriterT.Prj writerT in writer


/// WriterT Monad Transformer instance
type WriterTMonadTrans<'W>(monoid : Monoid<'W>) =
    inherit MonadTrans<App<WriterT, 'W>>() with
    override self.Lift monad m =
        WriterT.Inj <| WT (monad { let! x = m in return (x, monoid.Empty) })


/// WriterT Monad instance
type WriterTMonad<'W, 'M>(monoid : Monoid<'W>, monad : Monad<'M>) =
    inherit Monad<App2<WriterT, 'W, 'M>>() with
    override self.Return x = WriterT.Inj <| WT (monad { return (x, monoid.Empty) })
    override self.Bind (m, f) =
        WriterT.Inj <| WT (monad {
                                    let! (x, v) = WriterT.Run m
                                    let! (y, v') = WriterT.Run <| f x
                                    return (y, monoid.Append v v')
                                })
