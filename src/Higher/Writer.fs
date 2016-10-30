namespace Higher

/// Writer Monad type
type Writer<'W, 'T> = W of ('T * 'W)

type Writer private () =
    static let token = new Writer()
    static member Inj (value : Writer<'W, 'T>) : App2<Writer, 'W, 'T> =
        App2<Writer, 'W, 'T>.Create(AppToken<Writer, 'W>.Token(token), value)
    static member Prj (app2 : App2<Writer, 'W, 'T>) : Writer<'W, 'T> =
        app2.Apply(AppToken<Writer, 'W>.Token(token)) :?> _
    static member Run(writer : App2<Writer, 'W, 'T>) =
        let (W (v, w)) = Writer.Prj writer in (v, w)

/// Writer Monad instance
type WriterMonad<'W>(monoid : Monoid<'W>) =
    inherit Monad<App<Writer, 'W>>() with
    override self.Return x = Writer.Inj <| W (x, monoid.Empty)
    override self.Bind (m, f) =
        let (x, v) = Writer.Run m
        let (y, v') = Writer.Run <| f x
        Writer.Inj <| W (y, monoid.Append v v')
