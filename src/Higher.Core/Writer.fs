namespace Higher.Core

// Writer Monad type
type Writer<'W, 'T> = W of ('T * 'W)
type Writer private () =
    static let token = new Writer()
    static member Inj (value : Writer<'W, 'T>) : App2<Writer, 'W, 'T> = 
        let app = new App<Writer, 'W>(token, value)
        new App2<Writer, 'W, 'T>(AppToken<Writer, 'W>.Token token, app)
    static member Prj (app2 : App2<Writer, 'W, 'T>) : Writer<'W, 'T> = 
        let app = app2.Apply(AppToken<Writer, 'W>.Token token) :?> App<Writer, 'W>
        app.Apply(token) :?> _

// Writer Monad instance
type WriterMonad<'W>(monoid : Monoid<'W>) = 
    inherit Monad<App<Writer, 'W>>() with
    override self.Return x = Writer.Inj <| W (x, monoid.Empty)
    override self.Bind (m, f) =
        let (W (x, v)) = Writer.Prj m
        let (W (y, v')) = Writer.Prj <| f x   
        Writer.Inj <| W (y, monoid.Append v v')

