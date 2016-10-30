namespace Higher

/// Reader Monad type
type Reader<'R, 'T> = R of ('R -> 'T)

type Reader private () =
    static let token = new Reader()
    static member Inj (value : Reader<'R, 'T>) : App2<Reader, 'R, 'T> =
        App2<Reader, 'R, 'T>.Create(AppToken<Reader, 'R>.Token(token), value)
    static member Prj (app2 : App2<Reader, 'R, 'T>) : Reader<'R, 'T> = 
        app2.Apply(AppToken<Reader, 'R>.Token(token)) :?> _
    static member Run(reader : App2<Reader, 'R, 'T>) =
        let (R f) = Reader.Prj reader in f

/// Reader Monad instance
type ReaderMonad<'R>() =
    inherit Monad<App<Reader, 'R>>() with
    override self.Return x = Reader.Inj <| R (fun _ -> x)
    override self.Bind (m, f) =
        Reader.Inj <| R (fun env ->
                             let rf = Reader.Run m
                             Reader.Run (f (rf env)) env)
    member self.Get() : App2<Reader, 'R, 'R> =
        Reader.Inj <| R (fun env -> env)
