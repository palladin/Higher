namespace Higher.Core


// Tuple type
type Tuple private () =
    static let token = new Tuple()
    static member Inj (value : 'A * 'B) : App2<Tuple, 'A, 'B> =
        let app = new App<Tuple, 'A>(token, value)
        new App2<Tuple, 'A, 'B>(AppToken<Tuple, 'A>.Token token, app)
    static member Prj (app2 : App2<Tuple, 'A, 'B>) : 'A * 'B = 
        let app = app2.Apply(AppToken<Tuple, 'A>.Token token) :?> App<Tuple, 'A>
        app.Apply(token) :?> _
    static member Run(t : App2<Tuple, 'A, 'B>) = 
        Tuple.Prj t 


// Tuple Functor
type TupleFunctor<'W>() = 
    inherit Functor<App<Tuple, 'W>>() 
    override self.Map (f : 'A -> 'B) (v : App2<Tuple, 'W, 'A>) : App2<Tuple, 'W, 'B> =
        let (w, a) = Tuple.Prj v
        Tuple.Inj (w, f a)
