namespace Higher


/// Tuple type
type Tuple private () =
    static let token = new Tuple()
    static member Inj (value : 'A * 'B) : App2<Tuple, 'A, 'B> =
        App2<Tuple, 'A, 'B>.Create(AppToken<Tuple, 'A>.Token(token), value)
    static member Prj (app2 : App2<Tuple, 'A, 'B>) : 'A * 'B =
        app2.Apply(AppToken<Tuple, 'A>.Token(token)) :?> _


/// Tuple Functor
type TupleFunctor<'W>() =
    inherit Functor<App<Tuple, 'W>>()
    override self.Map (f : 'A -> 'B) (v : App2<Tuple, 'W, 'A>) : App2<Tuple, 'W, 'B> =
        let (w, a) = Tuple.Prj v
        Tuple.Inj (w, f a)
