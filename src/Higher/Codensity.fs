namespace Higher

/// type C µ α = C (∀β. (α → µ β) → µ β)
type Codensity<'M, 'A> =
    abstract Invoke<'B> : ('A -> App<'M, 'B>) -> App<'M, 'B>


type CodenT private () =
    static let token = new CodenT()
    static member Inj (value : Codensity<'M, 'T>) : App2<CodenT, 'M, 'T> =
        App2<CodenT, 'M, 'T>.Create(AppToken<CodenT, 'M>.Token(token), value)
    static member Prj (app2 : App2<CodenT, 'M, 'T>) : Codensity<'M, 'T> =
        app2.Apply(AppToken<CodenT, 'M>.Token(token)) :?> _


type CodensityMonad<'M>() =
    inherit Monad<App<CodenT, 'M>>()
    override self.Return (x : 'A) =
        CodenT.Inj <|
            { new Codensity<'M, 'A> with
                member self.Invoke<'B> (f : 'A -> App<'M, 'B>) = f x }
    override self.Bind (c : App2<CodenT, 'M, 'A>, k : 'A -> App2<CodenT, 'M, 'B>) =
        CodenT.Inj <|
            { new Codensity<'M, 'B> with
                member self.Invoke<'R> (f : 'B -> App<'M, 'R>) =
                    let c = CodenT.Prj c
                    c.Invoke (fun a -> let c' = CodenT.Prj (k a) in c'.Invoke f) }
