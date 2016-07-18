namespace Higher.Core

[<AbstractClass>]
type Adjunction<'F, 'G>(func : Functor<'F>, g : Functor<'G>) = 
    abstract Unit : 'A -> App<'G, App<'F, 'A>>
    abstract CoUnit : App<'F, App<'G, 'A>> -> 'A
    member self.LeftAdjunct (f : App<'F, 'A> -> 'B) (v : 'A) : App<'G, 'B> = 
        g.Map f (self.Unit v)
    member self.RightAdjunct (f : 'A -> App<'G, 'B>) (v : App<'F, 'A>) : 'B = 
        self.CoUnit <| func.Map f v
    member self.F : Functor<'F> = func
    member self.G : Functor<'G> = g

type AdjunctionMonad<'F, 'G>(adj : Adjunction<'F, 'G>) = 
    inherit Monad<App2<Compose, 'G, 'F>>()
    override self.Return (x : 'A) : App3<Compose, 'G, 'F, 'A> = 
        Compose.Inj <| Comp (adj.Unit x)
    override self.Bind (c : App3<Compose, 'G, 'F, 'A>, k : 'A -> App3<Compose, 'G, 'F, 'B>) = 
        let (Comp c') = Compose.Prj c
        Compose.Inj <| 
            Comp (adj.G.Map (fun v -> adj.RightAdjunct (fun a -> let (Comp c) = Compose.Prj (k a) in c) v) c')

type AdjunctionComonad<'F, 'G>(adj : Adjunction<'F, 'G>) = 
    inherit Comonad<App2<Compose, 'F, 'G>>()
    override self.Extract<'A> (w:App3<Compose, 'F, 'G, 'A>) : 'A =
      let (Comp w') = Compose.Prj w
      adj.CoUnit w'
    override self.Extend<'A, 'B> (f:App3<Compose, 'F, 'G, 'A> -> 'B) (w:App3<Compose, 'F, 'G, 'A>) : App3<Compose, 'F, 'G, 'B> =
      let (Comp w') = Compose.Prj w
      adj.F.Map (adj.LeftAdjunct (Comp >> Compose.Inj >> f)) w' |> Comp |> Compose.Inj