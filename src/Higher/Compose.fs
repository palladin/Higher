namespace Higher

type Compose<'F, 'G, 'A> = Comp of App<'F, App<'G, 'A>>

type Compose private () =
    static let token = new Compose()
    static member Inj (value : Compose<'F, 'G, 'A>) : App3<Compose, 'F, 'G, 'A> =
        App3<Compose, 'F, 'G, 'A>.Create(AppToken2<Compose, 'F, 'G>.Token(token), value)
    static member Prj (app3 : App3<Compose, 'F, 'G, 'A>) : Compose<'F, 'G, 'A> =
        app3.Apply(AppToken2<Compose, 'F, 'G>.Token(token)) :?> _
    static member Run (app3 : App3<Compose, 'F, 'G, 'A>) : App<'F, App<'G, 'A>> =
        let (Comp app) = Compose.Prj app3 in app


type ComposeFunctor<'F, 'G>(F : Functor<'F>, G : Functor<'G>) =
    inherit Functor<App2<Compose, 'F, 'G>>()
    override self.Map (f : 'A -> 'B) (app : App3<Compose, 'F, 'G, 'A>) : App3<Compose, 'F, 'G, 'B> =
        Compose.Inj <| Comp (F.Map (G.Map f) (Compose.Run app))


type ComposeApplicative<'F, 'G>(F : Applicative<'F>, G : Applicative<'G>) =
    inherit Applicative<App2<Compose, 'F, 'G>>()
    override self.Pure (v : 'A) : App3<Compose, 'F, 'G, 'A> =
        Compose.Inj <| Comp  (F.Pure (G.Pure v))
    override self.Apply (f : App<App2<Compose, 'F, 'G>, 'A -> 'B>) (app : App3<Compose, 'F, 'G, 'A>) : App3<Compose, 'F, 'G, 'B> =
        Compose.Inj <| Comp (F.Apply (F.Map G.Apply (Compose.Run f)) (Compose.Run app))
