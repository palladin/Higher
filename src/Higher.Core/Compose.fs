namespace Higher.Core

type Compose<'F, 'G, 'A> = Comp of App<'F, App<'G, 'A>>

type Compose private () =    
    static let token = new Compose()
    static member Inj (value : Compose<'F, 'G, 'A>) : App3<Compose, 'F, 'G, 'A> = 
        let app = new App<Compose, 'F>(token, value)
        let app2 = new App2<Compose, 'F, 'G>(AppToken<Compose, 'F>.Token token, app)
        new App3<Compose, 'F, 'G, 'A>(AppToken<App<Compose, 'F>, 'G>.Token app, app2)
    static member Prj (app3 : App3<Compose, 'F, 'G, 'A>) : Compose<'F, 'G, 'A> =
        let token' = AppToken<Compose, 'F>.Token token
        let token'' = AppToken<App<Compose, 'F>, 'G>.Token token'
        let app2 = app3.Apply(token'') :?> App2<Compose, 'F, 'G>
        let app = app2.Apply(token') :?> App<Compose, 'F>
        app.Apply(token) :?> _
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
        
        

