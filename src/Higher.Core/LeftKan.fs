namespace Higher.Core

type Lambda<'G, 'H, 'A, 'R> = 
    abstract Invoke<'B> : (App<'G, 'B> -> 'A) -> App<'H, 'B> -> 'R

// type Lan g h α = ∃β. (g β → α) * h β
// type CoYoneda f a = Lan Id f a
[<AbstractClass>]
type Lan<'G, 'H, 'A>() =
    abstract Invoke<'R> : Lambda<'G, 'H, 'A, 'R> -> 'R 

type LanConstr<'G, 'H, 'B, 'A>(f : App<'G, 'B> -> 'A, app : App<'H, 'B>) = 
    inherit Lan<'G, 'H, 'A>()
    override self.Invoke lambda = 
        lambda.Invoke f app


type Lan private () =
    static let token = new Lan()
    static member Inj (value : Lan<'G, 'H, 'A>) : App3<Lan, 'G, 'H, 'A> = 
        let app = new App<Lan, 'G>(token, value)
        let app2 = new App2<Lan, 'G, 'H>(AppToken<Lan, 'G>.Token token, app)
        new App3<Lan, 'G, 'H, 'A>(AppToken<App<Lan, 'G>, 'H>.Token app, app2)
    static member Prj (app3 : App3<Lan, 'G, 'H, 'A>) : Lan<'G, 'H, 'A> =
        let token' = AppToken<Lan, 'G>.Token token
        let token'' = AppToken<App<Lan, 'G>, 'H>.Token token'
        let app2 = app3.Apply(token'') :?> App2<Lan, 'G, 'H>
        let app = app2.Apply(token') :?> App<Lan, 'G>
        app.Apply(token) :?> _


type LanFunctor<'G, 'H>() = 
    inherit Functor<App2<Lan, 'G, 'H>>()
    
    override self.Map (f : 'A -> 'B) (app : App3<Lan, 'G, 'H, 'A>) = 
        Lan.Inj <|
            (Lan.Prj app).Invoke<Lan<'G, 'H, 'B>> 
                { new Lambda<'G, 'H, 'A, Lan<'G, 'H, 'B>> with
                    member self.Invoke<'C> (k : App<'G, 'C> -> 'A) (app : App<'H, 'C>) = 
                        new LanConstr<'G, 'H, 'C, 'B>(k >> f, app) :> Lan<'G, 'H, 'B> } 
        
        
        



