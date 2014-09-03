namespace Higher.Core

// State Monad type
type State<'S, 'T> = S of ('S -> ('T * 'S))
type State private () =
    static let token = new State()
    static member Inj (value : State<'S, 'T>) : App2<State, 'S, 'T> = 
        let app = new App<State, 'S>(token, value)
        new App2<State, 'S, 'T>(AppToken<State, 'S>.Token token, app)
    static member Prj (app2 : App2<State, 'S, 'T>) : State<'S, 'T> = 
        let app = app2.Apply(AppToken<State, 'S>.Token token) :?> App<State, 'S>
        app.Apply(token) :?> _

// State Monad instance
type StateMonad<'S>() = 
    inherit Monad<App<State, 'S>>() with
    override self.Return x = State.Inj <| S (fun s -> (x, s))
    override self.Bind (m, f) = 
        State.Inj <| S (fun s -> 
                            let (S stateF) = State.Prj m
                            let (x, s') = stateF s
                            let (S stateF') = State.Prj <| f x
                            stateF' s')

    member self.Get() : App2<State, 'S, 'S> = 
        State.Inj <| S (fun s -> (s, s))
    member self.Put (newState : 'S) : App2<State, 'S, unit> = 
        State.Inj <| S (fun s -> ((), newState))

