namespace Higher

/// State Monad type
type State<'S, 'T> = S of ('S -> ('T * 'S))

type State private () =
    static let token = new State()
    static member Inj (value : State<'S, 'T>) : App2<State, 'S, 'T> =
        App2<State, 'S, 'T>.Create(AppToken<State, 'S>.Token(token), value)
    static member Prj (app2 : App2<State, 'S, 'T>) : State<'S, 'T> =
        app2.Apply(AppToken<State, 'S>.Token(token)) :?> _
    static member Run(state : App2<State, 'S, 'T>) =
        let (S f) = State.Prj state in f

/// State Monad instance
type StateMonad<'S>() =
    inherit Monad<App<State, 'S>>() with
    override self.Return x = State.Inj <| S (fun s -> (x, s))
    override self.Bind (m, f) =
        State.Inj <| S (fun s ->
                            let (x, s') = (State.Run m) s
                            State.Run (f x) s')

    member self.Get() : App2<State, 'S, 'S> =
        State.Inj <| S (fun s -> (s, s))
    member self.Put (newState : 'S) : App2<State, 'S, unit> =
        State.Inj <| S (fun _ -> ((), newState))
