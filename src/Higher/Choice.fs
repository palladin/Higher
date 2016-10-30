namespace Higher

/// Choice Monad type
type Choice private () =
    static let token = new Choice()
    static member Inj (value : Choice<'A,'B>) : App2<Choice, 'A, 'B> =
        App2<_, _, _>.Create (AppToken<Choice, 'A>.Token(token), value)
    static member Prj (app2 : App2<Choice, 'A, 'B>) : Choice<'A, 'B> =
        app2.Apply(AppToken<Choice, 'A>.Token(token)) :?> _

/// Choice Monad instance
type ChoiceMonad<'A>() =
    inherit Monad<App<Choice, 'A>>() with
    override self.Return x = Choice.Inj <| Choice2Of2 x
    override self.Bind (m, f) =
        match Choice.Prj m with
        | Choice2Of2 v -> f v
        | Choice1Of2 a -> Choice.Inj (Choice1Of2 a)
