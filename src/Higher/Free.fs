namespace Higher

/// Free Monad type
type Free<'F, 'T> =
    | Return of 'T
    | Wrap of App<'F, Free<'F, 'T>>

type Free private () =
    static let token = new Free()
    static member Inj (value : Free<'F, 'T>) : App2<Free, 'F, 'T> =
        App2<Free, 'F, 'T>.Create(AppToken<Free, 'F>.Token(token), value)
    static member Prj (app2 : App2<Free, 'F, 'T>) : Free<'F, 'T> =
        app2.Apply(AppToken<Free, 'F>.Token(token)) :?> _

/// Free Monad instance
type FreeMonad<'F>(fuctorFree : Functor<'F>) =
    inherit Monad<App<Free, 'F>>() with
    override self.Return x = Free.Inj <| Return x
    override self.Bind (m, f) =
        match Free.Prj m with
        | Return x -> f x
        | Wrap func ->
            let func' = fuctorFree.Map (fun m' -> Free.Prj <| self.Bind(Free.Inj m', f)) func
            Free.Inj <| Wrap func'
