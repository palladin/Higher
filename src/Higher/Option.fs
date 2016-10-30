namespace Higher

/// Option Monad type
type Option private () =
    static let token = new Option()
    static member Inj (value : 'T option) : App<Option, 'T> =
        App<_, _>.Create(token, value)
    static member Prj (app : App<Option, 'T>) : 'T option =
        app.Apply(token) :?> _

/// Option Monad instance
type OptionMonad() =
    inherit Monad<Option>() with
    override self.Return x = Option.Inj <| Some x
    override self.Bind (m, f) =
        match Option.Prj m with
        | Some v -> f v
        | None -> Option.Inj None
