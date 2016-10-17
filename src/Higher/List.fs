namespace Higher

/// List Monad type
type List private () =
    static let token = new List()
    static member Inj (value : 'T list) : App<List, 'T> =
        new App<_, _>(token, value)
    static member Prj (app : App<List, 'T>) : 'T list =
        app.Apply(token) :?> _

/// List Monad instance
type ListMonad() =
    inherit Monad<List>() with
    override self.Return x = List.Inj [x]
    override self.Bind (m, f) =
        m
        |> List.Prj
        |> List.collect (fun v -> List.Prj (f v))
        |> List.Inj

/// List Applicative Functor instance
type ListApplicative() =
    inherit Applicative<List>() with
    override self.Pure x = List.Inj [x]
    override self.Apply appF app =
        let fs, xs = List.Prj appF, List.Prj app
        xs |> List.zip fs |> List.map (fun (f, x) -> f x) |> List.Inj

/// List Traversable instance
type ListTraversable() =
     inherit Traversable<List>() with
     override self.Map f func =
        func |> List.Prj |> List.map f |> List.Inj
     override self.Traverse<'F, 'T, 'R> (app : Applicative<'F>) (f : 'T -> App<'F, 'R>) (trav : App<List, 'T>) =
        let xs = trav |> List.Prj |> List.map f
        let appCons = app.Pure (fun (x : 'R) xs -> List.Inj (x :: (List.Prj xs)))
        List.foldBack (fun appX appXs -> app.Apply (app.Apply appCons appX) appXs) xs (app.Pure (List.Inj []))
