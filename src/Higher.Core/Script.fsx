
// Represents type application
type App<'F, 'T> (token : 'F, value : obj) =
    // Apply the secret token to have access the encapsulated value
    member self.Apply(token' : 'F) =
        if System.Object.ReferenceEquals(token, token') then  
            value 
        else raise <| new System.InvalidOperationException("Invalid token")
type App2<'F, 'T1, 'T2> = App<App<'F, 'T1>, 'T2>
type App3<'F, 'T1, 'T2, 'T3> = App<App2<'F, 'T1, 'T2>, 'T3>
type App4<'F, 'T1, 'T2, 'T3, 'T4> = App<App3<'F, 'T1, 'T2, 'T3>, 'T4>

// Basic Type Constructors with Inj/Prj operations
// To ensure type-safety we use a secret token based control access policy. 
type List private () =
    static member private token = new List()  
    static member Inj (value : 'T list) : App<List, 'T> = 
        new App<_, _>(List.token, value)
    static member Prj (app : App<List, 'T>) : 'T list = 
        app.Apply(List.token) :?> _

type Option private () =
    static member private token = new Option()
    static member Inj (value : 'T option) : App<Option, 'T> = 
        new App<_, _>(Option.token, value)
    static member Prj (app : App<Option, 'T>) : 'T option = 
        app.Apply(Option.token) :?> _



[<AbstractClass>]
type Monad<'M>() = 
    abstract Return<'T> : 'T -> App<'M, 'T>
    abstract Bind<'T, 'R> : App<'M, 'T> -> ('T -> App<'M, 'R>) -> App<'M, 'R>


type OptionMonad() = 
    inherit Monad<Option>() with
        override self.Return x = Option.Inj <| Some x
        override self.Bind m f = 
            match Option.Prj m with
            | Some v -> f v
            | None -> Option.Inj None


module Monad = 
    
    let sequence (d : Monad<'M>) (ms :  App<'M, 'T> list) : App<'M, 'T list> = 
        raise <| new System.NotImplementedException()