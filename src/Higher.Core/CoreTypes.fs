namespace Higher.Core
open System

// The basic idea of Type Defunctionalization is based on 
// https://ocamllabs.github.io/higher/lightweight-higher-kinded-polymorphism.pdf
// OCaml implementation https://github.com/ocamllabs/higher

// Represents type application
type App<'F, 'T> (token : 'F, value : obj) =
    do 
        if Object.ReferenceEquals(token,  Unchecked.defaultof<'F>) then
            raise <| new System.InvalidOperationException("Invalid token")
    // Apply the secret token to have access to the encapsulated value
    member self.Apply(token' : 'F) =
        if Object.ReferenceEquals(token, token') then  
            value 
        else raise <| new InvalidOperationException("Invalid token")
type App2<'F, 'T1, 'T2> = App<App<'F, 'T1>, 'T2>
type App3<'F, 'T1, 'T2, 'T3> = App<App2<'F, 'T1, 'T2>, 'T3>
type App4<'F, 'T1, 'T2, 'T3, 'T4> = App<App3<'F, 'T1, 'T2, 'T3>, 'T4>

// Basic Type Constructors with Inj/Prj operations
// To ensure type-safety we use a secret token based control access policy. 
type List private () =
    static let token = new List()  
    static member Inj (value : 'T list) : App<List, 'T> = 
        new App<_, _>(token, value)
    static member Prj (app : App<List, 'T>) : 'T list = 
        app.Apply(token) :?> _

type Seq private () =
    static let token = new Seq()  
    static member Inj (value : seq<'T>) : App<Seq, 'T> = 
        new App<_, _>(token, value)
    static member Prj (app : App<Seq, 'T>) : seq<'T> = 
        app.Apply(token) :?> _

type Option private () =
    static let token = new Option()
    static member Inj (value : 'T option) : App<Option, 'T> = 
        new App<_, _>(token, value)
    static member Prj (app : App<Option, 'T>) : 'T option = 
        app.Apply(token) :?> _

