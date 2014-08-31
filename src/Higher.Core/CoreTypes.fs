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

// FSharp.Core Types 
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


// Basic Types

type Reader<'R, 'T> = R of ('R -> 'T)
type Reader private () =
    static let token = new Reader()
    static member Inj (value : Reader<'R, 'T>) : App2<Reader, 'R, 'T> = 
        // new App<_, _>(token, value)
        raise <| new NotImplementedException()
    static member Prj (app : App2<Reader, 'R, 'T>) : Reader<'R, 'T> = 
        //app.Apply(token) :?> _
        raise <| new NotImplementedException()

type Writer<'W, 'T> = W of ('T * 'W)
type Writer private () =
    static let token = new Writer()
    static member Inj (value : Writer<'W, 'T>) : App2<Writer, 'W, 'T> = 
        // new App<_, _>(token, value)
        raise <| new NotImplementedException()
    static member Prj (app : App2<Writer, 'W, 'T>) : Writer<'W, 'T> = 
        //app.Apply(token) :?> _
        raise <| new NotImplementedException()

type State<'S, 'T> = S of ('S -> ('T * 'S))
type State private () =
    static let token = new State()
    static member Inj (value : State<'S, 'T>) : App2<State, 'S, 'T> = 
        // new App<_, _>(token, value)
        raise <| new NotImplementedException()
    static member Prj (app : App2<State, 'S, 'T>) : State<'S, 'T> = 
        //app.Apply(token) :?> _
        raise <| new NotImplementedException()

type Cont<'R, 'T> = C of (('T -> 'R) -> 'R)
type Cont private () =
    static let token = new Cont()
    static member Inj (value : Cont<'R, 'T>) : App2<Cont, 'R, 'T> = 
        // new App<_, _>(token, value)
        raise <| new NotImplementedException()
    static member Prj (app : App2<Cont, 'S, 'T>) : Cont<'R, 'T> = 
        //app.Apply(token) :?> _
        raise <| new NotImplementedException()