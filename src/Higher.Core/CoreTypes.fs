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


// A Singleton-like type for managing parameterized tokens 
type AppToken<'App, 'R>() = 
    static let appTokenRef = ref Unchecked.defaultof<App<'App, 'R>> 
    static member Token (token : 'App) = 
        if !appTokenRef = Unchecked.defaultof<App<'App, 'R>> then
            lock appTokenRef (fun () ->
                if !appTokenRef = Unchecked.defaultof<App<'App, 'R>> then
                    appTokenRef := new App<'App, 'R>(token, Unchecked.defaultof<'R>)
            )
        !appTokenRef

// Basic Types

type Reader<'R, 'T> = R of ('R -> 'T)
type Reader private () =
    static let token = new Reader()
    static member Inj (value : Reader<'R, 'T>) : App2<Reader, 'R, 'T> =
        let app = new App<Reader, 'R>(token, value)
        new App2<Reader, 'R, 'T>(AppToken<Reader, 'R>.Token token, app)
    static member Prj (app2 : App2<Reader, 'R, 'T>) : Reader<'R, 'T> = 
        let app = app2.Apply(AppToken<Reader, 'R>.Token token) :?> App<Reader, 'R>
        app.Apply(token) :?> _

type Writer<'W, 'T> = W of ('T * 'W)
type Writer private () =
    static let token = new Writer()
    static member Inj (value : Writer<'W, 'T>) : App2<Writer, 'W, 'T> = 
        let app = new App<Writer, 'W>(token, value)
        new App2<Writer, 'W, 'T>(AppToken<Writer, 'W>.Token token, app)
    static member Prj (app2 : App2<Writer, 'W, 'T>) : Writer<'W, 'T> = 
        let app = app2.Apply(AppToken<Writer, 'W>.Token token) :?> App<Writer, 'W>
        app.Apply(token) :?> _

type State<'S, 'T> = S of ('S -> ('T * 'S))
type State private () =
    static let token = new State()
    static member Inj (value : State<'S, 'T>) : App2<State, 'S, 'T> = 
        let app = new App<State, 'S>(token, value)
        new App2<State, 'S, 'T>(AppToken<State, 'S>.Token token, app)
    static member Prj (app2 : App2<State, 'S, 'T>) : State<'S, 'T> = 
        let app = app2.Apply(AppToken<State, 'S>.Token token) :?> App<State, 'S>
        app.Apply(token) :?> _

type Cont<'R, 'T> = C of (('T -> 'R) -> 'R)
type Cont private () =
    static let token = new Cont()
    static member Inj (value : Cont<'R, 'T>) : App2<Cont, 'R, 'T> = 
        let app = new App<Cont, 'R>(token, value)
        new App2<Cont, 'R, 'T>(AppToken<Cont, 'R>.Token token, app)
    static member Prj (app2 : App2<Cont, 'R, 'T>) : Cont<'R, 'T> = 
        let app = app2.Apply(AppToken<Cont, 'R>.Token token) :?> App<Cont, 'R>
        app.Apply(token) :?> _


