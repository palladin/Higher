
open System

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


// Monad Class and helper functions
[<AbstractClass>]
type Monad<'M>() = 
    abstract Return<'T> : 'T -> App<'M, 'T>
    abstract Bind<'T, 'R> : App<'M, 'T> -> ('T -> App<'M, 'R>) -> App<'M, 'R>

module Monad = 

    let sequence (d : Monad<'M>) (ms :  App<'M, 'T> list) : App<'M, 'T list> = 
        raise <| new System.NotImplementedException()

// Basic Monad instances
type ListMonad() = 
    inherit Monad<List>() with
        override self.Return x = List.Inj [x]
        override self.Bind m f =
            m
            |> List.Prj 
            |> List.collect (fun v -> List.Prj (f v))  
            |> List.Inj 

type SeqMonad() = 
    inherit Monad<Seq>() with
        override self.Return x = Seq.Inj [x]
        override self.Bind m f =
            m
            |> Seq.Prj 
            |> Seq.collect (fun v -> Seq.Prj (f v))  
            |> Seq.Inj 
            
type OptionMonad() = 
    inherit Monad<Option>() with
        override self.Return x = Option.Inj <| Some x
        override self.Bind m f = 
            match Option.Prj m with
            | Some v -> f v
            | None -> Option.Inj None



#time

for i = 1 to 1000 do
    [1..100] |> List.collect (fun x -> [1..x]) |> ignore

let listMonad = new ListMonad()
for i = 1 to 1000 do
    let appList = List.Inj [1..100] 
    listMonad.Bind appList (fun x -> List.Inj [1..x]) |> List.Prj |> ignore

