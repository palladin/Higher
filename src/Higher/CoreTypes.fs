namespace Higher
open System

// The basic idea of Type Defunctionalization is based on
// https://ocamllabs.github.io/higher/lightweight-higher-kinded-polymorphism.pdf
// OCaml implementation https://github.com/ocamllabs/higher

/// Represents type application.
/// To ensure type-safety we use a secret token based control access policy.
type App<'F, 'T> private (value : obj) =
    private new(token : 'F, value : obj) = 
        if Object.ReferenceEquals(token, Unchecked.defaultof<'F>) then
            raise <| new System.InvalidOperationException("Invalid token")
        App<'F, 'T>(value)
    private new(token : 'F) = App<'F, 'T>(token, null) 
    // Apply the secret token to have access to the encapsulated value
    member self.Apply(token : 'F) : obj =
        if Object.ReferenceEquals(token, Unchecked.defaultof<'F>) then
            raise <| new System.InvalidOperationException("Invalid token")
        value 
    static member Create<'F, 'T>(token : 'F, value : obj) = new App<'F, 'T>(token , value)
    static member Create<'F, 'T>(token : 'F) = new App<'F, 'T>(token)
        
type App2<'F, 'T1, 'T2> = App<App<'F, 'T1>, 'T2>
type App3<'F, 'T1, 'T2, 'T3> = App<App2<'F, 'T1, 'T2>, 'T3>
type App4<'F, 'T1, 'T2, 'T3, 'T4> = App<App3<'F, 'T1, 'T2, 'T3>, 'T4>


// A Singleton-like type for managing parameterized tokens
type AppToken<'F, 'T>() =
    static let mutable appToken = Unchecked.defaultof<App<'F, 'T>>
    static member Token (token : 'F) =
        if Object.ReferenceEquals(appToken, Unchecked.defaultof<App<'F, 'T>>) then
            appToken <- App<'F, 'T>.Create(token)
        appToken

type AppToken2<'F, 'T1, 'T2>() =
    static let mutable appToken = Unchecked.defaultof<App2<'F, 'T1, 'T2>>
    static member Token (token : 'F) =
        if Object.ReferenceEquals(appToken, Unchecked.defaultof<App2<'F, 'T1, 'T2>>) then
            appToken <- App2<'F, 'T1, 'T2>.Create(AppToken<'F, 'T1>.Token token)
        appToken

type AppToken3<'F, 'T1, 'T2, 'T3>() =
    static let mutable appToken = Unchecked.defaultof<App3<'F, 'T1, 'T2, 'T3>>
    static member Token (token : 'F) =
        if Object.ReferenceEquals(appToken, Unchecked.defaultof<App3<'F, 'T1, 'T2, 'T3>>) then
            appToken <- App3<'F, 'T1, 'T2, 'T3>.Create(AppToken2<'F, 'T1, 'T2>.Token token)
        appToken

type AppToken4<'F, 'T1, 'T2, 'T3, 'T4>() =
    static let mutable appToken = Unchecked.defaultof<App4<'F, 'T1, 'T2, 'T3, 'T4>>
    static member Token (token : 'F) =
        if Object.ReferenceEquals(appToken, Unchecked.defaultof<App4<'F, 'T1, 'T2, 'T3, 'T4>>) then
            appToken <- App4<'F, 'T1, 'T2, 'T3, 'T4>.Create(AppToken3<'F, 'T1, 'T2, 'T3>.Token token)
        appToken
