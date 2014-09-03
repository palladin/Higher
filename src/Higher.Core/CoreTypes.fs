namespace Higher.Core
open System

// The basic idea of Type Defunctionalization is based on 
// https://ocamllabs.github.io/higher/lightweight-higher-kinded-polymorphism.pdf
// OCaml implementation https://github.com/ocamllabs/higher

// Represents type application
// To ensure type-safety we use a secret token based control access policy.
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








