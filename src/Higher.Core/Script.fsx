
// Represents type application
type App<'F, 'T> = App of 'F
type App2<'F, 'T1, 'T2> = App<App<'F, 'T1>, 'T2>
type App3<'F, 'T1, 'T2, 'T3> = App<App2<'F, 'T1, 'T2>, 'T3>
type App4<'F, 'T1, 'T2, 'T3, 'T4> = App<App3<'F, 'T1, 'T2, 'T3>, 'T4>

// Basic Types App Inj/Prj
type List = private F of obj with
    static member Inj (value : 'T list) : App<List, 'T> = App (F value)
    static member Prj (app : App<List, 'T>) : 'T list = 
        let (App (F value)) = app
        value :?> _

type Option = private F of obj with
    static member Inj (value : 'T option) : App<Option, 'T> = App (F value)
    static member Prj (app : App<Option, 'T>) : 'T option = 
        let (App (F value)) = app
        value :?> _




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