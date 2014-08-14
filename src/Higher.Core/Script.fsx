

type App<'F, 'T> = App of 'F
type App2<'F, 'T1, 'T2> = App<App<'F, 'T1>, 'T2>
type App3<'F, 'T1, 'T2, 'T3> = App<App2<'F, 'T1, 'T2>, 'T3>
type App4<'F, 'T1, 'T2, 'T3, 'T4> = App<App3<'F, 'T1, 'T2, 'T3>, 'T4>

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


let foo = List.Inj [1..100]
List.Prj foo

let foo = Option.Inj <| Some 1
Option.Prj foo


