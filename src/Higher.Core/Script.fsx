
#load "CoreTypes.fs"
open Higher.Core
#load "Monad.fs"
open Higher.Core






#time

for i = 1 to 1000 do
    [1..100] |> List.collect (fun x -> [1..x]) |> ignore

let listMonad = new ListMonad()
for i = 1 to 1000 do
    let appList = List.Inj [1..100] 
    listMonad.Bind appList (fun x -> List.Inj [1..x]) |> List.Prj |> ignore

