
#r "bin/Release/Higher.Core.dll"
open Higher.Core

let incr (monad : StateTMonad<_, _>) a = monad {
        let! n = monad.Get()
        do! monad.Put (n + 1)
        return n, a
    }

let zipIndex (xs: 'a list) : (int * 'a) list =
    let monad = new StateTMonad<_, _>(new ContMonad<_>())
    Cont.Run (StateT.Run (Monad.mapM monad (incr monad) xs) 0) (fun (a, _) -> a)
    
    
zipIndex [1..100000]

// Category Example

let f = Fun.Inj (fun x -> x + 1) 
let g = Fun.Inj (fun (x : int) -> string x)

let category = new FunCategory()
let h = category.Compose f g
Fun.Prj h 1 // "2"

// Index-preserving functions
let f' = Index.Inj { new Index<Option, List> with
                        member self.Invoke x = 
                            match Option.Prj x with
                            | Some v -> List.Inj [v] 
                            | None -> List.Inj [] }
let g' = Index.Inj { new Index<List, Seq> with
                        member self.Invoke x = 
                            x |> List.Prj |> Seq.ofList |> Seq.Inj }

let indexCategory = new IndexCategory()
let h' = indexCategory.Compose f' g'
Some 2
|> Option.Inj
|> (Index.Prj h').Invoke 
|> Seq.Prj // [2]


// Lens example
open Lens

let fstL<'a, 'b> : FTLens<'a * 'b, 'a> =
    lens fst <| fun x (_, y) -> (x, y)
let sndL<'a, 'b> : FTLens<'a * 'b, 'b> =
    lens snd <| fun y (x, _) -> (x, y)

(("2", (41, true)), 1) 
|> over (fstL >-> sndL >-> fstL) (fun x -> x + 1) // (("2", (42, true)), 1)

