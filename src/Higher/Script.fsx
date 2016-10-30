#r "bin/Release/Higher.dll"
open Higher


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

let fstL<'a, 'b, 'c> : FTLens<'a * 'b, 'c * 'b, 'a, 'c> =
    lens fst <| fun x (_, y) -> (x, y)
let sndL<'a, 'b, 'c> : FTLens<'a * 'b, 'a * 'c, 'b, 'c> =
    lens snd <| fun y (x, _) -> (x, y)

do ((1, (2.0, '3')), true)
    |> over (fstL >-> sndL >-> fstL) (fun x -> x + 3.0 |> string)
    |> printfn "%A" // ((1, ("5", '3')), true)


// Perfect tree example

let p = Succ (Succ (Zero ((1, 2), (3, 4))))

let phi = { new PerfectFolder<List> with
    member self.Zero<'T>(v : 'T) : App<List, 'T> =
        List.Inj [v]
    member self.Succ<'T>(app : App<List, 'T * 'T>) : App<List, 'T> =
        let v = List.Prj app
        List.Inj <| List.collect (fun (f, s) -> [f; s]) v }

List.Prj (Perfect.foldP phi p) // [1; 2; 3; 4]


// Curry - Uncurry Adjunction

type CurryUncurry<'A>() =
    inherit Adjunction<App<Tuple, 'A>, App<Fun, 'A>>(new TupleFunctor<'A>(), new FunFunctor<'A>())
    override self.Unit (v : 'B) : App<App<Fun, 'A>, App2<Tuple, 'A, 'B>> =
        Fun.Inj (fun x -> Tuple.Inj (x, v))
    override self.CoUnit (v : App<App<Tuple, 'A>, App2<Fun, 'A, 'B>> ) : 'B =
        let (x, f) = Tuple.Prj v
        Fun.Prj f x


