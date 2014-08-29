namespace Higher.Core

// Monad Class 
[<AbstractClass>]
type Monad<'M>() = 
    inherit Applicative<'M>() 
        override self.Pure x = self.Return x
        override self.Apply appF app = 
            self {
                let! f = appF
                let! x = app
                return f x
            }
    abstract Return<'T> : 'T -> App<'M, 'T>
    abstract Bind<'T, 'R> : App<'M, 'T> *  ('T -> App<'M, 'R>) -> App<'M, 'R>
    

// Generic Monad functions 
module Monad = 

    let rec sequence (monad : Monad<'M>) (ms :  App<'M, 'T> list) : App<'M, 'T list> =
        match ms with
        | m :: ms -> 
            monad {
                let! x = m
                let! xs = sequence monad ms
                return x :: xs
            }
        | [] -> monad { return [] }

    let mapM (monad : Monad<'M>) (f : 'T -> App<'M, 'R>) (xs : 'T list) : App<'M, 'R list> =
        xs |> List.map f |> sequence monad

    let rec filterM (monad : Monad<'M>) (f : 'T -> App<'M, bool>) (xs : 'T list) : App<'M, 'T list> =
        match xs with
        | x :: xs -> 
            monad {
                let! flag = f x
                let! xs' = filterM monad f xs
                return if flag then x :: xs' else xs'
            }
        | [] -> monad { return [] }

// Basic Monad instances
type ListMonad() = 
    inherit Monad<List>() with
        override self.Return x = List.Inj [x]
        override self.Bind (m, f) =
            m
            |> List.Prj 
            |> List.collect (fun v -> List.Prj (f v))  
            |> List.Inj 

type SeqMonad() = 
    inherit Monad<Seq>() with
        override self.Return x = Seq.Inj [x]
        override self.Bind (m, f) =
            m
            |> Seq.Prj 
            |> Seq.collect (fun v -> Seq.Prj (f v))  
            |> Seq.Inj 
            
type OptionMonad() = 
    inherit Monad<Option>() with
        override self.Return x = Option.Inj <| Some x
        override self.Bind (m, f) = 
            match Option.Prj m with
            | Some v -> f v
            | None -> Option.Inj None

