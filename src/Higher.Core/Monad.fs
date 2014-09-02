namespace Higher.Core
open System 

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
    member self.ReturnFrom m = m
    

// Generic Monad functions 
module Monad = 
    
    let join (monad : Monad<'M>) (mm : App<'M, App<'M, 'T>>) : App<'M, 'T> = 
        monad.Bind(mm, fun m -> m)

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


type ReaderMonad<'R>() = 
    inherit Monad<App<Reader, 'R>>() with
    override self.Return x = Reader.Inj <| R (fun env -> x)
    override self.Bind (m, f) = 
        Reader.Inj <| R (fun env -> 
                            let (R rf) = Reader.Prj m 
                            let (R rf') = Reader.Prj <| f (rf env)
                            rf' env)
    member self.Get() : App2<Reader, 'R, 'R> =
        Reader.Inj <| R (fun env -> env) 
        

type WriterMonad<'W>(monoid : Monoid<'W>) = 
    inherit Monad<App<Writer, 'W>>() with
    override self.Return x = Writer.Inj <| W (x, monoid.Empty)
    override self.Bind (m, f) =
        let (W (x, v)) = Writer.Prj m
        let (W (y, v')) = Writer.Prj <| f x   
        Writer.Inj <| W (y, monoid.Append v v')


type StateMonad<'S>() = 
    inherit Monad<App<State, 'S>>() with
    override self.Return x = State.Inj <| S (fun s -> (x, s))
    override self.Bind (m, f) = 
        State.Inj <| S (fun s -> 
                            let (S stateF) = State.Prj m
                            let (x, s') = stateF s
                            let (S stateF') = State.Prj <| f x
                            stateF' s')

    member self.Get() : App2<State, 'S, 'S> = 
        State.Inj <| S (fun s -> (s, s))
    member self.Update (newState : 'S) : App2<State, 'S, unit> = 
        State.Inj <| S (fun s -> ((), newState))

type ContMonad<'R>() = 
    inherit Monad<App<Cont, 'R>>() with
    override self.Return x = Cont.Inj <| C (fun k -> k x)
    override self.Bind (m, f) = 
        Cont.Inj <| C (fun k -> 
                            let (C contF) = Cont.Prj m
                            contF (fun x -> 
                                        let (C contF') = Cont.Prj <| f x
                                        contF' k))
    
