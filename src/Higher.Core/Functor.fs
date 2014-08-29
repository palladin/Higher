namespace Higher.Core

// Functor Class 
[<AbstractClass>]
type Functor<'F>() = 
    abstract Map<'T, 'R> : ('T -> 'R) -> App<'F, 'T> -> App<'F, 'R>


// Basic Functor instances
type ListFunctor() = 
    inherit Functor<List>() with
        override self.Map f func =
            func
            |> List.Prj 
            |> List.map f  
            |> List.Inj 

type SeqFunctor() = 
    inherit Functor<Seq>() with
        override self.Map f func =
            func
            |> Seq.Prj 
            |> Seq.map f
            |> Seq.Inj 
            
type OptionFunctor() = 
    inherit Functor<Option>() with
        override self.Map f func = 
            func 
            |> Option.Prj 
            |> Option.map f 
            |> Option.Inj
