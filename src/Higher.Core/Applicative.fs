namespace Higher.Core

// Applicative Class 
[<AbstractClass>]
type Applicative<'F>() = 
    inherit Functor<'F>() 
        override self.Map f func = 
            self.Apply (self.Pure f) func
    abstract Pure<'T> : 'T -> App<'F, 'T>
    abstract Apply<'T, 'R> : App<'F, 'T -> 'R> -> App<'F, 'T> -> App<'F, 'R>



// Basic Applicative instances
type ListApplicative() = 
    inherit Applicative<List>() with
        override self.Pure x = List.Inj [x]
        override self.Apply appF app =
            let fs, xs = List.Prj appF, List.Prj app
            xs |> List.zip fs |> List.map (fun (f, x) -> f x) |> List.Inj


type SeqApplicative() = 
    inherit Applicative<Seq>() with
        override self.Pure x = Seq.Inj [x]
        override self.Apply appF app =
            let fs, xs = Seq.Prj appF, Seq.Prj app
            xs |> Seq.zip fs |> Seq.map (fun (f, x) -> f x) |> Seq.Inj
            
