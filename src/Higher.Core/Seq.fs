namespace Higher.Core

// Seq Monad type
type Seq private () =
    static let token = new Seq()  
    static member Inj (value : seq<'T>) : App<Seq, 'T> = 
        new App<_, _>(token, value)
    static member Prj (app : App<Seq, 'T>) : seq<'T> = 
        app.Apply(token) :?> _

// Seq Monad instance
type SeqMonad() = 
    inherit Monad<Seq>() with
    override self.Return x = Seq.Inj [x]
    override self.Bind (m, f) =
        m
        |> Seq.Prj 
        |> Seq.collect (fun v -> Seq.Prj (f v))  
        |> Seq.Inj


// Seq Applicative Functor instance
type SeqApplicative() = 
    inherit Applicative<Seq>() with
    override self.Pure x = Seq.Inj [x]
    override self.Apply appF app =
        let fs, xs = Seq.Prj appF, Seq.Prj app
        xs |> Seq.zip fs |> Seq.map (fun (f, x) -> f x) |> Seq.Inj
