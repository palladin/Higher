namespace Higher.Core

// Source 
// https://github.com/ocamllabs/higher/blob/master/examples/example-2-leibniz.ml
// http://okmij.org/ftp/Haskell/LeibnizInjective.hs

type Eq<'A, 'B> =
    abstract Invoke<'F> : App<'F, 'A> -> App<'F, 'B>

type Eq private () =
    static let token = new Eq()
    static member Inj (value : Eq<'A, 'B>) : App2<Eq, 'A, 'B> = 
        let app = new App<Eq, 'A>(token, value)
        new App2<Eq, 'A, 'B>(AppToken<Eq, 'A>.Token token, app)
    static member Prj (app2 : App2<Eq, 'A, 'B>) : Eq<'A, 'B> = 
        let app = app2.Apply(AppToken<Eq, 'A>.Token token) :?> App<Eq, 'A>
        app.Apply(token) :?> _

type AppEq<'A, 'B> = App2<Eq, 'A, 'B>

module Leibniz = 

    
    let refl : unit -> AppEq<'A, 'A> = fun () ->
        Eq.Inj <|
            { new Eq<'A, 'A> with
                member self.Invoke<'F> (eq : App<'F, 'A>) = eq }

    let subst : AppEq<'A, 'B> -> App<'F, 'A> -> App<'F, 'B> = fun eq app ->
        (Eq.Prj eq).Invoke app  

    let trans : AppEq<'A, 'B> -> AppEq<'B, 'C> -> AppEq<'A, 'C> = fun ab bc ->
        subst bc ab

    let cast : AppEq<'A, 'B> -> 'A -> 'B = fun eq x ->
        let (Id x) = Identity.Prj (subst eq (Identity.Inj (Id x)))
        x

    let symm : AppEq<'A, 'B> -> AppEq<'B, 'A> = fun ab ->
        let flip = Flip.Inj <| F (refl ()) 
        Flip.Run <| subst ab flip
        