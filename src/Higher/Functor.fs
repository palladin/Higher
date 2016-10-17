namespace Higher

// Functor base classes

[<AbstractClass>]
type Functor<'F>() =
    abstract Map<'A, 'B> : ('A -> 'B) -> App<'F, 'A> -> App<'F, 'B>

[<AbstractClass>]
type ContraFunctor<'F>() =
    abstract ContraMap<'A, 'B> : ('A -> 'B) -> App<'F, 'B> -> App<'F, 'A>

[<AbstractClass>]
type BiFunctor<'F>() =
    abstract BiMap<'A, 'B, 'C, 'D> : ('A -> 'B) -> ('C -> 'D) -> App2<'F, 'A, 'C> -> App2<'F, 'B, 'D>
    member self.First<'A, 'B, 'C> (f : 'A -> 'B) (fac : App2<'F, 'A, 'C>) : App2<'F, 'B, 'C> =
        self.BiMap f id fac
    member self.Second<'A, 'B, 'C> (f : 'B -> 'C) (fab : App2<'F, 'A, 'B>) : App2<'F, 'A, 'C> =
        self.BiMap id f fab

[<AbstractClass>]
type ProFunctor<'F>() =
    abstract DiMap<'A, 'B, 'C, 'D> : ('C -> 'A) -> ('B -> 'D) -> App2<'F, 'A, 'B> -> App2<'F, 'C, 'D>
    member self.First<'A, 'B, 'C> (f : 'C -> 'A) (fab : App2<'F, 'A, 'B>) : App2<'F, 'C, 'B> =
        self.DiMap f id fab
    member self.Second<'A, 'B, 'D> (f : 'B -> 'D) (fab : App2<'F, 'A, 'B>) : App2<'F, 'A, 'D> =
        self.DiMap id f fab


module FunctorLaws =

  let identity (eq : App<'F, 'A> -> App<'F, 'A> -> bool) (func : Functor<'F>) (fa : App<'F, 'A>) =
    eq (func.Map id fa) fa


module BiFunctorLaws =

  let identity (eq : App2<'F, 'A, 'B> -> App2<'F, 'A, 'B> -> bool) (func : BiFunctor<'F>) (fab : App2<'F, 'A, 'B>) =
    eq (func.BiMap id id fab) fab


module ProFunctorLaws =

  let identity (eq : App2<'F, 'A, 'B> -> App2<'F, 'A, 'B> -> bool) (func : ProFunctor<'F>) (fab : App2<'F, 'A, 'B>) =
    eq (func.DiMap id id fab) fab
