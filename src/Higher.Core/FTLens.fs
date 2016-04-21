namespace Higher.Core

// Twan van Laarhoven's Functor transformer lenses
// http://twanvl.nl/files/lenses-talk-2011-05-17.pdf

// type FTLens α β γ δ = ∀f . Functor f ⇒ (α → f β) → (γ → f δ)


type FTLens<'S, 'T, 'A, 'B> = 
    abstract Apply<'F> : Functor<'F> -> ('A -> App<'F, 'B>) -> ('S -> App<'F, 'T>)



module Lens =

    let lens<'S, 'T, 'A, 'B> (get: 'S -> 'A) (set: 'B -> 'S -> 'T) : FTLens<'S, 'T, 'A, 'B> =
        {   new FTLens<'S, 'T, 'A, 'B> with
                override self.Apply<'F> (F : Functor<'F>) (f : 'A -> App<'F, 'B>) = 
                    fun s -> F.Map (fun x -> set x s) (f (get s))  }

    let view<'S, 'T, 'A, 'B> (lens : FTLens<'S, 'T, 'A, 'B>) : 'S -> 'A =
        let F = new ConstFunctor<'A>()
        let f = lens.Apply F (C >> Const.Inj)
        fun a -> let (C k) = Const.Prj (f a) in k 

    let over<'S, 'T, 'A, 'B> (lens : FTLens<'S, 'T, 'A, 'B>) (f : 'A -> 'B) : 'S -> 'T  =
        let F = new IdentityFunctor()
        let f' = lens.Apply F (f >> Id >> Identity.Inj) 
        fun a -> let (Id v) = Identity.Prj (f' a) in v

    let set (lens : FTLens<'S, 'T, 'A, 'B>) (b : 'B) : 'S -> 'T = over lens (fun _ -> b)

    let (>->) (l1: FTLens<_, _, _, _>) (l2: FTLens<_, _, _, _>) =
        {   new FTLens<_, _, _, _> with
                override t.Apply F f = l1.Apply F (l2.Apply F f)}
        
        

