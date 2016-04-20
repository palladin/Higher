namespace Higher.Core

// Twan van Laarhoven's Functor transformer lenses
// http://twanvl.nl/files/lenses-talk-2011-05-17.pdf

// type FTLens α β = ∀f . Functor f ⇒ (β → f β) → (α → f α)


type FTLens<'A, 'B> = 
    abstract Apply<'F> : Functor<'F> -> ('B -> App<'F, 'B>) -> ('A -> App<'F, 'A>)



module Lens =

    let lens<'A, 'B> (get: 'A -> 'B) (set: 'B -> 'A -> 'A) : FTLens<'A, 'B> =
        {   new FTLens<'A, 'B> with
                override self.Apply<'F> (F : Functor<'F>) (f : 'B -> App<'F, 'B>) = 
                    fun s -> F.Map (fun x -> set x s) (f (get s))  }

    let view (lens : FTLens<'A, 'B>) : 'A -> 'B =
        let F = new ConstFunctor<'B>()
        let f = lens.Apply F (C >> Const.Inj)
        fun a -> let (C k) = Const.Prj (f a) in k 

    let over (lens : FTLens<'A, 'B>) (f : 'B -> 'B) : 'A -> 'A  =
        let F = new IdentityFunctor()
        let f' = lens.Apply F (f >> Id >> Identity.Inj) 
        fun a -> let (Id v) = Identity.Prj (f' a) in v

    let set (lens : FTLens<'A, 'B>) (b : 'B) : 'A -> 'A = over lens (fun _ -> b)

    let (>->) (l1: FTLens<_, _>) (l2: FTLens<_, _>) =
        {   new FTLens<_, _> with
                override t.Apply F f = l1.Apply F (l2.Apply F f)}
        
        

