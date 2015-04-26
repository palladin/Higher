namespace Higher.Core

// Comonad Class 
[<AbstractClass>]
type Comonad<'W>() = 
    inherit Functor<'W>()
    override self.Map (f : 'A -> 'B) (func : App<'W, 'A>) =
      self.Extend (self.Extract >> f) func
    abstract Extract<'A> : App<'W, 'A> -> 'A
    abstract Extend<'A, 'B> : (App<'W, 'A> -> 'B) -> App<'W, 'A> -> App<'W, 'B>

// Generic comonad functions.
module Comonad =
  
  let duplicate (comonad : Comonad<'W>) (w : App<'W, 'A>) : App<'W, App<'W, 'A>> =
    comonad.Extend id w

  let rec fix (comonad : Comonad<'W>) (w : App<'W, App<'W, 'A> -> 'A>) : 'A =
    comonad.Extend (fix comonad) w |> comonad.Extract