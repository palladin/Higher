namespace Higher.Core

// Applicative Class 
[<AbstractClass>]
type Applicative<'F>() = 
    inherit Functor<'F>() 
    override self.Map f func = 
        self.Apply (self.Pure f) func
    abstract Pure<'T> : 'T -> App<'F, 'T>
    abstract Apply<'T, 'R> : App<'F, 'T -> 'R> -> App<'F, 'T> -> App<'F, 'R>

            
