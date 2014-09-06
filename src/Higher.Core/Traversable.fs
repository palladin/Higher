namespace Higher.Core

// Traversable Class
[<AbstractClass>]
type Traversable<'TR>() = 
    inherit Functor<'TR>() 
    abstract Traverse<'F, 'T, 'R> : Applicative<'F> -> ('T -> App<'F, 'R>) -> App<'TR, 'T> -> App<'F, App<'TR, 'R>>
    member self.SequenceA applicative trav =
        self.Traverse applicative id trav 
