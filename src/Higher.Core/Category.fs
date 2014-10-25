namespace Higher.Core

// Category Class 
[<AbstractClass>]
type Category<'F>() = 
    abstract Ident<'A> : unit -> App2<'F, 'A, 'A>
    abstract Compose<'A, 'B, 'C> : App2<'F, 'A, 'B> -> App2<'F, 'B, 'C> -> App2<'F, 'A, 'C>