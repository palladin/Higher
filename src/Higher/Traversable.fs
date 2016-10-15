namespace Higher

/// Traversable Class
[<AbstractClass>]
type Traversable<'TR>() =
    inherit Functor<'TR>()
    abstract Traverse<'F, 'T, 'R> : Applicative<'F> -> ('T -> App<'F, 'R>) -> App<'TR, 'T> -> App<'F, App<'TR, 'R>>
    override self.Map<'T, 'R> (f : 'T -> 'R) (func : App<'TR, 'T>) : App<'TR, 'R> =
        Identity.Run <| self.Traverse<Identity, 'T, 'R> (new IdentityApplicative()) (Identity.Inj << Id << f) func
    member self.SequenceA applicative trav =
        self.Traverse applicative id trav
