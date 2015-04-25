namespace Higher.Core

type Algebra<'F, 'A> = App<'F, 'A> -> 'A

type CoAlgebra<'F, 'A> = 'A -> App<'F, 'A>