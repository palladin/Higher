namespace Higher.Core

/// Fix point.
type Fix<'F> = Fix of App<'F, Fix<'F>>

module Fix =
  
  let inline un (Fix(f)) = f

  let rec cata (func : Functor<'F> ) (alg : Algebra<'F, 'A>) (fix : Fix<'F>) : 'A =
    fix
    |> un
    |> func.Map (cata func alg)
    |> alg

  let rec ana (func : Functor<'F>) (coalg : CoAlgebra<'F, 'A>) (a : 'A) : Fix<'F> =
    a
    |> coalg
    |> func.Map (ana func coalg)
    |> Fix.Fix

  let hylo (func : Functor<'F>) (alg : Algebra<'F, 'B>) (coalg : CoAlgebra<'F, 'A>) (a : 'A) : 'B =    
    ana func coalg a |> cata func alg

  