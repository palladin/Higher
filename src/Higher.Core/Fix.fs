namespace Higher.Core

/// Fix point.
type Fix<'F> = Fix of App<'F, Fix<'F>>

type Fix private () =
  static let token = new Fix()    
  static member Inj (value : Fix<'F>) : App<Fix, 'F> =
    new App<Fix, 'F>(token, value)  
  static member Prj (app2 : App<Fix, 'F>) : Fix<'F> =
    app2.Apply(token) :?> _

type Fix with
  
  static member un (Fix(f)) = f

  static member cata (func : Functor<'F> ) (alg : Algebra<'F, 'A>) (fix : Fix<'F>) : 'A =
    fix
    |> Fix.un
    |> func.Map (Fix.cata func alg)
    |> alg

  static member ana (func : Functor<'F>) (coalg : CoAlgebra<'F, 'A>) (a : 'A) : Fix<'F> =
    a
    |> coalg
    |> func.Map (Fix.ana func coalg)
    |> Fix.Fix