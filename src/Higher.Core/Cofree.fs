namespace Higher.Core

/// Cofree comonad - an 'A stream with branching factor 'F.
type Cofree<'F, 'A> = Cofree of 'A * App<'F, Cofree<'F, 'A>>

type Cofree private () =
  static let token = new Cofree()  
  static member Inj (value : Cofree<'F, 'A>) : App2<Cofree, 'F, 'A> =
    let app = new App<Cofree, 'F>(token, value)
    new App2<Cofree, 'F, 'A>(AppToken<Cofree, 'F>.Token token, app)  
  static member Prj (app2 : App2<Cofree, 'F, 'A>) : Cofree<'F, 'A> =
    let app = app2.Apply(AppToken<Cofree, 'F>.Token token) :?> App<Cofree, 'A>
    app.Apply(token) :?> _

type CofreeComonad<'F>(func : Functor<'F>) =
  inherit Comonad<App<Cofree, 'F>>() with
  override self.Extract (c : App2<Cofree, 'F, 'A>) : 'A = 
    let (Cofree(a,_)) = Cofree.Prj c in a
  override self.Extend (f : App2<Cofree, 'F, 'A> -> 'B) (fa : App2<Cofree, 'F, 'A>) : App2<Cofree, 'F, 'B> =
    let (Cofree(a, ffa)) = Cofree.Prj fa
    let ffb = func.Map (fun fa -> self.Extend f (Cofree.Inj fa) |> Cofree.Prj) ffa
    Cofree.Cofree(f fa, ffb) |> Cofree.Inj

type Cofree with
  
  static member inline head (Cofree(a,_)) = a

  static member inline tail (Cofree(_,tl)) = tl
  
  static member toList (cofree : Cofree<Option, 'A> ) : 'A list =    
    let (Cofree(a, ffa)) = cofree in
    match Option.Prj ffa with
    | Some cofree -> a::(Cofree.toList cofree)
    | None -> [a]

  static member ana (func : Functor<'F>) (f : 'A -> 'B) (g : 'A -> App<'F, 'A>) (a : 'A) : Cofree<'F, 'B> =
    Cofree.Cofree(f a, g a |> func.Map (Cofree.ana func f g))

    
