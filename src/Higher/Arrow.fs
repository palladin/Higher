namespace Higher

[<AbstractClass>]
type Arrow<'F>() =
  inherit Category<'F>()
    abstract Arr : ('A -> 'B) -> App2<'F, 'A, 'B>
    abstract First : App2<'F, 'A, 'B> -> App2<'F, 'A * 'C, 'B * 'C>
    member self.MapIn (f : 'C -> 'A) (ab : App2<'F, 'A, 'B>) : App2<'F, 'C, 'B> =
      self.Compose (self.Arr f) ab
    member self.MapOut (f : 'B -> 'C) (ab : App2<'F, 'A, 'B>) : App2<'F, 'A, 'C> =
      self.Compose ab (self.Arr f)
    member self.Second (ab : App2<'F, 'A, 'B>) : App2<'F, 'C * 'A, 'C * 'B> =
      let swap (a,b) = b,a in
      self.First ab |> self.MapIn swap |> self.MapOut swap
    member self.Split (a : App2<'F, 'A, 'B>) (b : App2<'F, 'C, 'D>) : App2<'F, 'A * 'C, 'B * 'D> =
      self.First a |> self.Compose (self.Second b)
    member self.Fanout (a : App2<'F, 'A, 'B>) (b : App2<'F, 'A, 'C>) : App2<'F, 'A, 'B * 'C> =
      self.Split a b |> self.MapIn (fun a -> a,a)
