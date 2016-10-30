namespace Higher

type Flip<'F, 'A, 'B> = F of App2<'F, 'B, 'A>

type Flip private () =
    static let token = new Flip()
    static member Inj (value : Flip<'F, 'A, 'B>) : App3<Flip, 'F, 'A, 'B> =
        App3<Flip, 'F, 'A, 'B>.Create(AppToken2<Flip, 'F, 'A>.Token(token), value)
    static member Prj (app3 : App3<Flip, 'F, 'A, 'B>) : Flip<'F, 'A, 'B> =
        app3.Apply(AppToken2<Flip, 'F, 'A>.Token(token)) :?> _
    static member Run (app3 : App3<Flip, 'F, 'A, 'B>) : App2<'F, 'B, 'A> =
        let (F app) = Flip.Prj app3 in app
