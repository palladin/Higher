namespace Higher

// http://okmij.org/ftp/Computation/free-monad.html

[<AbstractClass>]
type FFree<'F, 'A>() =
    abstract Invoke<'R> : FFreeUnPack<'F, 'A, 'R> -> 'R

and FPure<'F, 'A>(a : 'A) =
    inherit FFree<'F, 'A>()
    override self.Invoke unpack =
        unpack.Invoke a

and FImpure<'F, 'B, 'A>(app : App<'F, 'B>, f : 'B -> App2<FFree, 'F, 'A>) =
    inherit FFree<'F, 'A>()
    override self.Invoke unpack =
        unpack.Invoke<'B>(app, f)

and FFreeUnPack<'F, 'A, 'R> =
    abstract Invoke : 'A -> 'R
    abstract Invoke<'B> : App<'F, 'B> * ('B -> App2<FFree, 'F, 'A>) -> 'R

and FFree private () =
    static let token = new FFree()
    static member Inj (value : FFree<'F, 'T>) : App2<FFree, 'F, 'T> =
        let app = new App<FFree, 'F>(token, value)
        new App2<FFree, 'F, 'T>(AppToken<FFree, 'F>.Token token, app)
    static member Prj (app2 : App2<FFree, 'F, 'T>) : FFree<'F, 'T> =
        let app = app2.Apply(AppToken<FFree, 'F>.Token token) :?> App<FFree, 'F>
        app.Apply(token) :?> _

/// Freer Monad instance
type FreerMonad<'F>() =
    inherit Monad<App<FFree, 'F>>() with
    override self.Return x = FFree.Inj <| new FPure<'F, 'A>(x)
    override self.Bind (app : App2<FFree, 'F, 'A>, f : 'A -> App2<FFree, 'F, 'B>) : App2<FFree, 'F, 'B> =
        let monad = self
        let ffree = FFree.Prj app
        ffree.Invoke<App2<FFree, 'F, 'B>>
            { new FFreeUnPack<'F, 'A, App2<FFree, 'F, 'B>> with
                member self.Invoke (a : 'A) = f a
                member self.Invoke<'C> (app : App<'F, 'C>, f' : 'C -> App2<FFree, 'F, 'A>) =
                    FFree.Inj <| new FImpure<'F, 'C, 'B>(app, Monad.compose monad f' f)  }
