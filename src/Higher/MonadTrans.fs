namespace Higher

/// Monad Transformer Class
[<AbstractClass>]
type MonadTrans<'MT>() =
    abstract Lift<'M, 'T> : Monad<'M> -> App<'M, 'T> -> App2<'MT, 'M, 'T>
