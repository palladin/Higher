namespace Higher.Core

// Monad Transformer Class
[<AbstractClass>]
type MonadTrans<'MT>() =
    abstract Lift<'M, 'T> : Monad<'M> -> App<'M, 'T> -> App2<'MT, 'M, 'T>


// Monad Transformer instances
type OptionMonadTrans() = 
    inherit MonadTrans<OptionT>() with
    override self.Lift monad m = 
        OptionT.Inj <| OT (monad { let! x = m in return Some x })


// Monad Transformer Monad instances
type OptionTMonad<'M>(monad : Monad<'M>) = 
    inherit Monad<App<OptionT, 'M>>() with
    override self.Return x = OptionT.Inj <| OT (monad { return Some x })
    override self.Bind (m, f) = 
        OptionT.Inj <| OT (monad {    
                              let! option = OptionT.UnWrap (OptionT.Prj m)
                              match option with 
                              | Some x -> return! OptionT.UnWrap (OptionT.Prj <| f x)  
                              | None -> return None 
                           })