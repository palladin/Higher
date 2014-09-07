
#r "bin/Release/Higher.Core.dll"
open Higher.Core


let incr (monad : StateTMonad<_, _>) a = monad {
        let! n = monad.Get()
        do! monad.Put (n + 1)
        return n, a
    }

let zipIndex (xs: 'a list) : (int * 'a) list =
    let monad = new StateTMonad<_, _>(new ContMonad<_>())
    let state = Monad.mapM monad (incr monad) xs
    let cont = StateT.UnWrap (StateT.Prj state) 0
    let (C cont) =  Cont.Prj cont
    cont (fun (a, _) -> a)
    
zipIndex [1..10000] 