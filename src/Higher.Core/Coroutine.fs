namespace Higher.Core

type Coroutine<'S,'M,'R> = 
  Resume of App<'M,Choice<App<'S,Coroutine<'S,'M,'R>>,'R>>

type Coroutine private () =
  static let token = new Coroutine()
  static member Inj (value: Coroutine<'S,'M,'R>) : App3<Coroutine,'S,'M,'R> =
    let app = new App<Coroutine,'S>(token,value)
    let app2 = new App2<Coroutine,'S,'M>(AppToken<Coroutine,'S>.Token token, app)
    new App3<Coroutine,'S,'M,'R>(AppToken<App<Coroutine,'S>,'M>.Token app, app2)

  static member Prj (app3: App3<Coroutine,'S,'M,'R>) : Coroutine<'S,'M,'R> = 
    let token' = AppToken<Coroutine,'S>.Token token
    let token'' = AppToken<App<Coroutine,'S>,'M>.Token token'
    let app2 = app3.Apply(token'') :?> App2<Coroutine,'S,'M>
    let app = app2.Apply(token') :?> App<Coroutine,'S>
    app.Apply(token) :?> _

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Coroutine =

  let inline suspend
    (monadM : Monad<'M>) (functorS: Functor<'S>) 
    (s: App<'S,Coroutine<'S,'M,'R>>) 
    : Coroutine<'S,'M,'R> = 
    
    Resume (monadM.Return <| Choice1Of2 s)

  let inline resume (Resume cr) : App<'M,Choice<App<'S,Coroutine<'S,'M,'R>>,'R>> = cr


//type CoroutineFunctor<'S,'M>
//  (functorS : Functor<'S>, functorM : Functor<'M>) = 
//  inherit Functor<App2<Coroutine,'S,'M>>()

//  override self.Map 
//    (f: 'A -> 'B) 
//    (app: App3<Coroutine, 'S, 'M, 'A>) 
//    : App3<Coroutine,'S,'M,'B> = 

//    CR (functorM.Map (apply f) (resume t)


type SomeFunctor<'L,'R,'T> 
  = LeftSome of App<'L,'T>
  | RightSome of App<'R,'T>
  | Both of App3<Compose,'L,'R,'T>

