namespace Higher

// Source
// https://github.com/ocamllabs/higher/blob/master/examples/example-1-perfect-trees.ml

type Perfect<'T> =
    | Zero of 'T
    | Succ of Perfect<'T * 'T>


type PerfectFolder<'F> =
    abstract Zero<'T> : 'T-> App<'F, 'T>
    abstract Succ<'T> : App<'F, 'T * 'T> -> App<'F, 'T>


module Perfect =
    let rec foldP<'F, 'T> : PerfectFolder<'F> -> Perfect<'T> -> App<'F, 'T> = fun phi p ->
        match p with
        | Zero v -> phi.Zero v
        | Succ p -> phi.Succ (foldP phi p)
