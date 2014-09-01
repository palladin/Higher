namespace Higher.Core
open System 

// Monoid class
[<AbstractClass>]
type Monoid<'T>() =
    abstract Empty : 'T
    abstract Append : 'T -> 'T -> 'T
    
    
// Basic Monoid instances
type StringMonoid() =
    inherit Monoid<string>() with 
    override self.Empty = ""  
    override self.Append x y = x + y


type ListMonoid<'T>() =
    inherit Monoid<'T list>() with 
    override self.Empty = []  
    override self.Append xs ys = List.append xs ys

type SeqMonoid<'T>() =
    inherit Monoid<seq<'T>>() with 
    override self.Empty = Seq.empty   
    override self.Append xs ys = Seq.append xs ys