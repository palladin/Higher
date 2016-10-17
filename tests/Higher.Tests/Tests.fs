module Higher.Tests

open NUnit.Framework

[<Test>]
let ``Cathegory function``() =
    // Arrange
    let f = Fun.Inj (fun x -> x + 1)
    let g = Fun.Inj (fun (x : int) -> string x)
    let category = new FunCategory()

    // Act
    let h = category.Compose f g
    let actual = Fun.Prj h 1

    // Assert
    let expected = "2"
    Assert.AreEqual(expected, actual)
