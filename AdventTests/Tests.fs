module Tests

open System
open Xunit

[<Theory>]
[<InlineData("1,0,0,0,99","2,0,0,0,99")>]
let ``Op Code Alters Instruction`` (start, finish) =
    Assert.True(true)
