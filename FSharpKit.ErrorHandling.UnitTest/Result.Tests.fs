namespace FSharpKit.ErrorHandling

open System
open FSharpKit.UnitTest

module ``test Result`` =
  let ``test isOk and isError`` =
    let body (actual, expected) =
      test {
        do! actual |> Result.isOk |> is expected
        do! actual |> Result.isError |> not |> is expected
      }
    parameterize {
      case (Ok 0, true)
      case (Error "x", false)
      run body
    }

  let ``test tryValue and tryError`` =
    let body (result, valueOption, errorOption) =
      test {
        do! result |> Result.tryValue |> is valueOption
        do! result |> Result.tryError |> is errorOption
      }
    parameterize {
      case (Ok 0, Some 0, None)
      case (Error "x", None, Some "x")
      run body
    }

  let ``test defaultValue`` =
    test {
      do! Ok 1 |> Result.defaultValue 0 |> is 1
      do! Error "x" |> Result.defaultValue 0 |> is 0
    }

  let ``test defaultError`` =
    test {
      do! Ok 1 |> Result.defaultError "x" |> is "x"
      do! Error "y" |> Result.defaultError "x" |> is "y"
    }

  let ``test defaultWith`` =
    test {
      let count = ref 0
      let f () = count |> incr; 0
      do! Ok 1 |> Result.defaultWith f |> is 1
      do! !count |> is 0

      do! Error "y" |> Result.defaultWith f |> is 0
      do! !count |> is 1
    }

  let ``test defaultWithError`` =
    test {
      let count = ref 0
      let f () = count |> incr; "x"
      do! Ok 1 |> Result.defaultWithError f |> is "x"
      do! !count |> is 1

      do! Error "y" |> Result.defaultWithError f |> is "y"
      do! !count |> is 1
    }

  let ``test valueOrRaise`` =
    test {
      do! Ok 1 |> Result.valueOrRaise |> is 1
      let! e = trap { it (Error "x" |> Result.valueOrRaise) }
      do! e :? InvalidOperationException |> is true
    }

  let ``test errorOrRaise`` =
    test {
      let! e = trap { it (Ok 1 |> Result.errorOrRaise) }
      do! e :? InvalidOperationException |> is true
      do! Error "x" |> Result.errorOrRaise |> is "x"
    }

  let ``test flatten`` =
    let body (result, expected) =
      test {
        do! result |> Result.flatten |> is expected
      }
    parameterize {
      case (Ok (Ok 0), Ok 0)
      case (Ok (Error "x"), Error "x")
      case (Error "x", Error "x")
      run body
    }

  let ``test flattenError`` =
    let body (result, expected) =
      test {
        do! result |> Result.flattenError |> is expected
      }
    parameterize {
      case (Ok 0, Ok 0)
      case (Error (Ok 0), Ok 0)
      case (Error (Error "x"), Error "x")
      run body
    }

  let ``test bindError`` =
    let body (result, expected) =
      test {
        let f e = if e = "x" then Error "y" else Ok 0
        do! result |> Result.bindError f |> is expected
      }
    parameterize {
      case (Ok 0, Ok 0)
      case (Error "x", Error "y")
      case (Error "y", Ok 0)
      run body
    }

  let ``test iter`` =
    let body (result, expectedValues, expectedErrors) =
      test {
        // Test iter.
        let values = ResizeArray()
        result |> Result.iter values.Add
        do! values.ToArray() |> is expectedValues

        // Test iterError.
        let errors = ResizeArray()
        result |> Result.iterError errors.Add
        do! errors.ToArray() |> is expectedErrors
      }
    parameterize {
      case (Ok 0, [|0|], [||])
      case (Error "x", [||], [|"x"|])
      run body
    }

  let ``test exists and forall`` =
    let body (result, expectedExists, expectedForall) =
      test {
        let p x = x >= 0
        do! result |> Result.exists p |> is expectedExists
        do! result |> Result.forall p |> is expectedForall
      }
    parameterize {
      case (Ok 0, true, true)
      case (Ok (-1), false, false)
      case (Error "x", false, true)
      run body
    }

  let ``test existsError and forallError`` =
    let body (result, expectedExists, expectedForall) =
      test {
        let p e = e = "x"
        do! result |> Result.existsError p |> is expectedExists
        do! result |> Result.forallError p |> is expectedForall
      }
    parameterize {
      case (Ok 0, false, true)
      case (Error "x", true, true)
      case (Error "y", false, false)
      run body
    }
