module ErrorBuilders.ResultTests

open System
open Expecto

[<Tests>]
let tests =
  testList "test Result" [
    parameterize "test isOk and isError"
      [
        (Ok 0, true)
        (Error "x", false)
      ]
      (fun (actual, expected) ->
        actual |> Result.isOk |> is expected
        actual |> Result.isError |> not |> is expected
      )

    parameterize "test tryValue and tryError"
      [
        (Ok 0, Some 0, None)
        (Error "x", None, Some "x")
      ]
      (fun (result, valueOption, errorOption) ->
        result |> Result.tryValue |> is valueOption
        result |> Result.tryError |> is errorOption
      )

    test "test defaultValue" {
      Ok 1 |> Result.defaultValue 0 |> is 1
      Error "x" |> Result.defaultValue 0 |> is 0
    }

    test "test defaultError" {
      Ok 1 |> Result.defaultError "x" |> is "x"
      Error "y" |> Result.defaultError "x" |> is "y"
    }

    test "test defaultWith" {
      let count = ref 0
      let f () = count |> incr; 0
      Ok 1 |> Result.defaultWith f |> is 1
      !count |> is 0

      Error "y" |> Result.defaultWith f |> is 0
      !count |> is 1
    }

    test "test defaultWithError" {
      let count = ref 0
      let f () = count |> incr; "x"
      Ok 1 |> Result.defaultWithError f |> is "x"
      !count |> is 1

      Error "y" |> Result.defaultWithError f |> is "y"
      !count |> is 1
    }

    parameterize "test flatten"
      [
        (Ok (Ok 0), Ok 0)
        (Ok (Error "x"), Error "x")
        (Error "x", Error "x")
      ]
      (fun (result, expected) ->
        result |> Result.flatten |> is expected
      )

    parameterize "test flattenError"
      [
        (Ok 0, Ok 0)
        (Error (Ok 0), Ok 0)
        (Error (Error "x"), Error "x")
      ]
      (fun (result, expected) ->
        result |> Result.flattenError |> is expected
      )

    parameterize "test bindError"
      [
        (Ok 0, Ok 0)
        (Error "x", Error "y")
        (Error "y", Ok 0)
      ]
      (fun (result, expected) ->
        let f e = if e = "x" then Error "y" else Ok 0
        result |> Result.bindError f |> is expected
      )

    parameterize "test iter"
      [
        (Ok 0, [|0|], [||])
        (Error "x", [||], [|"x"|])
      ]
      (fun (result, expectedValues, expectedErrors) ->
        // Test iter.
        let values = ResizeArray()
        result |> Result.iter values.Add
        values.ToArray() |> is expectedValues

        // Test iterError.
        let errors = ResizeArray()
        result |> Result.iterError errors.Add
        errors.ToArray() |> is expectedErrors
      )

    parameterize "test exists and forall"
      [
        (Ok 0, true, true)
        (Ok (-1), false, false)
        (Error "x", false, true)
      ]
      (fun (result, expectedExists, expectedForall) ->
        let p x = x >= 0
        result |> Result.exists p |> is expectedExists
        result |> Result.forall p |> is expectedForall
      )

    parameterize "test existsError and forallError"
      [
        (Ok 0, false, true)
        (Error "x", true, true)
        (Error "y", false, false)
      ]
      (fun (result, expectedExists, expectedForall) ->
        let p e = e = "x"
        result |> Result.existsError p |> is expectedExists
        result |> Result.forallError p |> is expectedForall
      )

    testList "test build" [
      test "test return" {
        Result.build {
          return 1
        } |> is (Ok 1)
      }

      test "test return!" {
        Result.build {
          return! Ok 1
        } |> is (Ok 1)

        Result.build {
          return! Error "x"
        } |> is (Error "x")
      }

      testList "test use" [
        test "test completion" {
          let disposable = new CountDisposable()
          Result.build {
            use disposable = disposable
            return disposable.Count
          } |> is (Ok 0)
          disposable.Count |> is 1
        }

        test "test exceptional" {
          let disposable = new CountDisposable()
          let e = trap (fun () ->
            Result.build {
              use disposable = disposable
              exn(disposable.Count |> string) |> raise
            })
          e.Message |> is "0"
          disposable.Count |> is 1
        }
      ]

      testList "test try-with" [
        test "test completion" {
          let disposable = new CountDisposable()
          do
            Result.build {
              try
                return disposable.Count
              with
              | e ->
                disposable.Dispose()
                return! Error e
            } |> is (Ok 0)
          disposable.Count |> is 0
        }

        test "test exceptional" {
          let disposable = new CountDisposable()
          let e =
            Result.build {
              try
                exn(disposable.Count |> string) |> raise
              with
              | e ->
                disposable.Dispose()
                return! Error e
            } |> unwrapError
          e.Message |> is "0"
          disposable.Count |> is 1
        }
      ]

      testList "test try-finally" [
        test "test completion" {
          let disposable = new CountDisposable()
          do
            Result.build {
              try
                return disposable.Count
              finally
                disposable.Dispose()
            } |> is (Ok 0)
          disposable.Count |> is 1
        }

        test "test exceptional" {
          let disposable = new CountDisposable()
          let e = trap (fun () ->
            Result.build {
              try
                exn(disposable.Count |> string) |> raise
              finally
                disposable.Dispose()
            }
          )
          e.Message |> is "0"
          disposable.Count |> is 1
        }
      ]

      testList "test while" [
        test "test completion" {
          let i = ref 0
          let n = 5
          do
            Result.build {
              while !i < n do
                i |> incr
            } |> is (Ok ())
          !i |> is n
        }

        test "test error" {
          let i = ref 0
          let n = 5
          let tryIncrement () =
            let j = !i + 1
            if j < n then Ok j else Error ()
          do
            Result.build {
              while true do
                let! j = tryIncrement ()
                i := j
            } |> is (Error ())
        }
      ]

      testList "test for" [
        test "test completion" {
          let n = 5
          let xs = [|0..(n - 1)|]
          let list = ResizeArray()
          do
            Result.build {
              for x in xs do
                list.Add(x)
            } |> is (Ok ())
          list.ToArray() |> is xs
        }

        test "test error" {
          let n = 5
          let list = ResizeArray()
          do
            Result.build {
              for x in seq {0..10000} do
                do! (if x < n then Ok () else Error x)
                list.Add(x)
            } |> is (Error n)
          list.ToArray() |> is [|0..(n - 1)|]
        }
      ]
    ]
  ]
