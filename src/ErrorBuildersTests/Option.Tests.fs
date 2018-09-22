module ErrorBuilders.OptionTests

open Expecto

[<Tests>]
let tests =
  testList "test Option" [
    testList "test build" [
      test "test return" {
          Option.build {
            return 1
          } |> is (Some 1)
        }

      test "test return!" {
        Option.build {
          return! Some 1
        } |> is (Some 1)
        Option.build {
          return! None
        } |> is None
      }

      testList "test use" [
        test "completion case" {
          let disposable = new CountDisposable()
          let k =
            Option.build {
              use disposable = disposable
              return disposable.Count
            }

          // It shouldn't be disposed before `return`.
          k |> is (Some 0)

          // It should get disposed after `return`.
          disposable.Count |> is 1
        }

        test "exceptional case" {
          let disposable = new CountDisposable()
          let e = trap (fun () ->
            Option.build {
              use disposable = disposable
              exn(disposable.Count |> string) |> raise
            })
          e.Message |> is "0"
          disposable.Count |> is 1
        }
      ]

      testList "test try-with" [
        // Almost same as `use`.

        test "completion case" {
          let disposable = new CountDisposable()
          Option.build {
            try
              return disposable.Count
            with
            | _ ->
              disposable.Dispose()
              return! None
          } |> is (Some 0)
          disposable.Count |> is 0
        }

        test "exceptional case" {
          let disposable = new CountDisposable()
          let e =
            Option.build {
              try
                return! exn(disposable.Count |> string) |> raise
              with
              | e ->
                disposable.Dispose()
                return! Some e
            } |> Option.get
          e.Message |> is "0"
          disposable.Count |> is 1
        }
      ]

      testList "test try-finally" [
        // Almost same as `use`.

        test "completion case" {
          let disposable = new CountDisposable()
          Option.build {
            try
              return disposable.Count
            finally
              disposable.Dispose()
          } |> is (Some 0)
          disposable.Count |> is 1
        }

        test "exceptional case" {
          let disposable = new CountDisposable()
          let e = trap (fun () ->
            Option.build {
              try
                exn(disposable.Count |> string) |> raise
              finally
                disposable.Dispose()
            })
          e.Message |> is "0"
          disposable.Count |> is 1
        }
      ]

      testList "test while" [
        test "completion case" {
          let i = ref 0
          let n = 5
          Option.build {
            while !i < n do
              i |> incr
          } |> is (Some ())
          !i |> is n
        }

        test "error case" {
          let i = ref 0
          let n = 5
          let tryIncrement () =
            let j = !i + 1
            if j < n then Some j else None
          Option.build {
            while true do
              // If we bind `None`, the loop should stop.
              let! j = tryIncrement ()
              i := j
          } |> is None
        }
      ]

      testList "test for" [
        test "completion case" {
          let n = 5
          let xs = [|0..(n - 1)|]
          let list = ResizeArray()
          Option.build {
            for x in xs do
              list.Add(x)
          } |> is (Some ())
          list.ToArray() |> is xs
        }

        test "error case" {
          let n = 5
          let list = ResizeArray()
          Option.build {
            for x in seq {0..10000} do
              do! (if x < n then Some () else None)
              list.Add(x)
          } |> is None
          list.ToArray() |> is [|0..(n - 1)|]
        }
      ]
    ]
  ]
