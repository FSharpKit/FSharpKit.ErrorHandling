module VainZero.FSharpErrorHandling.AsyncResultTests

open System
open System.Threading
open Expecto

let run a = a |> Async.RunSynchronously

let isA expected actualAsync =
  async {
    let! actual = actualAsync
    expected |> is actual
  }

[<Tests>]
let tests =
  testList "test AsyncResult" [
    yield testList "test build" [
      let positive x =
        async {
          do! Async.SwitchToThreadPool()
          return
            if x >= 1
            then Ok x
            else x |> string |> Error
        }

      let defaultThreadId = Thread.CurrentThread.ManagedThreadId

      yield testAsync "test return" {
        do! AsyncResult.build { return 1 } |> isA (Ok 1)
      }

      yield testAsync "test return!" {
        do! AsyncResult.build { return! positive 1 } |> isA (Ok 1)
        do! AsyncResult.build { return! positive 0 } |> isA (Error "0")
      }

      yield testAsync "test let!: Success case." {
        do!
          AsyncResult.build {
            let! x = positive 1
            let! y = positive 2
            let! z = Ok 3
            return x + y + z
          } |> isA (Ok 6)
      }

      yield testAsync "test let!: Error case." {
        do!
          AsyncResult.build {
            let! x = positive 1
            let! y = positive 0
            exn() |> raise
            return x + y
          } |> isA (Error "0")
      }

      yield testAsync "test do!" {
        do!
          AsyncResult.build {
            do! Async.SwitchToThreadPool()
            // On a thread from the thread pool.
            if defaultThreadId = Thread.CurrentThread.ManagedThreadId then
              return!
                Error "Unexpectedly on the default thread."
                |> AsyncResult.ofResult
          } |> isA (Ok ())
      }

      yield testList "test use" [
        yield testAsync "completion case" {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              use disposable = disposable
              do! Async.SwitchToThreadPool()
              return disposable.Count
            }
          disposable.Count |> is 0
          let result = ar |> run
          (result, disposable.Count) |> is (Ok 0, 1)
        }

        yield testAsync "exceptional case" {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              use disposable = disposable
              do! Async.SwitchToThreadPool()
              exn(disposable.Count |> string) |> raise
              return disposable.Count
            }
          disposable.Count |> is 0
          let result =
            try
              ar |> run
            with
            | e -> Error e
          let k = (result |> Result.errorOrRaise).Message |> int
          (k, disposable.Count) |> is (0, 1)
        }
      ]

      yield testList "test try-with" [
        yield testAsync "completion case" {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              try
                do! Async.SwitchToThreadPool()
                return disposable.Count
              with
              | e ->
                disposable.Dispose()
                return! e |> AsyncResult.error
            }
          disposable.Count |> is 0
          let result = ar |> run
          (result, disposable.Count) |> is (Ok 0, 0)
        }

        yield testAsync "exceptional case" {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              try
                do! Async.SwitchToThreadPool()
                return exn(disposable.Count |> string) |> raise
              with
              | e ->
                disposable.Dispose()
                return! e |> AsyncResult.error
            }
          disposable.Count |> is 0
          let result =
            try
              ar |> run
            with
            | e -> Error e
          let k = (result |> Result.errorOrRaise).Message |> int
          (k, disposable.Count) |> is (0, 1)
        }
      ]

      yield testList "test try-finally" [
        yield testAsync "completion case" {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              try
                do! Async.SwitchToThreadPool()
                return disposable.Count
              finally
                disposable.Dispose()
            }
          disposable.Count |> is 0
          let result = ar |> run
          (result, disposable.Count) |> is (Ok 0, 1)
        }

        yield testAsync "exceptional case" {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              try
                do! Async.SwitchToThreadPool()
                return exn(disposable.Count |> string) |> raise
              finally
                disposable.Dispose()
            }
          disposable.Count |> is 0
          let result =
            try
              ar |> run
            with
            | e -> Error e
          let k = (result |> Result.errorOrRaise).Message |> int
          (k, disposable.Count) |> is (0, 1)
        }
      ]

      yield testAsync "test Combine" {
        let count = ref 0
        do!
          AsyncResult.build {
            count |> incr
            return !count
          } |> isA (Ok 1)
      }

      yield testList "test while" [
        yield testAsync "completion case" {
          let count = ref 0
          let n = 10
          let counts = ResizeArray()
          let incr =
            async {
              counts.Add(!count)
              count |> incr
              return Ok ()
            }
          do!
            AsyncResult.build {
              while (!count) < n do
                do! incr
            } |> isA (Ok ())
          !count |> is n
          counts.ToArray() |> is [|0..(n - 1)|]
        }

        yield testAsync "error case" {
          let n = 3
          let tryIncrement x =
            async {
              let y = x + 1
              return if y < n then Ok y else Error ()
            }
          let i = ref 0
          let values = ResizeArray()
          do!
            AsyncResult.build {
              while true do
                values.Add(!i)
                let! j = !i |> tryIncrement
                i := j
            } |> isA (Error ())
          values.ToArray() |> is [|0..(n - 1)|]
        }
      ]

      yield testList "test for" [
        yield testAsync "completion case" {
          let n = 5
          let advanceCount = ref 0
          let xs =
            seq {
              for i in 0..(n - 1) do
                advanceCount |> incr
                yield i
            }
          let values = ResizeArray()
          do!
            AsyncResult.build {
              for x in xs do
                values.Add((x, !advanceCount))
            } |> isA (Ok ())
          values.ToArray() |> is [|for i in 0..(n - 1) -> (i, i + 1)|]
        }
      ]
    ]
  ]
