namespace ErrorBuilders

open System

[<AutoOpen>]
module TestHelpers =
  open Expecto

  let inline is expected actual =
    Expect.equal actual expected "These should equal"

  let trap f =
    try
      f () |> ignore
      failtest "It should throw"
    with
    | e ->
      e

  let parameterize name parameters run =
    testList name
      [
        for (i, parameter) in parameters |> Seq.indexed do
          let name = sprintf "#%d: %A" (i + 1) parameter
          yield testCase name (fun () -> run parameter)
      ]

[<Sealed>]
type CountDisposable() =
  let count = ref 0

  member __.Count = !count
  member __.Dispose() = count |> incr

  interface IDisposable with
    override this.Dispose() = this.Dispose()
