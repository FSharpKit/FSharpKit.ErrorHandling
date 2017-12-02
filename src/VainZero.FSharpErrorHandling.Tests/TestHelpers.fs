namespace VainZero.FSharpErrorHandling

open System

[<AutoOpen>]
module TestHelpers =
  open Expecto

  let is expected actual =
    Expect.equal actual expected "These should equal."

  let trap f =
    try
      f () |> ignore
      failtest "It should throw."
    with
    | e ->
      e

[<Sealed>]
type CountDisposable() =
  let count = ref 0

  member __.Count = !count
  member __.Dispose() = count |> incr

  interface IDisposable with
    override this.Dispose() = this.Dispose()
