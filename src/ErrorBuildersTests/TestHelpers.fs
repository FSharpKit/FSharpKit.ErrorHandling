namespace ErrorBuilders

open System

[<AutoOpen>]
module TestHelpers =
  open Expecto

  /// Tests two values are equal.
  let inline is<'T when 'T : equality> (expected: 'T) (actual: 'T): unit =
    Expect.equal actual expected "These should equal"

  /// Tests the specified function raises an exception
  /// and gets it.
  let trap<'T> (f: unit -> 'T): exn =
    try
      f () |> ignore
      failtest "It should throw"
    with
    | ex ->
      ex

  /// Tests for each parameter.
  let parameterize (name: string) (parameters: #seq<'T>) (run: 'T -> unit): Test =
    testList name
      [
        for (i, parameter) in parameters |> Seq.indexed do
          let name = sprintf "#%d: %A" (i + 1) parameter
          yield testCase name (fun () -> run parameter)
      ]

  /// Expects the specified result to be an `Error(e)`
  /// and gets `e`.
  let inline unwrapError (r: Result<'T, 'E>): 'E =
    match r with
    | Ok value -> failtestf "Expected an error but Ok: %A" value
    | Error e -> e

/// Represents a resource that counts how many times it's disposed
/// to test exactly-once diposing.
[<Sealed>]
type CountDisposable() =
  let count = ref 0

  member __.Count = !count
  member __.Dispose() = count |> incr

  interface IDisposable with
    override this.Dispose() = this.Dispose()
