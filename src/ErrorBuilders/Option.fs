namespace ErrorBuilders

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
  module Internals =
    let inline toValueOption o =
      match o with
      | Some x -> ValueSome x
      | None -> ValueNone

    let inline ofValueOption o =
      match o with
      | ValueSome x -> Some x
      | ValueNone -> None

    let inline combine (o: voption<unit>) (f: unit -> voption<_>): voption<_> =
      match o with
      | ValueSome () ->
        f ()
      | ValueNone ->
        ValueNone

  type OptionMinimalBuilder internal () =
    member inline __.Return(x): voption<'x> =
      ValueSome x

    member inline __.ReturnFrom(o: option<'x>): voption<'x> =
      o |> Internals.toValueOption

    member inline __.Zero(): voption<unit> =
      ValueSome ()

    member inline __.Bind(o, f): voption<'x> =
      match o with
      | Some x ->
        f x
      | None ->
        ValueNone

    member inline __.Using(x, f): voption<'x> =
      use x = x
      f x

  type OptionFullBuilder internal () =
    inherit OptionMinimalBuilder()

    member inline __.Run(f): option<'x> =
      f () |> Internals.ofValueOption

    member inline __.Delay(f): unit -> voption<'x> = f

    member inline __.TryWith(f, h): voption<'x> =
      try
        f ()
      with
      | e -> h e

    member inline __.TryFinally(f, g): voption<'x> =
      try
        f ()
      finally
        g ()

    member inline __.Combine(o, f): voption<'x> =
      Internals.combine o f

    member inline __.While(guard, f): voption<unit> =
      let rec loop () =
        if guard () then
          Internals.combine (f ()) loop
        else
          ValueSome ()
      loop ()

    member inline __.For(xs: seq<'x>, f): voption<unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          Internals.combine (f enumerator.Current) loop
        else
          ValueSome ()
      loop ()

  /// Computation expression builder for `Option`.
  /// Unlike `build`, this builder supports restricted features for performance.
  let build' = OptionMinimalBuilder()

  /// Computation expression builder for `Option`.
  /// using computation expression syntax.
  let build = OptionFullBuilder()
