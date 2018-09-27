namespace ErrorBuilders

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
  module Internals =
    let inline combine (o: option<unit>) (f: unit -> option<_>): option<_> =
      match o with
      | Some () ->
        f ()
      | None ->
        None

  type OptionMinimalBuilder internal () =
    member inline __.Return(x): option<'x> =
      Some x

    member inline __.ReturnFrom(option): option<'x> =
      option

    member inline __.Zero(): option<unit> =
      Some ()

    member inline __.Bind(o, f): option<'x> =
      match o with
      | Some x ->
        f x
      | None ->
        None

    member inline __.Using(x, f): option<'x> =
      use x = x
      f x

  type OptionFullBuilder internal () =
    inherit OptionMinimalBuilder()

    member inline __.Run(f): option<'x> = f ()

    member inline __.Delay(f): unit -> option<'x> = f

    member inline __.TryWith(f, h): option<'x> =
      try
        f ()
      with
      | e -> h e

    member inline __.TryFinally(f, g): option<'x> =
      try
        f ()
      finally
        g ()

    member inline __.Combine(o, f): option<'x> =
      Internals.combine o f

    member inline __.While(guard, f): option<unit> =
      let rec loop () =
        if guard () then
          Internals.combine (f ()) loop
        else
          Some ()
      loop ()

    member inline __.For(xs: seq<'x>, f): option<unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          Internals.combine (f enumerator.Current) loop
        else
          Some ()
      loop ()

  /// Computation expression builder for `Option`.
  /// Unlike `build`, this builder supports restricted features for performance.
  let build' = OptionMinimalBuilder()

  /// Computation expression builder for `Option`.
  /// using computation expression syntax.
  let build = OptionFullBuilder()
