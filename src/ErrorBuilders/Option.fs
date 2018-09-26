namespace ErrorBuilders

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
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
      using x f

  type OptionFullBuilder internal () =
    inherit OptionMinimalBuilder()

    member __.Run(f): option<'x> = f ()

    member __.Delay(f): unit -> option<'x> = f

    member __.TryWith(f, h): option<'x> =
      try
        f ()
      with
      | e -> h e

    member __.TryFinally(f, g): option<'x> =
      try
        f ()
      finally
        g ()

    member __.Combine(o, f): option<'x> =
      match o with
      | Some () ->
        f ()
      | None ->
        None

    member this.While(guard, f): option<unit> =
      let rec loop () =
        if guard () then
          this.Combine(f (), loop)
        else
          Some ()
      loop ()

    member this.For(xs: seq<'x>, f): option<unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          this.Combine(f enumerator.Current, loop)
        else
          Some ()
      loop ()

  let inline combine o f: option<'x> =
    match o with
    | Some () -> f ()
    | None -> None

  type OptionFullInlineBuilder internal () =
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
      combine o f

    member inline this.While(guard, f): option<unit> =
      let rec loop () =
        if guard () then
          combine (f ()) loop
        else
          Some ()
      loop ()

    member inline this.For(xs: #seq<'x>, f): option<unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          combine (f enumerator.Current) loop
        else
          Some ()
      loop ()

  /// Computation expression builder for `Option`.
  /// Unlike `build`, this builder supports restricted features for performance.
  let build' = OptionMinimalBuilder()

  /// Computation expression builder for `Option`.
  /// using computation expression syntax.
  let build = OptionFullBuilder()
