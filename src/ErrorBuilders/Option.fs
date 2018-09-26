namespace ErrorBuilders

open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
  let someUnit = Some ()

  type OptionMinimalBuilder internal () =
    member inline __.Return(x): option<'x> =
      Some x

    member inline __.ReturnFrom(option): option<'x> =
      option

    member inline __.Zero(): option<unit> =
      someUnit

    member inline __.Bind(o, f): option<'x> =
      match o with
      | Some x ->
        f x
      | None ->
        None

    member inline __.Using(x: #IDisposable, f): option<'x> =
      use x = x
      f x

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

    member inline __.While(guard, f): option<unit> =
      let rec loop () =
        if guard () then
          combine (f ()) loop
        else
          Some ()
      loop ()

    member inline __.For(xs: #seq<'x>, f): option<unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          match f enumerator.Current with
          | Some () -> loop ()
          | None -> None
        else
          someUnit
      loop ()

  [<Struct>]
  type OptionFullInlineStructExceptionalBuilder =
    member inline __.Return(x): option<'x> =
      Some x

    member inline __.Return(()): option<unit> =
      someUnit

    member inline __.ReturnFrom(option): option<'x> =
      option

    member inline __.Zero(): option<unit> =
      someUnit

    member inline __.Bind(o, f): option<'x> =
      match o with
      | Some x ->
        f x
      | None ->
        None

    member inline __.Using(r: #IDisposable, f): option<'x> =
      use r = r
      f r

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

    member inline __.While(guard, f): option<unit> =
      let rec loop () =
        if guard () then
          combine (f ()) loop
        else
          someUnit
      loop ()

    member inline __.For(xs: seq<'x>, f): option<unit> =
      try
        for x in xs do
          match f x with
          | Some () -> ()
          | None -> raise (exn())
        someUnit
      with e ->
        None

  [<Struct>]
  type ValueOptionFullInlineStructBuilder =
    member inline __.Return(x): voption<'x> =
      ValueSome x

    member inline __.ReturnFrom(option): voption<'x> =
      option

    member inline __.Zero(): voption<unit> =
      ValueSome ()

    member inline __.Bind(o, f): voption<'x> =
      match o with
      | ValueSome x ->
        f x
      | ValueNone ->
        ValueNone

    member inline __.Using(r: #IDisposable, f): voption<'x> =
      use r = r
      f r

    member inline __.Run(f): voption<'x> = f ()

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
      match o with
      | ValueSome () -> f ()
      | ValueNone -> ValueNone

    member inline __.While(guard, f): voption<unit> =
      let rec loop () =
        if guard () then
          match f () with
          | ValueSome () -> loop ()
          | ValueNone -> ValueNone
        else
          ValueSome ()
      loop ()

    member inline __.For(xs: #seq<'x>, f): voption<unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          match f enumerator.Current with
          | ValueSome () -> loop ()
          | ValueNone -> ValueNone
        else
          ValueSome ()
      loop ()

  [<Struct>]
  type ValueOptionFullInlineStructExceptionalBuilder =
    member inline __.Return(x): voption<'x> =
      ValueSome x

    member inline __.ReturnFrom(option): voption<'x> =
      option

    member inline __.Zero(): voption<unit> =
      ValueSome ()

    member inline __.Bind(o, f): voption<'x> =
      match o with
      | ValueSome x ->
        f x
      | ValueNone ->
        ValueNone

    member inline __.Using(r: #IDisposable, f): voption<'x> =
      use r = r
      f r

    member inline __.Run(f): voption<'x> = f ()

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
      match o with
      | ValueSome () -> f ()
      | ValueNone -> ValueNone

    member inline __.While(guard, f): voption<unit> =
      let rec loop () =
        if guard () then
          match f () with
          | ValueSome () -> loop ()
          | ValueNone -> ValueNone
        else
          ValueSome ()
      loop ()

    member inline __.For(xs: seq<'x>, f): voption<unit> =
      try
        for x in xs do
          match f x with
          | ValueSome () -> ()
          | ValueNone -> raise (exn())
        ValueSome ()
      with _ ->
        ValueNone

  /// Computation expression builder for `Option`.
  /// Unlike `build`, this builder supports restricted features for performance.
  let build' = OptionMinimalBuilder()

  /// Computation expression builder for `Option`.
  /// using computation expression syntax.
  let build = OptionFullBuilder()

  let buildInline = OptionFullInlineBuilder()

  let inline buildInlineStruct() = OptionFullInlineStructExceptionalBuilder()

  let inline buildv() = ValueOptionFullInlineStructBuilder()
  let inline buildvx() = ValueOptionFullInlineStructExceptionalBuilder()
