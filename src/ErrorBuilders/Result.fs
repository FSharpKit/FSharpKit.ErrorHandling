namespace ErrorBuilders

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
  /// Gets a value indicating whether the result has a successful value.
  let isOk (result: Result<'x, 'e>): bool =
    match result with
    | Ok _ ->
      true
    | Error _ ->
      false

  /// Gets a value indicating whether the result has an error.
  let isError (result: Result<'x, 'e>): bool =
    match result with
    | Ok _ ->
      false
    | Error _ ->
      true

  /// Tries to get the successful value from the result.
  let tryValue (result: Result<'x, 'e>): option<'x> =
    match result with
    | Ok x ->
      Some x
    | Error _ ->
      None

  /// Tries to get the error from the result.
  let tryError (result: Result<'x, 'e>): option<'e> =
    match result with
    | Ok _ ->
      None
    | Error e ->
      Some e

  /// Gets the successful value from the result if able.
  /// Returns the specified value otherwise.
  let defaultValue (value: 'x) (result: Result<'x, 'e>): 'x =
    match result with
    | Ok x ->
      x
    | Error _ ->
      value

  /// Gets the error from the result if able.
  /// Returns the specified error otherwise.
  let defaultError (error: 'e) (result: Result<'x, 'e>): 'e =
    match result with
    | Ok _ ->
      error
    | Error e ->
      e

  /// Gets the successful value from the result if able.
  /// Invokes the specified function otherwise.
  let defaultWith (getValue: unit -> 'x) (result: Result<'x, 'e>): 'x =
    match result with
    | Ok x ->
      x
    | Error _ ->
      getValue ()

  /// Gets the error from the result if able.
  /// Invokes the specified function otherwise.
  let defaultWithError (getError: unit -> 'e) (result: Result<'x, 'e>): 'e =
    match result with
    | Ok _ ->
      getError ()
    | Error e ->
      e

  /// Flattens the result with a nested value type.
  let flatten (result: Result<Result<'x, 'e>, 'e>): Result<'x, 'e> =
    match result with
    | Ok (Ok x) ->
      Ok x
    | Ok (Error e) ->
      Error e
    | Error e ->
      Error e

  /// Flattens the result with a nested error type.
  let flattenError (result: Result<'x, Result<'x, 'e>>): Result<'x, 'e> =
    match result with
    | Ok x ->
      Ok x
    | Error (Ok x) ->
      Ok x
    | Error (Error e) ->
      Error e

  /// Maps the error of the result with the specified function if it has an error.
  /// Returns the given result otherwise.
  let bindError (f: 'e -> Result<'x, 'f>) (result: Result<'x, 'e>): Result<'x, 'f> =
    match result with
    | Ok x ->
      Ok x
    | Error e ->
      f e

  /// If the result has a successful value, invokes the specified function.
  let iter (f: 'x -> unit) (result: Result<'x, 'e>): unit =
    match result with
    | Ok x ->
      f x
    | Error _ ->
      ()

  /// If the result has an error, invokes the specified function.
  let iterError (f: 'e -> unit) (result: Result<'x, 'e>): unit =
    match result with
    | Ok _ ->
      ()
    | Error e ->
      f e

  /// Gets a value indicating whether the result has a successful value which satisfies the predicate.
  let exists (p: 'x -> bool) (result: Result<'x, 'e>): bool =
    match result with
    | Ok x ->
      p x
    | Error _ ->
      false

  /// Gets a value indicating whether the result has an error which satisfies the predicate.
  let existsError (p: 'e -> bool) (result: Result<'x, 'e>): bool =
    match result with
    | Ok x ->
      false
    | Error e ->
      p e

  /// Gets a value indicating whether
  /// if the result has a successful value then it satisfies the predicate.
  let forall (p: 'x -> bool) (result: Result<'x, 'e>): bool =
    match result with
    | Ok x ->
      p x
    | Error _ ->
      true

  /// Gets a value indicating whether
  /// if the result has an error then it satisfies the predicate.
  let forallError (p: 'e -> bool) (result: Result<'x, 'e>): bool =
    match result with
    | Ok x ->
      true
    | Error e ->
      p e

  type ResultMinimalBuilder internal () =
    member inline __.Return(x) =
      Ok x

    member inline __.ReturnFrom(result: Result<_, _>) =
      result

    member inline __.Zero() =
      Ok ()

    member inline __.Bind(m, f) =
      m |> Result.bind f

    member inline __.Using(x, f) =
      use x = x
      f x

  type ResultFullBuilder internal () =
    inherit ResultMinimalBuilder()

    member inline __.Run(f): Result<'x, 'e> = f ()

    member inline __.Delay(f): unit -> Result<'x, 'e> = f

    member inline __.TryWith(f, h): Result<'x, 'e> =
      try
        f ()
      with
      | e -> h e

    member inline __.TryFinally(f, g): Result<'x, 'e> =
      try
        f ()
      finally
        g ()

    member inline __.Combine(r, f): Result<'x, 'e> =
      match r with
      | Ok () ->
        f ()
      | Error e ->
        Error e

    member inline this.While(guard, f): Result<unit, 'e> =
      let rec loop () =
        if guard () then
          this.Combine(f (), loop)
        else
          Ok ()
      loop ()

    member inline this.For(xs: seq<'x>, f): Result<unit, 'e> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          this.Combine(f enumerator.Current, loop)
        else
          Ok ()
      loop ()

  /// Computation expression builder for `Result`.
  /// Unlike `build`, this builder supports restricted features for performance.
  let build' = ResultMinimalBuilder()

  /// Computation expression builder for `Result`.
  let build = ResultFullBuilder()

  type ResultErrorMinimalBuilder internal () =
    member inline __.Return(x) =
      Error x

    member inline __.ReturnFrom(result: Result<_, _>) =
      result

    member inline __.Zero() =
      Error ()

    member inline __.Bind(m, f) =
      m |> bindError f

    member inline __.Using(x, f) =
      use x = x
      f x

  type ResultErrorFullBuilder internal () =
    inherit ResultErrorMinimalBuilder()

    member inline __.Run(f): Result<'x, 'e> = f ()

    member inline __.Delay(f): unit -> Result<'x, 'e> = f

    member inline __.TryWith(f, h): Result<'x, 'e> =
      try
        f ()
      with
      | e -> h e

    member inline __.TryFinally(f, g): Result<'x, 'e> =
      try
        f ()
      finally
        g ()

    member inline __.Combine(r, f): Result<'x, 'e> =
      match r with
      | Ok x ->
        Ok x
      | Error () ->
        f ()

    member inline this.While(guard, f): Result<'x, unit> =
      let rec loop () =
        if guard () then
          this.Combine(f (), loop)
        else
          Error ()
      loop ()

    member inline this.For(xs: seq<'x>, f): Result<'x, unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          this.Combine(f enumerator.Current, loop)
        else
          Error ()
      loop ()

  /// Computation expression builder for `Result`
  /// with restricted features for performance.
  let buildError' = ResultErrorMinimalBuilder()

  /// Computation expression builder for `Result`.
  /// The computation stops when you "unwrap" a `Ok x` with `let!`
  /// and then the return value is `Ok x`.
  /// `return e` returns `Error e`.
  let buildError = ResultErrorFullBuilder()
