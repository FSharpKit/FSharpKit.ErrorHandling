module ErrorBuilder.Bench.Program

open System.Reflection.Metadata
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open ErrorBuilders

type Benchmarks() =
  [<Literal>]
  let P = 1_000_000_007

  let xs = [1..1_000_000]

  [<Benchmark>]
  member this.NativeFor() =
    let mutable s = 0
    for i in xs do
      s <- (s + i) % P
    s

  [<Benchmark>]
  member this.OptionBuilderFor() =
    Option.build {
      let mutable s = 0
      for i in xs do
        s <- (s + i) % P
      return s
    }

  [<Benchmark>]
  member this.OptionBuilderInlineFor() =
    Option.buildInline {
      let mutable s = 0
      for i in xs do
        s <- (s + i) % P
      return s
    }

  [<Benchmark>]
  member this.OptionBuilderInlineStructExceptionalFor() =
    Option.buildInlineStruct () {
      let mutable s = 0
      for i in xs do
        s <- (s + i) % P
      return s
    }

  [<Benchmark>]
  member this.OptionBuilderValueFor() =
    Option.buildv () {
      let mutable s = 0
      for i in xs do
        s <- (s + i) % P
      return s
    }

  [<Benchmark>]
  member this.OptionBuilderValueExceptionalFor() =
    Option.buildvx () {
      let mutable s = 0
      for i in xs do
        s <- (s + i) % P
      return s
    }

[<EntryPoint>]
let main _ =
  let config =
    let rough = AccuracyMode(MaxRelativeError = 0.1)
    let quickRoughJob = Job("QuickRough", rough, RunMode.Short)

    let c = ManualConfig()
    c.Add(quickRoughJob)

    // Inherit other configs from default.
    ManualConfig.Union(DefaultConfig.Instance, c)

  let _summary = Running.BenchmarkRunner.Run<Benchmarks>(config)
  0
