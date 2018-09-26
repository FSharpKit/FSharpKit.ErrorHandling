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

  [<Params(1_000_000)>]
  member val N = 0 with get, set

  [<Benchmark>]
  member this.NativeFor() =
    let mutable s = 0
    for i in 1..this.N do
      s <- (s + i) % P
    s

  [<Benchmark>]
  member this.OptionBuilderFor() =
    Option.build {
      let mutable s = 0
      for i in 0..this.N do
        s <- (s + i) % P
      return s
    }

  [<Benchmark>]
  member this.OptionMimBuilderFor() =
    Option.build' {
      let mutable s = 0
      for i in 0..this.N do
        s <- (s + i) % P
      return s
    }

  [<Benchmark>]
  member this.OptionBuilderInlineFor() =
    Option.buildInline {
      let mutable s = 0
      for i in 0..this.N do
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
