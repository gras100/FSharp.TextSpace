#r "bin/Debug/net45/gs.FSharp.TextSpace.dll"
open gras100.FSharp.TextSpace.Hyphenation
open gras100.FSharp.TextSpace.Wrapping
Hyphenation.Fsi.setSourceDirectory __SOURCE_DIRECTORY__

open System
open System.IO
open System.Text
open System.Diagnostics

open gras100.FSharp.TextSpace.Hyphenation
open gras100.FSharp.TextSpace.Wrapping

module ComponentsVsComponents2 =
  

  (*
      Timings to check impact of hyphenation function timing + InserterFactory (components) or SplitterFactory (components2) based
      components function. 
      Conclusions: 
       o Timing of hyphenation function can be ignored as insignificant.
       o Timing difference between InserterFactory vs SplitterFactory in single paragraph test, however gen1 garbage collection 
          considerably higher (factor of 100), so SplitterFactory preferred.

  *)

  [<ObsoleteAttribute("Use SplitterFactory/components2 functions.")>]
  let timeInserterBasedHyphenation repeats width paragraph = 
    let spec, hyphenate = Hyphenation.InserterFactory().GetSpecNewInserterPair(Hyphenation.Languages.EnGb)
    let totalTime = Stopwatch()
    let hyphenationTime = Stopwatch()
    let timedHyphenate value =
      hyphenationTime.Start()
      let result = hyphenate value
      hyphenationTime.Stop()
      result
    let processOnce() = 
      Wrapping.tokens '`' paragraph
      |> Wrapping.components spec.breakChar timedHyphenate //(hyphenate >> Array.ofSeq)
      |> Wrapping.wrap spec.breakChar width 5
      |> Seq.iter (fun sq -> ignore sq)
    processOnce() // warm-up.
    totalTime.Start()
    for i in 0 .. repeats do
      processOnce()
    totalTime.Stop()
    printfn "Time spent hyphenating: %s (%s)" (hyphenationTime.Elapsed.ToString("ss\.fff")) (System.TimeSpan(hyphenationTime.Elapsed.Ticks/100L).ToString("ss\.fff"))
    printfn "Total time: %s (%s)" (totalTime.Elapsed.ToString("ss\.fff")) (System.TimeSpan(hyphenationTime.Elapsed.Ticks/100L).ToString("ss\.fff"))
    printfn "Ratio: %f%%" ((float hyphenationTime.ElapsedMilliseconds/float totalTime.ElapsedMilliseconds) * 100.0)

  [<ObsoleteAttribute("Use SplitterFactory/components2 functions.")>]
  let timeInserterBasedHyphenationNoHyphenationTime repeats width paragraph = 
    let spec, hyphenate = Hyphenation.InserterFactory().GetSpecNewInserterPair(Hyphenation.Languages.EnGb)
    let totalTime = Stopwatch()
    let hyphenationTime = Stopwatch()
    let processOnce() =
      Wrapping.tokens '`' paragraph
      |> Wrapping.components spec.breakChar hyphenate //(hyphenate >> Array.ofSeq)
      |> Wrapping.wrap spec.breakChar width 5
      |> Seq.iter (fun sq -> ignore sq)
    processOnce() // warm-up.
    totalTime.Start()
    for i in 0 .. repeats do
      processOnce()
    totalTime.Stop()
    printfn "Time spent hyphenating: %s (%s)" (hyphenationTime.Elapsed.ToString("ss\.fff")) (System.TimeSpan(hyphenationTime.Elapsed.Ticks/100L).ToString("ss\.fff"))
    printfn "Total time: %s (%s)" (totalTime.Elapsed.ToString("ss\.fff")) (System.TimeSpan(hyphenationTime.Elapsed.Ticks/100L).ToString("ss\.fff"))
    printfn "Ratio: %f%%" ((float hyphenationTime.ElapsedMilliseconds/float totalTime.ElapsedMilliseconds) * 100.0)

  let timeSplitterBasedHyphenation repeats width paragraph = 
    let spec, hyphenate = Hyphenation.SplitterFactory().GetSpecNewSplitterPair(Hyphenation.Languages.EnGb)
    let totalTime = Stopwatch()
    let hyphenationTime = Stopwatch()
    let timedHyphenate value =
      hyphenationTime.Start()
      let result = Array.ofSeq (hyphenate value)
      hyphenationTime.Stop()
      result
    let processOnce() = 
      Wrapping.tokens '`' paragraph
      |> Wrapping.components2 spec.breakChar timedHyphenate //(hyphenate >> Array.ofSeq)
      |> Wrapping.wrap spec.breakChar width 5
      |> Seq.iter (fun sq -> ignore sq)
    processOnce() // warm-up.
    totalTime.Start()
    for i in 0 .. repeats do
      processOnce()
    totalTime.Stop()
    printfn "Time spent hyphenating: %s (%s)" (hyphenationTime.Elapsed.ToString("ss\.fff")) (System.TimeSpan(hyphenationTime.Elapsed.Ticks/100L).ToString("ss\.fff"))
    printfn "Total time: %s (%s)" (totalTime.Elapsed.ToString("ss\.fff")) (System.TimeSpan(hyphenationTime.Elapsed.Ticks/100L).ToString("ss\.fff"))
    printfn "Ratio: %f%%" ((float hyphenationTime.ElapsedMilliseconds/float totalTime.ElapsedMilliseconds) * 100.0)

  [<ObsoleteAttribute("Use SplitterFactory/components2 functions.")>]
  let timeSplitterComponents repeats width paragraph = 
    let spec, hyphenate = Hyphenation.SplitterFactory().GetSpecNewSplitterPair(Hyphenation.Languages.EnGb)
    let totalTime = Stopwatch()
    let hyphenationTime = Stopwatch()
    let timedHyphenate value =
      hyphenationTime.Start()
      let result = Array.ofSeq (hyphenate value)
      hyphenationTime.Stop()
      result
    let processOnce() = 
      Wrapping.tokens '`' paragraph
      |> Wrapping.components2 spec.breakChar timedHyphenate //(hyphenate >> Array.ofSeq)
      |> Wrapping.wrap spec.breakChar width 5
      |> Seq.iter (fun sq -> ignore sq)
    processOnce() // warm-up.
    totalTime.Start()
    for i in 0 .. repeats do
      processOnce()
    totalTime.Stop()
    printfn "Time spent hyphenating: %s (%s)" (hyphenationTime.Elapsed.ToString("ss\.fff")) (System.TimeSpan(hyphenationTime.Elapsed.Ticks/100L).ToString("ss\.fff"))
    printfn "Total time: %s (%s)" (totalTime.Elapsed.ToString("ss\.fff")) (System.TimeSpan(hyphenationTime.Elapsed.Ticks/100L).ToString("ss\.fff"))
    printfn "Ratio: %f%%" ((float hyphenationTime.ElapsedMilliseconds/float totalTime.ElapsedMilliseconds) * 100.0)

let path = @"D:\dev\repos\gs.Tools\gs.FSharp.ConsoleServices\res\spices.longest.paragraph.txt"
let longParagraph = Seq.head (Wrapping.Diagnostics.loadParagraphs (fun () -> "") path)

let steps = 200
let width = 50
#time
ComponentsVsComponents2.timeInserterBasedHyphenation steps width longParagraph
#time

#time
ComponentsVsComponents2.timeInserterBasedHyphenationNoHyphenationTime steps width longParagraph
#time

#time
ComponentsVsComponents2.timeSplitterBasedHyphenation steps width longParagraph
#time

#time
ComponentsVsComponents2.timeSplitterComponents steps width longParagraph
#time