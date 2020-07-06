#r "bin/Debug/net45/gs.FSharp.TextSpace.dll"
open gras100.FSharp.TextSpace.Hyphenation
open gras100.FSharp.TextSpace.Wrapping
Hyphenation.Fsi.setSourceDirectory __SOURCE_DIRECTORY__

open System.IO
open System.Text

open gras100.FSharp.TextSpace.Hyphenation
open gras100.FSharp.TextSpace.Wrapping
  
let wrappedLinesForParagraphs width (paragraphs:seq<string>) = 
  let spec, hyphenate = Hyphenation.InserterFactory().GetSpecNewInserterPair(Hyphenation.Languages.EnGb)
  paragraphs 
  |> Seq.collect (fun paragraph -> 
      Wrapping.tokens '`' paragraph
      |> Wrapping.components spec.breakChar hyphenate //(hyphenate >> Array.ofSeq)
      |> Wrapping.wrap spec.breakChar width 5
      |> Seq.append [ "" ])

let path = @"D:\dev\repos\gs.Tools\gs.FSharp.ConsoleServices\res\spices.txt"    

let createWrappedFile width (path:string) =
  Wrapping.Diagnostics.loadParagraphs (fun () -> "") path
  |> wrappedLinesForParagraphs width
  |> (fun lines ->
      use writer = new System.IO.StreamWriter(Path.ChangeExtension(path, sprintf @".wrapped.%i.txt" width))
      lines
      |> Seq.iter writer.WriteLine)

#time
createWrappedFile 50 @"D:\dev\repos\gs.Tools\gs.FSharp.ConsoleServices\res\spices.txt"
#time

#time
createWrappedFile 70 @"D:\dev\repos\gs.Tools\gs.FSharp.ConsoleServices\res\spices.txt"
#time

#time
Wrapping.Diagnostics.loadParagraphs (fun () -> "") @"D:\dev\repos\gs.Tools\gs.FSharp.ConsoleServices\res\spices.txt"
|> Array.ofSeq
#time