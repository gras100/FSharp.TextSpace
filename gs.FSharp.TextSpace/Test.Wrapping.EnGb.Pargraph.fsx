#r "bin/Debug/net45/gs.FSharp.TextSpace.dll"
open gras100.FSharp.TextSpace.Hyphenation
open gras100.FSharp.TextSpace.Wrapping
Hyphenation.Fsi.setSourceDirectory __SOURCE_DIRECTORY__

open gras100.FSharp.TextSpace.Hyphenation
open gras100.FSharp.TextSpace.Wrapping

let paragraphWithLotsOfPuncutationAWebAddressAndForcedBreakPoints = 
  "  Help: the easiest way to access the help information is to go to \
   the website at http://www.gutenberg.org/dirs/6/0/1/9/60192#remark which is \
   full of `fr+iend+ly advice--If advice can be fr+iend+ly--and regularly \
   updated (if you'll believe that). If you don't believe it you should try \
   looking for information on animals `e.g. dogs/cats/elephants anywhere \
   else."

let (enGbSpec:Hyphenation.Spec), enGbSplit =
  Hyphenation.SplitterFactory().GetSpecNewSplitterPair(Hyphenation.Languages.EnGb)
Wrapping.tokens '`' paragraphWithLotsOfPuncutationAWebAddressAndForcedBreakPoints
|> Wrapping.components2  enGbSpec.breakChar (enGbSplit >> Array.ofSeq)
|> Wrapping.wrap enGbSpec.breakChar 48 3
|> Array.ofSeq
|> Wrapping.Diagnostics.printWithRulers (" ", Wrapping.Diagnostics.RulerAtTop) 48

let _, enGbInsert = 
  Hyphenation.InserterFactory().GetSpecNewInserterPair(Hyphenation.Languages.EnGb)
let tokens = Wrapping.tokens '`' paragraphWithLotsOfPuncutationAWebAddressAndForcedBreakPoints
let components = Wrapping.components  enGbSpec.breakChar enGbInsert tokens
let wrapped = Wrapping.wrap enGbSpec.breakChar 48 3 components |> Array.ofSeq
Wrapping.Diagnostics.printWithRulers (" ", Wrapping.Diagnostics.RulerAtTop) 48 wrapped

// Width=41, tolerance=4 advice -> advice.

let printVisualChecks(paragraph:string) = 
  let enGbSpec, enGbSplit = Hyphenation.SplitterFactory().GetSpecNewSplitterPair(Hyphenation.Languages.EnGb)
  let components = 
    paragraph
    |> Wrapping.tokens '`' 
    |> Wrapping.components2  enGbSpec.breakChar (enGbSplit >> Array.ofSeq)
  [ for i in 20..5..70 do for j in 5..-1..4 do yield (i, j) ]
  |> Seq.iter (fun (i, j) ->
      printfn "Width=%i, tolerance=%i" i j
      Wrapping.wrap enGbSpec.breakChar i j components
      |> Wrapping.Diagnostics.printWithRulers (" ", Wrapping.Diagnostics.RulerOptions.RulerAtTop) i)

printVisualChecks paragraphWithLotsOfPuncutationAWebAddressAndForcedBreakPoints
