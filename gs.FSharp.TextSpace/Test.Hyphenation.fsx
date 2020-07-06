#r "bin/Debug/net45/gs.FSharp.TextSpace.dll"
open gras100.FSharp.TextSpace.Hyphenation
Hyphenation.Fsi.setSourceDirectory __SOURCE_DIRECTORY__


Hyphenation.InserterFactory().GetSpecNewInserterPair(Hyphenation.Languages.EnGb)

let splitSpec, splitWord = Hyphenation.SplitterFactory(Hyphenation.Spec.Defaults).GetSpecNewSplitterPair(Hyphenation.Languages.EnGb)
let insertSpec, insertWordSplitPoints = Hyphenation.InserterFactory(Hyphenation.Spec.Defaults).GetSpecNewInserterPair(Hyphenation.Languages.EnGb)

let splitSpecEs, splitWordEs = Hyphenation.SplitterFactory(Hyphenation.Spec.Defaults).GetSpecNewSplitterPair(Hyphenation.Languages.Es)
splitWordEs "entonces"
splitWordEs "sociopolítico"
splitWordEs "precocinar"
splitWordEs "hispanohablante"
splitWordEs "bienintencionado"

splitWord "antidisestablishmentarianism" |> Seq.toArray = [|"an+"; "ti+"; "dis+"; "es+"; "tab+"; "lish+"; "ment+"; "ari+"; "an+"; "ism"|]
splitWord "multi-faceted" |> Seq.toArray = [| "multi-"; "fa+"; "ceted" |]

insertWordSplitPoints "antidisestablishmentarianism"
insertWordSplitPoints "multi-faceted"

splitWord "many times multi-faceted" // doesn't work currently, split is for word.
insertWordSplitPoints "many times multi-faceted" // doesn't work currently, split is for word.