#r "bin/Debug/net45/gs.FSharp.TextSpace.dll"
open gras100.FSharp.TextSpace.Wrapping

open Wrapping.Diagnostics

"Description of flag1:"
|> Wrapping.tokens '~'
|> Wrapping.components '+' id

let testCollectTokenBlocksSplitting() = 
  printfn "Running test << %s >>" "testCollectTokenBlocksSplitting"
  let wrap collectChar (value:string) = 
    value
    |> Wrapping.tokens collectChar
    |> Wrapping.components '+' id
    |> Array.ofSeq

  let egSplit = [|"xxxx "; "xxxx "; "xxxx "; "e."; "g. "; "xxxx "; "xxxx."|]

  let egNotSplit = [|"xxxx "; "xxxx "; "xxxx "; "e.g. "; "xxxx "; "xxxx."|]

  [|
  wrap '`' "xxxx xxxx xxxx e.g. xxxx xxxx." = egSplit
  wrap '`' "xxxx xxxx xxxx `~e.g.~ xxxx xxxx." = egNotSplit // punctuation.
  wrap '`' "xxxx xxxx xxxx `_e.g._ xxxx xxxx." = egNotSplit // underscore
  wrap '`' "xxxx xxxx xxxx `e.g. xxxx xxxx." = egNotSplit // default = space.
  |] 
  |> Array.mapi (fun i result -> i, result)
  |> Array.filter (snd >> (fun result -> result = false))
  |> Array.map (fun (i, _) -> printfn "\tfailed at index << %i >>." i; i)
  |> (fun fails -> 
      if Seq.isEmpty fails then printfn "\tpassed."
      else printfn "\tfailed.")
  printfn "Complete."
  

testCollectTokenBlocksSplitting()