[<RequireQualifiedAccess>]
module gras100.FSharp.TextSpace.Wrapping.Wrapping

open System.Text
open System.Text.RegularExpressions


module Diagnostics = 

  type RulerOptions = 
    | RulerAtTop
    | RulersThroughout

  let printWithRulers (pad, option:RulerOptions) width lines = 
    let ruler = System.String('-', width)
    printfn "%s%s" pad ruler
    lines
    |> Seq.iter (fun line -> 
        printfn "%s%s" pad line
        if option = RulersThroughout
        then printfn "%s%s" pad ruler)

  /// <summary>
  /// Splits text at the given path into paragraphs based on empty lines.
  /// </summary>
  /// <remarks>
  /// Treats multiple empty lines no differently from single empty lines.
  /// </remarks>
  /// <param name="prefix">Paragraph prefix, such as two spaces.</param>
  /// <param name="path">Path to file containing text to be split into paragraphs.</param>
  let loadParagraphs prefix (path:string) = 
    let paragraphs (prefix:unit -> string) (lines:seq<string>) = seq {
      let mutable prevLine = ""
      let sb = new StringBuilder(5000)
      for line in lines do
        let line = line.Trim()
        match prevLine, line with
        | "", "" -> 
            ()
        | _, "" -> 
            match sb.ToString().Trim('.', ' ') with 
            | "" -> ()
            | para -> yield prefix() + para + "."
            sb.Clear() |> ignore
        | _, _ ->
            sb.Append(" ").Append(line) |> ignore
            prevLine <- line
      if sb.Length > 0
      then yield sb.ToString().Trim('.', ' ') + "."
      }
    System.IO.File.ReadLines path
    |> paragraphs prefix


/// <summary>
/// Splits a paragraph of text on non-word characters returning a sequence of the form 
/// <word-characters><non-word-characters><word-characters>... (or in regex terms <\w*><\W+><\w+>...).
/// </summary>
/// <remarks>
/// A paragraph is a set of one or more sentences with no blank lines in between.
/// Supported collect token expressions and there meanings are
///   <collectToken><nextToken>
///     collect content up to the next ' ' (space).
///   <collectToken><collectUntilToken>:<nextToken>
///     collect content up to the next <collectUntilToken> as a single token.
/// </remarks>
/// <param name="collectToken">Token indicating to collect all content upto ' ' or a given character as a single token.</param>
/// <param name="paragraphText">Text to be tokenized.</param>
let tokens (collectToken:char) (paragraphText:string) = 
  let collectTokenStr = collectToken.ToString()
  // Underscore is in \w so we have to treat as a special case in split and isCollectToken regex patterns to allow
  // its use as a collectUntil token: ...><collectToken>_<tokenNotToSplit>_<... .
  let split = (new Regex(@"(\W*[_]\W*|\W+)", RegexOptions.Compiled)).Split
  let isCollectToken = (new Regex(System.String.Format(@"^\W*({0}\W|{0}[_]|{0})$", Regex.Escape(collectTokenStr)))).IsMatch
  seq {
        let mutable collecting = false
        let mutable collectUntil = ' '
        let mutable suppressCollectUntil = false
        let sb = new StringBuilder(50)
        let mutable i = -1;
        let isWordpart i = i % 2 = 0
        for token in split (paragraphText.Normalize()) do
          //printfn "collectUntil=%c=" collectUntil
          i <- i + 1
          if isWordpart i then // letters/numbers.
            if collecting then ignore (sb.Append(token)) else yield token
          else if collecting 
                then  //printfn "token=%s=" token
                      //printfn "sb=%s=" (sb.ToString())
                      match token.Split(collectUntil) with
                      | [| collect |] -> ignore (sb.Append(token))
                      | parts -> yield sb.Append(parts.[0]).ToString()
                                 // Here we have <parts><collect-until><...
                                 //printfn "%A" parts
                                 if suppressCollectUntil
                                 then yield String.concat (collectUntil.ToString()) parts.[1..]
                                 else yield (collectUntil.ToString()) + String.concat (collectUntil.ToString()) parts.[1..]
                                 ignore (sb.Clear())
                                 collecting <- false
                elif  isCollectToken token
                then  collecting <- true
                      if    token.EndsWith(collectTokenStr)
                      then  collectUntil <- ' ' // <collectUntilToken><nextToken>
                            suppressCollectUntil <- false
                      else  collectUntil <- token.[token.Length - 1] // <collectToken><collectUntilToken>:<nextToken>
                            suppressCollectUntil <- true
                      if    not (token.StartsWith(collectTokenStr))
                      then  yield Array.head (token.Split(collectToken))
                else yield token
        if sb.Length > 0 then yield sb.ToString()
  }

/// <summary>
/// Converts tokens into components for word wrapping this includes inserting hyphenation points, zero-spaces and applying 
/// left and right bindings (for e.g. left an right brackets and quote characters).
/// </summary>
/// <remarks>
/// Breakpoints are inserted into any words not already containing them before componentization, thus pre-insertion of break 
/// points can be used to over-ride the inserter break points where these are problematic.
/// </remarks>
/// <param name="breakPoint">character indicating a break point.</param>
/// <param name="insertBreakPoints">A function that inserts break points into word tokens.</param>
/// <param name="paragraphTokens">Sequence of word and word-boundary tokens.</param>
let components (breakPoint:char) insertBreakPoints (paragraphTokens:seq<string>) =
  let breakPointStr = breakPoint.ToString()
  let (|BindRight|) = (new Regex(@"^\s*(\p{Ps}|[""'`])$", RegexOptions.Compiled)).IsMatch // @"^\s*[[(}""'`]$")
  let (|BindLeft|) = (new Regex(@"^(\p{Pe}|[:""';.])\s*$", RegexOptions.Compiled)).IsMatch //@"^[])}:""';.]\s*$") // \p{Ps}
  let (|ZeroSpaceBefore|) = (new Regex(@"^$", RegexOptions.Compiled)).IsMatch
  let (|ZeroSpaceAfter|) = (new Regex(@"^[.:\/\\]+$", RegexOptions.Compiled)).IsMatch
  let (|ZeroSpaceAround|) = (new Regex(@"^[#-;]+$", RegexOptions.Compiled)).IsMatch
  let withBreakPoints (token:string) =
    if token.Contains(breakPointStr) then token
    else insertBreakPoints(token)
  seq { let mutable i = -1;
        let sb = new StringBuilder(50)
        for token in paragraphTokens do
          i <- i + 1
          if i % 2 = 0 // non-separator token.
          then if   System.String.IsNullOrWhiteSpace(token) 
               then ignore (sb.Append(token))
               else let tokenParts = (withBreakPoints token).Split(breakPoint)
                    ignore (sb.Append(tokenParts.[0]))
                    for i in 1 .. tokenParts.Length - 1 do
                      yield sb.Append(breakPoint).ToString()
                      ignore (sb.Clear().Append(tokenParts.[i]))
          elif token = breakPointStr
          then yield sb.Append(token).ToString()
               ignore (sb.Clear())
          else match token with
                | (BindRight true)
                | (ZeroSpaceBefore true) ->
                    yield sb.ToString(); 
                    ignore (sb.Clear().Append(token))
                | (BindLeft true)
                | (ZeroSpaceAfter true) ->
                    yield sb.Append(token).ToString();
                    ignore (sb.Clear())
                | (ZeroSpaceAround true) -> 
                    yield sb.ToString()
                    yield token
                    ignore (sb.Clear())
                | _ ->
                    yield sb.Append(token).ToString();
                    ignore (sb.Clear())
  }

/// <summary>
/// Converts tokens into components for word wrapping this includes inserting hyphenation points, zero-spaces and applying 
/// left and right bindings (for e.g. left an right brackets and quote characters).
/// </summary>
/// <remarks>
/// Rather than inserting break points into words, this version of `components` uses a splitting function.
/// </remarks>
/// <param name="breakPoint">character indicating a break point.</param>
/// <param name="splitWord">A function that splits words at break points or where those would be inserted.</param>
/// <param name="paragraphTokens">Sequence of word and word-boundary tokens.</param>
let components2 (breakPoint:char) splitWord (paragraphTokens:seq<string>) =
  let breakPointStr = breakPoint.ToString()
  let (|BindRight|) = (new Regex(@"^\s*(\p{Ps}|[""'`])$", RegexOptions.Compiled)).IsMatch // @"^\s*[[(}""'`]$")
  let (|BindLeft|) = (new Regex(@"^(\p{Pe}|[:""';.])\s*$", RegexOptions.Compiled)).IsMatch //@"^[])}:""';.]\s*$") // \p{Ps}
  let (|ZeroSpaceBefore|) = (new Regex(@"^$", RegexOptions.Compiled)).IsMatch
  let (|ZeroSpaceAfter|) = (new Regex(@"^[.:\/\\]+$", RegexOptions.Compiled)).IsMatch
  let (|ZeroSpaceAround|) = (new Regex(@"^[#-;]+$", RegexOptions.Compiled)).IsMatch
  let splitToken (token:string) =
    if token.Contains(breakPointStr) 
    then token.Split(breakPoint)
    else splitWord(token)
  seq { let mutable i = -1;
        let sb = new StringBuilder(50)
        for token in paragraphTokens do
          i <- i + 1
          if i % 2 = 0 // non-separator token.
          then if   System.String.IsNullOrWhiteSpace(token) 
               then ignore (sb.Append(token))
               else let tokenParts = splitToken token
                    ignore (sb.Append(tokenParts.[0]))
                    for i in 1 .. tokenParts.Length - 1 do
                      yield sb.ToString()
                      ignore (sb.Clear().Append(tokenParts.[i]))
          elif token = breakPointStr
          then yield sb.Append(token).ToString()
               ignore (sb.Clear())
          else match token with
                | (BindRight true)
                | (ZeroSpaceBefore true) ->
                    yield sb.ToString(); 
                    ignore (sb.Clear().Append(token))
                | (BindLeft true)
                | (ZeroSpaceAfter true) ->
                    yield sb.Append(token).ToString();
                    ignore (sb.Clear())
                | (ZeroSpaceAround true) -> 
                    yield sb.ToString()
                    yield token
                    ignore (sb.Clear())
                | _ ->
                    yield sb.Append(token).ToString();
                    ignore (sb.Clear())
  }

/// <summary>
/// Inserts additional spaces into line parts in an attempt to achieve a target concatenated width.
/// </summary>
/// <param name="targetWidth">Target width for concatenated parts.</param>
/// <param name="startingWidth">Starting width so the number of spaces required is targetWidth - startingWidth.</param>
/// <param name="lineParts">Line parts to which space will be added.</param>
let justify targetWidth startingWidth (lineParts:string list) = 
  //printfn "justify %i %i %A" targetWidth startingWidth lineParts
  let epsilon = 0.02 // adjusts category (lower) bounds.
  let inflate = 1.0 // determines over-accumulation of width.
  let widthToAllocate = float (targetWidth - startingWidth)
  let requiredPerChar = 
    widthToAllocate/float targetWidth * inflate
  let rec loop width accSpace acc (rest:string list) = 
    if width >= targetWidth
    then  (List.rev rest @ acc)
    else  match rest with
          | x :: rest ->
              let accSpace = 
                accSpace + requiredPerChar * (float x.Length)
              if not (x.EndsWith(" "))
              then  //printfn "justify.1.x=%s" x
                    match rest with
                    | [] -> x :: acc
                    | _  -> loop width accSpace (x :: acc) rest
              elif  accSpace >= (2.0 - epsilon) && (targetWidth - width) >= 2
              then  //printfn "justify.2.x=%s" x
                    match rest with
                    | [] -> x + "  " :: acc //x + "  " :: acc
                    | _  -> loop (width + 2) (accSpace - 2.0) (" " + x + " " :: acc) rest
              elif  accSpace > (1.0 - epsilon)
              then  //printfn "justify.3.x=%s" x
                    match rest with
                    | [] ->  x + " " :: acc  //(x + " ") :: acc
                    | _  -> loop (width + 1) (accSpace - 1.0) (x + " " :: acc) rest
              else  //printfn "justify.4.x=%s" x
                    match rest with
                    | [] -> x :: acc
                    | _  -> loop width accSpace (x :: acc) rest
            | [] -> invalidOp "Unreachable"
  let initAccSpaces = 0.5
  loop startingWidth initAccSpaces [] lineParts
  |> List.rev

/// <summary>
/// 
/// </summary>
/// <param name="breakPoint"></param>
/// <param name="targetWidth"></param>
/// <param name="tolerance"></param>
/// <param name="components"></param>
let wrap (breakPoint:char) (targetWidth:int) (tolerance:int) (components:seq<string>) = 
  let breakPointStr = breakPoint.ToString()
  let minWidth = targetWidth - tolerance
  let hyphen = '-'
  let hpyhenStr = hyphen.ToString()
  let isWordBreak (token:string) =
    token.EndsWith(" ")
  let isBreakPoint (token:string) = 
    token.EndsWith(breakPointStr)
  let justify = justify targetWidth
  seq {
    let mutable currentWidth = 0
    let mutable lineComponents = []
    let mutable ending = ""
    let mutable endAdjustment = 0
    let sb = new StringBuilder(targetWidth+3*tolerance)
    for token in components do
      //printfn "token=%s=" token
      let width = currentWidth + token.Length
      if                isWordBreak token 
      then  if          width > targetWidth 
            then  if    width = token.Length // overLengthUnbreakable
                  then  yield 
                          (sb.Append(token.TrimEnd()).ToString() :: lineComponents)
                        ignore (sb.Clear())
                        lineComponents <- []
                        currentWidth <- 0
                        ending <- ""
                        endAdjustment <- 0
                        //printfn "Branch.0.0.%s" token
                  else  yield
                          justify (currentWidth + endAdjustment) 
                            (sb.Append(ending).ToString().TrimEnd() :: lineComponents); 
                        ignore (sb.Clear())
                        lineComponents <- [ token ]
                        currentWidth <- token.Length
                        ending <- ""
                        endAdjustment <- -1
                        //printfn "Branch.0.1.%s" token
            elif        width > minWidth
            then        yield
                          justify (width - 1) 
                            (sb.Append(token.TrimEnd()).ToString() :: lineComponents)
                        ignore (sb.Clear())
                        lineComponents <- []
                        currentWidth <- 0
                        ending <- ""
                        endAdjustment <- 0
                        //printfn "Branch.1.%s" token
            else        lineComponents <- sb.Append(token).ToString() :: lineComponents; 
                        ignore (sb.Clear())
                        currentWidth <- width
                        ending <- ""
                        endAdjustment <- -1
                        //printfn "Branch.2.%s" token

      elif              isBreakPoint token then
        if              width >= targetWidth
        then  if        width = token.Length // overLengthUnbreakable
              then      yield
                          (sb.Append(token.Substring(0, token.Length - 1)).Append(hyphen).ToString() :: lineComponents)
                        ignore (sb.Clear())
                        lineComponents <- []
                        currentWidth <- 0
                        ending <- ""
                        endAdjustment <- 0
                        //printfn "Branch.3.0.%s" token
              else      yield
                          justify (currentWidth + endAdjustment) 
                            (sb.Append(ending).ToString().TrimEnd() :: lineComponents)
                        ignore (sb.Clear().Append(token.Substring(0, token.Length - 1)))
                        lineComponents <- []
                        currentWidth <- token.Length - 1
                        ending <- hpyhenStr
                        endAdjustment <- 1 // 0
                        //printfn "Branch.3.1.%s" token
        elif            width >= minWidth
        then            yield 
                          justify width 
                            (sb.Append(token.Substring(0, token.Length - 1)).Append(hyphen).ToString() :: lineComponents)
                        lineComponents <- []
                        currentWidth <- 0
                        ending <- ""
                        endAdjustment <- 0
                        ignore (sb.Clear())
                        //printfn "Branch.4.%s" token
        else            ignore (sb.Append(token.Substring(0, token.Length - 1)))
                        currentWidth <- width - 1 // unneeded hyphen removed.
                        ending <- hpyhenStr
                        endAdjustment <- 1
                        //printfn "Branch.5.%s" token

      else // All other cases.
        if              width >= targetWidth
        then  if        width = token.Length // overLengthUnbreakable
              then      yield 
                          (sb.Append(token).ToString() :: lineComponents)
                        ignore (sb.Clear())
                        lineComponents <- []
                        currentWidth <- 0
                        ending <- ""
                        endAdjustment <- 0
                        //printfn "Branch.6.1.%s" token
              else      yield 
                          justify (currentWidth + endAdjustment) 
                            (sb.Append(ending).ToString().TrimEnd() :: lineComponents)
                        ignore (sb.Clear().Append(token))
                        lineComponents <- []
                        currentWidth <- token.Length
                        ending <- ""
                        endAdjustment <- 0
                        //printfn "Branch.6.2.%s" token
        elif            width >= minWidth
        then            yield
                          justify width 
                            (sb.Append(token).ToString() :: lineComponents)
                        ignore (sb.Clear())
                        lineComponents <- []
                        currentWidth <- 0
                        ending <- ""
                        endAdjustment <- 0
                        //printfn "Branch.7.%s" token
        else            ignore (sb.Append(token))
                        currentWidth <- width
                        ending <- ""
                        endAdjustment <- 0
                        //printfn "Branch.8.%s" token
    if currentWidth > 0
    then yield sb.ToString() :: lineComponents
        //printfn "Branch.9"
  }
  |> Seq.map (List.rev >> String.concat "")



