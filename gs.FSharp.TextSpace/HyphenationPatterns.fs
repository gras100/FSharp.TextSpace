namespace gras100.FSharp.TextSpace.Hyphenation

open System
open System.Text
open System.Text.RegularExpressions
open System.Globalization

open gras100.FSharp.TextSpace.Models.Hyphenation

(*  
    Use for Http text hyphenator later.
    http://www.tug.org/docs/liang/liang-thesis.pdf
    https://nedbatchelder.com/code/modules/hyphenate.html
    https://en.wikipedia.org/wiki/Soft_hyphen (SHY) (see links)
    https://en.wikipedia.org/wiki/Zero-width_space (see links)
    // bind characters too, to prevent a break that might otherwise occur.

    //let softHyphen = '\u00AD'
    //let zeroWidthSpace = '\u200B'
*)

[<RequireQualifiedAccess>]
module HyphenationPatterns = 

  [<AutoOpen>]
  module Format = 

    let shortestAllowedPattern = 2

    /// <summary>
    /// Character used to anchor prefix and suffix patterns that we need to quote words with prior 
    /// to applying hyphenation.
    /// </summary>
    let endAnchor = "."

    /// <summary>
    /// Character used to escape real hyphens in hyphenation override patterns e.g with escape character '\' 
    /// an over-ride pattern for 'multi-faceted' might be 'multi\-facet-ed'.
    /// </summary>
    let escapeChar = '\\'

    /// <summary>
    /// Character used to indicate a hyphenation point in hyphenation override patterns e.g. pro-ject.
    /// </summary>
    let hyphenIndicator = '-'

    /// <summary>
    /// Character representing a real hyphen used to split hyphenated words prior to applying hyphenation to 
    /// their component subwords and replace escaped hyphens with when unescaping.
    /// </summary>
    let realHyphen = '-'

    /// <summary>
    /// Unescapes escaped hyphens in hyphenation over-ride patterns.
    /// </summary>
    /// <param name ="input">hyphenation override pattern to be unescaped.</param>
    let unescape = 
      let (!) = Char.ToString >> Regex.Escape
      let replacer = (new Regex(sprintf @"[%s][%s]" !escapeChar !realHyphen, RegexOptions.Compiled))
      let realHyphen = realHyphen.ToString()
      fun (input:string) -> replacer.Replace(input, realHyphen)

    /// <summary>
    /// Replaces hyphenIndicators with the given replacement then unescapes a hyphenation override pattern.
    /// </summary>
    /// <param name ="replacement">value to replace hyphenIndicators with.</param>
    /// <param name ="input">pattern to replace hyphenIndicators in then escape.</param>
    let unescapeReplacingHyphenIndicatorsWith = 
      let (!) = Char.ToString >> Regex.Escape
      let replacer = (new Regex(sprintf @"[^%s][%s]" !escapeChar !hyphenIndicator, RegexOptions.Compiled))
      fun (replacement:string) (input:string) -> unescape (replacer.Replace(input, replacement))

    let anchor word = endAnchor + word + endAnchor

  /// <summary>
  /// 
  /// </summary>
  /// <param name="pattern"></param>
  let parsePattern (pattern:string) = 
    (* 
    We split out numbers from letters, so e.g. the pattern .ab30l parses to

        { 
        token = .abol; 
        scores = [ 0; 0; 0; 3; 0; 0; ] 
        }
          
    where there is a score for each letter gap indicating its favor as a break 
    (evens) or never-break (odds) point.
    *)
    let token = StringBuilder()
    let scores = ResizeArray<int>()
    let mutable awaitDigit = true
    pattern.ToCharArray()
    |> Array.iter (fun c ->
        match Int32.TryParse (c.ToString(CultureInfo.InvariantCulture)) with
        | true, value ->
            scores.Add(value); 
            awaitDigit <- false;
        | false, _ -> 
            if awaitDigit then scores.Add(0)
            token.Append(c) |> ignore 
            awaitDigit <- true)
    if awaitDigit then scores.Add(0)
    { 
    token = token.ToString(); 
    scores = scores.ToArray();
    }

  /// <summary>
  /// Reads tex hyphenation patterns file.
  /// </summary>
  /// <param name="path">Path to tex hyphenation file.</param>
  let readFile path = 
    System.IO.File.ReadAllLines(path)
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.map parsePattern

  /// <summary>
  /// Reads tex hyphenation over-ride patterns file subsituting hyphenIndicators with the given soft-hyphen or "break" char.
  /// </summary>
  /// <param name="breakChar">Character to use as a soft-hyphen must be a non-word character different from real-hyphen.</param>
  /// <param name="path">Path to tex hyphenation overrides file.</param>
  let readOverridesFile (softHyphen:char) path =
    if softHyphen = realHyphen
    then invalidArg "breakChar" (sprintf @"invalid value '%c', cannot be '%c'." softHyphen realHyphen)
    let createKey = unescapeReplacingHyphenIndicatorsWith String.Empty
    let insertBreakPoints = unescapeReplacingHyphenIndicatorsWith (softHyphen.ToString())
    System.IO.File.ReadLines path
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.map (fun line -> 
        let line = line.ToLowerInvariant()
        { key = createKey line; pattern = insertBreakPoints line })
    |> Array.ofSeq