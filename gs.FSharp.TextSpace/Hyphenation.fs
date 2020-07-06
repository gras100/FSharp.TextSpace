namespace gras100.FSharp.TextSpace.Hyphenation

open System
open System.Text

open gras100.FSharp.TextSpace.Core
open gras100.FSharp.TextSpace.Models.Hyphenation

[<RequireQualifiedAccess>]
//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Hyphenation = 

  module Fsi =

    let setSourceDirectory value = 
      ResourcePaths.FsiSourceDirectory <- value

  [<AutoOpen>]
  module Internal = 

    let patternsReference (patterns:seq<HyphenationPattern>) = 
      patterns
      |> Seq.map (fun pattern -> pattern.token.ToLowerInvariant(), pattern) 
      |> Dic.ofPairs

    let overridesReference (overridePatterns:seq<HyphenationOverride>) = 
      overridePatterns
      |> Seq.map (fun pattern -> pattern.key, pattern.pattern)
      |> Dic.ofPairs

    let subStrings (word:string) = seq {
      for startIndex in 0 .. word.Length - 2 do
        for length in HyphenationPatterns.Format.shortestAllowedPattern .. word.Length - startIndex do
          yield startIndex, word.Substring(startIndex, length)
      }

    let accumulateScores tryGetPatternForWordPart (scores:int[]) (scoreOffset, wordPart) = 
      match tryGetPatternForWordPart wordPart with
      | true, pattern ->
          for scoreIndex in 0 .. pattern.scores.Length - 1 do
              let score = pattern.scores.[scoreIndex]
              if    scores.[scoreOffset + scoreIndex] < score 
              then  scores.[scoreOffset + scoreIndex] <- score 
      | false, _ -> ()

    let generateScores accumulateLevels (word:string) = 
      let scores = Array.zeroCreate (word.Length + 1)
      Seq.iter (accumulateLevels scores) (subStrings word)
      scores

    let maskOfScores frontLetters backLetters (scores:int[]) =
      assert (frontLetters > 0 && backLetters > 0)
      [|  let lastBreakPointIndex = scores.Length - backLetters - 2
          for i in 0 .. frontLetters - 1 do 
            yield 0
          for i in frontLetters .. lastBreakPointIndex - 1 do
            if scores.[i + 1] % 2 <> 0 
            then yield 1 else yield 0
          for i in lastBreakPointIndex .. scores.Length - 2 do
            yield 0 |]

    let applyOverride (breakChar:char) (override_:string) (word:string) = 
      override_.Split(breakChar)
      |> Array.mapFold (fun startIndex part ->   
          word.Substring(startIndex, part.Length)
          , startIndex + part.Length) 0
      |> fst //(fst >> String.concat (breakChar.ToString()))

    let mask (patterns:seq<HyphenationPattern>) frontLetters backLetters =
      let accumulateScores = 
        accumulateScores (patternsReference patterns).TryGetValue
      let scores = generateScores accumulateScores
      let mask = maskOfScores frontLetters backLetters
      fun (word:string) -> 
        mask (scores (HyphenationPatterns.Format.anchor (word.ToLowerInvariant())))


    let applyMask (breakChar:char) (mask:int[]) (word:string) = seq {
        let chars = new StringBuilder()
        for i in 0 .. word.Length - 1 do
          if mask.[i] > 0
          then  yield chars.Append(breakChar).ToString()
                ignore (chars.Clear())
          ignore (chars.Append(word.[i]))
        yield chars.ToString() // one or more characters always left over.
      }

  type Languages = gras100.FSharp.TextSpace.Models.Hyphenation.HyphenationLanguages

  type Spec = 
    {
    /// Character used in output to indicate break points.
    breakChar: char;
    
    /// <param name="minWordLength">Minimum length for word to be hyphenated, shorter words are returned unprocessed.</param>
    minWordLength: int;
    
    /// Minimum unbroken characters at start of word.
    minFrontLetters: int;
    
    /// Minimum unbroken characters at end of word.
    minBackLetters: int;
    
    }

    static member Defaults =
      {
      breakChar = '+';
      minFrontLetters = 2;
      minBackLetters = 2;
      minWordLength = 4;
      }

    /// <summary>
    /// Specification for break point providers.
    /// </summary>
    /// <param name="breakChar">Character used in output to indicate break points.</param>
    /// <param name="minWordLength">Minimum length for word to be hyphenated, shorter words are returned unprocessed.</param>
    /// <param name="minFrontLetters">Minimum unbroken characters at start of word.</param>
    /// <param name="minBackLetters">Minimum unbroken characters at end of word.</param>
    static member Create (breakChar, ?minFrontLetters, ?minBackLetters, ?minWordLength) =
      {
      breakChar = breakChar;
      minFrontLetters = defaultArg minFrontLetters Spec.Defaults.minFrontLetters
      minBackLetters = defaultArg minBackLetters Spec.Defaults.minBackLetters
      minWordLength = defaultArg minWordLength Spec.Defaults.minWordLength
      }

  /// <summary>
  /// Provides hyphenation word splitting functions for a given specification.
  /// </summary>
  type SplitterFactory (spec:Spec) =
    
    /// <summary>
    /// Creates a SplitBuilder with default arguments.
    /// </summary>
    new () = SplitterFactory(Spec.Defaults)

    member __.Spec = spec

    member __.BreakChar = spec.breakChar

    member __.BreakCharStr = spec.breakChar.ToString()

    /// <summary>
    /// 
    /// </summary>
    /// <param name="patterns"></param>
    member __.NewSplitter (patterns:seq<HyphenationPattern>) =
      let realHyphen = HyphenationPatterns.Format.realHyphen
      let realHyphenStr = realHyphen.ToString()
      let breakChar = spec.breakChar
      let mask =
        mask patterns 
          (Math.Max(1, spec.minFrontLetters))
          (Math.Max(1, spec.minBackLetters))
      let applyMask = applyMask breakChar
      let appendRealHyphen (parts:seq<string>) = 
        let arr = [| yield! parts |]
        arr.[arr.Length-1] <- arr.[arr.Length-1] + realHyphenStr
        arr
      fun (word:string) -> seq {
        if word.Contains(realHyphenStr)
        then  let subWords = word.Split(realHyphen)
              for i in 0 .. subWords.Length - 2  do 
                let subWord = subWords.[i]
                yield! appendRealHyphen (applyMask (mask subWord) subWord)
              let subWord = Array.last subWords
              yield! applyMask (mask subWord) subWord
        else yield! applyMask (mask word) word
      }

      /// <summary>
      /// 
      /// </summary>
      /// <param name="overridePatterns"></param>
      /// <param name="patterns"></param>
      member __.NewSplitter (overridePatterns:seq<HyphenationOverride>, patterns:seq<HyphenationPattern>) = 
        let overrides = overridesReference overridePatterns
        let applyOverride = applyOverride spec.breakChar
        let nonOverrideSplit = __.NewSplitter(patterns)
        fun (word:string) ->
          match overrides.TryGetValue (word.ToLowerInvariant()) with
          | true, pattern -> Array.toSeq (applyOverride pattern word)
          | _ -> nonOverrideSplit word

      member __.NewSplitter(overridesPath:string, patternsPath:string) =
        __.NewSplitter(HyphenationPatterns.readOverridesFile spec.breakChar overridesPath, HyphenationPatterns.readFile (patternsPath))

      member __.NewSplitter(pathPair:HyphenationLanguageSet) =
        __.NewSplitter(pathPair.overrides, pathPair.patterns)

      member __.GetSpecNewSplitterPair (patterns:seq<HyphenationPattern>) =
        __.Spec, __.NewSplitter(patterns)

      member __.GetSpecNewSplitterPair (overridePatterns:seq<HyphenationOverride>, patterns:seq<HyphenationPattern>) = 
        __.Spec, __.NewSplitter(overridePatterns, patterns)

      member __.GetSpecNewSplitterPair(overridesPath:string, patternsPath:string) =
        __.Spec, __.NewSplitter(overridesPath, patternsPath)

      member __.GetSpecNewSplitterPair(pathPair:HyphenationLanguageSet) =
        __.Spec, __.NewSplitter(pathPair.overrides, pathPair.patterns)

  /// <summary>
  /// Provides functions to insert hyphenation markers into words for a given specification.
  /// </summary>
  type InserterFactory (spec:Spec) = 

    let splitter = SplitterFactory(spec)

    let breakCharStr = splitter.BreakCharStr

    /// <summary>
    /// Creates a SplitBuilder with default arguments.
    /// </summary>
    new () = InserterFactory(Spec.Defaults)

    member __.Spec = spec

    member __.BreakChar = spec.breakChar

    member __.BreakCharStr = breakCharStr

    /// <summary>
    /// 
    /// </summary>
    /// <param name="patterns"></param>
    member __.NewInserter (patterns:seq<HyphenationPattern>) =
      let split = splitter.NewSplitter(patterns)
      fun (word:string) -> String.concat breakCharStr (split word)

    /// <summary>
    /// 
    /// </summary>
    /// <param name="overridePatterns"></param>
    /// <param name="patterns"></param>
    member __.NewInserter (overridePatterns:seq<HyphenationOverride>, patterns:seq<HyphenationPattern>) = 
      let split = splitter.NewSplitter(overridePatterns, patterns)
      fun (word:string) -> String.concat "" (split word)

    member __.NewInserter(overridesPath:string, patternsPath:string) =
      __.NewInserter(HyphenationPatterns.readOverridesFile spec.breakChar overridesPath, HyphenationPatterns.readFile (patternsPath))

    member __.NewInserter(pathPair:HyphenationLanguageSet) =
      __.NewInserter(pathPair.overrides, pathPair.patterns)

    member __.GetSpecNewInserterPair (patterns:seq<HyphenationPattern>) =
      __.Spec, __.NewInserter(patterns)

    member __.GetSpecNewInserterPair (overridePatterns:seq<HyphenationOverride>, patterns:seq<HyphenationPattern>) = 
      __.Spec, __.NewInserter(overridePatterns, patterns)

    member __.GetSpecNewInserterPair(overridesPath:string, patternsPath:string) =
      __.Spec, __.NewInserter(overridesPath, patternsPath)

    member __.GetSpecNewInserterPair(languageSet:HyphenationLanguageSet) =
      __.Spec, __.NewInserter(languageSet.overrides, languageSet.patterns)