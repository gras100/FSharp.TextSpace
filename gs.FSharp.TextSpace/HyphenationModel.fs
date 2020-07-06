namespace gras100.FSharp.TextSpace.Models.Hyphenation

open System
open System.IO
open System.Text.RegularExpressions

open gras100.FSharp.TextSpace.Core


type HyphenationLanguageSet = { patterns:string; overrides:string }

type HyphenationPattern = { token:string; scores:int[]; }
  
type HyphenationOverride = { key:string; pattern: string }


/// <summary>
/// Partial Hyphenation language class implementing non-compiled language support extended
/// with compiled languages accessors in HyphenationLanguages.res.fs.
/// </summary>
type HyphenationLanguages private () =
  // TODO: understand utf pattersn at http://www.hyphenation.org/tex#languages
  // TODO: move to https://tools.ietf.org/html/bcp47 language tags.
  static member ImportPatternFile(patterns:string) = 
    let fileName = Path.GetFileName(patterns)
    if Regex.IsMatch(fileName, @"^hyph[-][a-z]{2}([-][a-z]+)*.pat.txt$")
    then  System.IO.File.Copy(patterns, ResourcePaths.GetPath("res", fileName))
          System.IO.File.Create(ResourcePaths.GetPath("res", fileName.Replace(".pat.txt", ".hyp.txt"))).Dispose()

  static member ImportOverridesFile(overrides:string) = 
    let fileName = Path.GetFileName(overrides)
    if Regex.IsMatch(fileName, @"^hyph[-][a-z]{2}([-][a-z]+)*.hyp.txt$")
    then System.IO.File.Copy(overrides, ResourcePaths.GetPath("res", fileName))
    
  static member OfString (language:string, ?qualifier:string) =
    let language = language + (Option.orElse (Some "") (Option.map ((+) "-") qualifier)).Value
    {
      patterns = ResourcePaths.GetPath("res", "hyph-" + language + ".pat.txt");
      overrides = ResourcePaths.GetPath("res", "hyph-" + language + ".hyp.txt");
    }


module CodeGeneration =

  module HyphenationLanguagesResourceExtensions = 

    let fileName = "HyphenationLanguagesResourceExtensions.fs"

    // Moved to Hyphenation namespace, as otherwise need to separately open namespace to use, e.g.,
    // Hyphenation.Languages.EnGb
    let moduleName = "gras100.FSharp.TextSpace.Hyphenation.HyphenationLanguagesResourceExtensions"

    /// <summary>
    /// 
    /// </summary>
    /// <param name="tab"></param>
    /// <param name="outputNameSpace"></param>
    /// <param name="append"></param>
    let generate (append:bool) =
      //let tab = if tab = "" then "  " else tab
      let tab = "  "
      let titleCase = 
        (new Globalization.CultureInfo("en-US", useUserOverride = false)).TextInfo.ToTitleCase
      let propertyName (fileName:string) = 
        fileName.Split('.').[0].Split('-').[1..]
        |> Array.map titleCase
        |> String.concat ""
      let pathEndingWithElse else_ ending (paths:seq<string>) = 
        (Option.orElse (Some else_) (paths |> Seq.tryFind(fun path -> path.EndsWith(ending)))).Value
      let emptyPatternsFile = "empty-hyph.pat.txt"
      let emptyOverridesFile = "empty-hyph.hyp.txt"
      seq {
        yield "////////////////////////////////////////////////////////////////////////////////////////////////////////////////////"
        yield "// This file was auto-generated based on the content of the res folder via"
        yield "//"
        yield "//    gras100.FSharp.TextSpace.Models.Hyphenation.CodeGeneration.HyphenationLanguagesResourceExtensions.generate."
        yield "//"
        yield "////////////////////////////////////////////////////////////////////////////////////////////////////////////////////"
        yield "[<AutoOpen>]"
        yield "module " + moduleName; yield ""
        yield "open gras100.FSharp.TextSpace.Core"; yield ""
        yield "type HyphenationLanguages with"; yield ""
        for group, paths in ResourcePaths.GetPaths("hyph-*", "res") |> Seq.groupBy (fun path -> Path.GetFileName(path).Split('.').[0]) do
              yield sprintf "%sstatic member %s = {" tab (propertyName (Path.GetFileName(Seq.head paths)))
              yield sprintf "%s%spatterns = ResourcePaths.GetPath(\"res\", \"%s\");" tab tab (Path.GetFileName(pathEndingWithElse emptyPatternsFile "pat.txt" paths))
              yield sprintf "%s%soverrides = ResourcePaths.GetPath(\"res\", \"%s\");" tab tab (Path.GetFileName(pathEndingWithElse emptyOverridesFile "hyp.txt" paths))
              yield sprintf "%s}" tab
      }
      |> (fun lines -> 
            let targetFileName = Path.Combine(ResourcePaths.GetPath(), fileName)
            try
              let backupFileName = 
                targetFileName + FileInfo(targetFileName).LastWriteTime.ToString(".yyyy_MM_dd_HHmmss")
              File.Copy (targetFileName, backupFileName)
            with
            | :? FileNotFoundException -> () //
            use writer = new StreamWriter(targetFileName, append = append)
            lines |> Seq.iter (writer.WriteLine))