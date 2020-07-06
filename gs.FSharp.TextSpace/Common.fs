namespace gras100.FSharp.TextSpace.Core

open System
open System.Collections.Generic
open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Dic =

  let ofPairs (pairs:seq<_*_>) =
    let dic = new Dictionary<_,_>()
    pairs
    |> Seq.distinct
    |> Seq.iter (fun (k, v) -> dic.Add(k, v))
    dic


[<RequireQualifiedAccess>]
module Cache = 

  open System.Collections.Generic
  
  let unbounded f = 
    let dic = new Dictionary<_,_>()
    fun x ->
      match dic.TryGetValue x with
      | true, value -> value
      | false, _ -> 
          let result = f x
          dic.Add(x, result)
          result


[<AutoOpen>]
module AssemblyExtensions = 

  type Assembly with
  
    member __.LocationOrElse(else_:string) =
      try __.Location 
      with
      | :? System.NotSupportedException as e -> 
          //printfn "message << %s >> source << %s >> stack << %s >>" e.Message e.Source e.StackTrace; 
          else_

    member __.CodeBaseOrElse (else_:string) = 
      try __.CodeBase with e -> else_


type ResourcePaths private () =

  static let (/+) x y = Array.append [| x |] y

  static let assemblyPath = Assembly.GetAssembly(typeof<ResourcePaths>).LocationOrElse("")

  [<Literal>]
  static let FsiSourceDirectoryToken = "__SOURCE_DIRECTORY__"
  
  static member val FsiSourceDirectory = FsiSourceDirectoryToken with get, set

  static member GetPath() = 
    match ResourcePaths.FsiSourceDirectory with
    | FsiSourceDirectoryToken ->  
        match assemblyPath with
        | "" -> ResourcePaths.FsiSourceDirectory 
        | path when path.EndsWith("fsi.exe") -> ResourcePaths.FsiSourceDirectory
        | path -> path
    | path -> path

  static member GetPath ([<ParamArray>]paths: string[]) = 
    Path.Combine(ResourcePaths.GetPath()/+paths)

  static member GetPaths ([<ParamArray>]paths: string[]) = 
    Directory.EnumerateFiles(ResourcePaths.GetPath paths)

  static member GetPaths (pattern:string, subPath:string) = seq {
    for path in Directory.EnumerateFiles(ResourcePaths.GetPath subPath, pattern) do
      yield path }