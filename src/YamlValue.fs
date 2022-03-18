module FSharp.Data

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
type YamlValue =
  | String of string
  | Number of decimal
  //| Float of float
  | Record of properties:(string * YamlValue)[]
  | Array of elements:YamlValue[]
  | Boolean of bool
  | Null

   static member private FromObjTree (node: obj) =
    match node with
    | :? string as str ->
        match str with
        | "true" -> YamlValue.Boolean true
        | "false" -> YamlValue.Boolean false
        | _ ->
            match Decimal.TryParse str with
            | (true, d) -> YamlValue.Number d
            | _ -> YamlValue.String str
    
    | :? IList<obj> as list ->
        list
        |> Seq.toArray
        |> Array.map YamlValue.FromObjTree
        |> YamlValue.Array
    
    | :? IDictionary<obj, obj> as dict -> raise (NotImplementedException())