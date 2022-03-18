module FSharp.Data

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
type YamlValue =
  | String of string
  | Integer of int
  | Decimal of decimal
  //| Float of float
  | Record of properties:(YamlValue * YamlValue)[]
  | Sequence of elements:YamlValue[]
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
            | (true, d) ->
                match Int32.TryParse str with
                // if no decimal places
                | (true, i) when (decimal i) = d -> YamlValue.Integer i
                | _ -> YamlValue.Decimal d
            | _ -> YamlValue.String str
    
    | :? IList<obj> as list ->
        list
        |> Seq.toArray
        |> Array.map YamlValue.FromObjTree
        |> YamlValue.Sequence
    
    | :? IDictionary<obj, obj> as dict ->
        dict
        |> Seq.toArray
        |> Array.map (fun pair ->
            YamlValue.FromObjTree pair.Key, YamlValue.FromObjTree pair.Value
            )
        |> YamlValue.Record
    
    | null -> YamlValue.Null

    | _ -> failwithf $"Invalid tree node {node} passed to %s{nameof YamlValue.FromObjTree}"