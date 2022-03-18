namespace FSharp.Data

open System
open System.Collections.Generic
open System.IO
open System.Text
open YamlDotNet.Serialization

[<RequireQualifiedAccess>]
type YamlValue =
    | String of string
    | Integer of int
    | Decimal of decimal
    //| Float of float
    | Mapping of properties: (YamlValue * YamlValue) []
    | Sequence of elements: YamlValue []
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
                | false, _ -> YamlValue.String str
                | true, d ->
                    match Int32.TryParse str with
                    // if no decimal places
                    | true, i when (decimal i) = d -> YamlValue.Integer i
                    | _ -> YamlValue.Decimal d

        | :? IList<obj> as list ->
            list
            |> Seq.toArray
            |> Array.map YamlValue.FromObjTree
            |> YamlValue.Sequence

        | :? IDictionary<obj, obj> as dict ->
            dict
            |> Seq.toArray
            |> Array.map (fun pair -> YamlValue.FromObjTree pair.Key, YamlValue.FromObjTree pair.Value)
            |> YamlValue.Mapping

        | null -> YamlValue.Null

        | _ -> failwithf $"Invalid tree node {node} passed to %s{nameof YamlValue.FromObjTree}"
    
    static member Parse (str: string) =
        new StreamReader(new MemoryStream(str |> Encoding.Default.GetBytes))
        |> Deserializer().Deserialize
        |> YamlValue.FromObjTree

[<RequireQualifiedAccess>]
module YamlValue =
    /// Represents a mapping with string keys, more useful than YamlValue.Mapping in most cases
    let (|StringMapping|_|) obj =
        match obj with
        | YamlValue.Mapping rawProps ->
            let stringProps =
                rawProps
                |> Array.choose (function | YamlValue.String k, v -> Some (k, v) | _ -> None)
            
            if stringProps.Length = rawProps.Length
            then Some stringProps
            else None

        | _ -> None