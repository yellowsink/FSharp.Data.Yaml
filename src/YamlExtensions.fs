namespace FSharp.Data

open System.Globalization
open System.Runtime.InteropServices
open FSharp.Data
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core

/// Extension methods with operations on YAML values
[<Extension>]
type YamlExtensions =
    
    static member private defaultIfNone def = function | Some c -> c | None -> def
    
    static member private defaultCulture = YamlExtensions.defaultIfNone CultureInfo.InvariantCulture
    
    /// Get a sequence of key-value pairs representing the props of a mapping, or an empty array if not a mapping
    [<Extension>]
    static member Properties x =
        match x with
        | YamlValue.Mapping props -> props
        | _ -> [||]
    
    /// Get property of a YAML mapping. Fails if the value is not a mapping or if the prop is not present
    [<Extension>]
    static member GetProperty (x, propertyName) =
        match x with
        | YamlValue.Mapping props ->
            match props |> Array.tryFind (fst >> (=) propertyName) with
            | Some (_, value) -> value
            | None -> failwithf $"Didn't find property '%s{propertyName.ToString()}' in %s{x.ToString()}"
        | _ -> failwithf $"Not an object: %s{x.ToString()}"
    
    /// Try to get a property of a YAML mapping. Returns None if value is not a mapping or prop is missing
    [<Extension>]
    static member TryGetProperty (x, propertyName) =
        match x with
        | YamlValue.Mapping props ->
            props
            |> Array.tryFind (fst >> (=) propertyName)
            |> Option.map snd
        | _ -> None
    
    /// Gets all the elements of a YAML sequence. Returns an empty array if the value is not a sequence
    [<Extension>]
    static member AsSequence x =
        match x with
        | YamlValue.Sequence elems -> elems
        | _ -> [||]
    
    /// Gets all the elements of a YAML sequence, else empty, but as an enumerator.
    [<Extension>]
    static member GetEnumerator x = (YamlExtensions.AsSequence x).GetEnumerator()
    
    /// Try to get the value at the specified index, if a sequence. Fails if not a sequence or out of range
    [<Extension>]
    static member Item (x, i) = (YamlExtensions.AsSequence x)[i]
    
    /// Get the string value of an element (assuming its a scalar), or empty string for null
    [<Extension>]
    static member AsString (x, [<Optional>] ?cultureInfo) =
        match YamlConversions.AsString false (YamlExtensions.defaultCulture cultureInfo) x with
        | Some s -> s
        | None -> failwithf $"Not a string: %s{x.ToString()}"
    
    /// Get a value as an integer
    [<Extension>]
    static member AsInteger (x, [<Optional>] ?cultureInfo) =
        match YamlConversions.AsInteger (YamlExtensions.defaultCulture cultureInfo) x with
        | Some i -> i
        | None -> failwithf $"Not an int: %s{x.ToString()}"
    
    /// Get a value as a 64 bit integer
    [<Extension>]
    static member AsInteger64 (x, [<Optional>] ?cultureInfo) =
        match YamlConversions.AsInteger64 (YamlExtensions.defaultCulture cultureInfo) x with
        | Some i -> i
        | None -> failwithf $"Not an int64: %s{x.ToString()}"
    
    /// Get a value as a decimal
    [<Extension>]
    static member AsDecimal (x, [<Optional>] ?cultureInfo) =
        match YamlConversions.AsDecimal (YamlExtensions.defaultCulture cultureInfo) x with
        | Some i -> i
        | None -> failwithf $"Not an int64: %s{x.ToString()}"
    
    /// Get a value as a float
    [<Extension>]
    static member AsFloat (x, [<Optional>] ?cultureInfo) =
        match YamlConversions.AsFloat (YamlExtensions.defaultCulture cultureInfo) x with
        | Some i -> i
        | None -> failwithf $"Not an int64: %s{x.ToString()}"
        
    /// Get a value as a boolean
    [<Extension>]
    static member AsBoolean x =
        match YamlConversions.AsBoolean x with
        | Some i -> i
        | None -> failwithf $"Not a boolean: %s{x.ToString()}"
    
    /// Get a value as a guid
    static member AsGuid x =
        match YamlConversions.AsGuid x with
        | Some i -> i
        | None -> failwithf $"Not a GUID: %s{x.ToString()}"

/// Provides the dynamic (?) operator for yaml values
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonExtensions =
    
    /// Get a property of a YAML mapping
    let (?) (yamlVal: YamlValue) propName = yamlVal.GetProperty(propName)