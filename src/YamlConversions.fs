namespace FSharp.Data

open System
open FSharp.Data

type YamlConversions = 
    static member AsString noneForNullEmpty cultureInfo =
        function
        | YamlValue.String s -> if noneForNullEmpty && String.IsNullOrEmpty s then None else Some s
        | YamlValue.Boolean b -> Some (b.ToString cultureInfo)
        | YamlValue.Integer n -> Some (n.ToString cultureInfo)
        | YamlValue.Float n -> Some (n.ToString cultureInfo)
        | YamlValue.Null when not noneForNullEmpty -> Some ""
        | _ -> None
    
    static member AsInteger (cultureInfo: IFormatProvider) =
        function
        // The JSON one checks for floats being ints too but I know with certainty that at anything an int32
        // cant hold is stored as a decimal and anything that it can hold is stored as an int, so unnecessary here
        | YamlValue.Integer n -> Some n
        | YamlValue.String s -> Some (Int32.Parse(s, cultureInfo))
        | _ -> None
    
    static member AsInteger64 (cultureInfo: IFormatProvider) =
        function
        | YamlValue.Integer n -> Some (int64 n)
        | YamlValue.Float d ->
            if (Math.Round d) = d && d >= double Int64.MinValue && d <= double Int64.MaxValue
            then Some (int64 d)
            else None
        | YamlValue.String s -> Some (Int64.Parse(s, cultureInfo))
        | _ -> None
    
    static member AsDecimal (cultureInfo: IFormatProvider) =
        function
        | YamlValue.Integer n -> Some (decimal n)
        | YamlValue.Float d -> Some d
        | YamlValue.String s -> Some (Decimal.Parse(s, cultureInfo))
        | _ -> None
    
    // no missingValues and noneForMissing values like in JsonConversions
    // big L
    static member AsFloat (*missingValues noneForMissing*) (cultureInfo: IFormatProvider) =
        function
        | YamlValue.Integer n -> Some (float n)
        | YamlValue.Float n -> Some (float n)
        | YamlValue.String s -> Some (Double.Parse(s, cultureInfo))
        | _ -> None
    
    static member AsBoolean =
        function
        | YamlValue.Boolean b -> Some b
        | YamlValue.Integer 0 -> Some false
        | YamlValue.Integer _ -> Some true
        | YamlValue.Float f -> Some (Double.IsNaN f |> not)
        | YamlValue.String "true" & YamlValue.String "yes" & YamlValue.String "1" -> Some true
        | YamlValue.String "false" & YamlValue.String "no" & YamlValue.String "0" -> Some false
        | _ -> None
    
    static member AsGuid =
        function
        | YamlValue.String s -> s.Trim() |> Guid.TryParse |> NumberParser.triedAsOption
        | _ -> None