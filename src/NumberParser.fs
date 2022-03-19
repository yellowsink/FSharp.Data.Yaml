module FSharp.Data.NumberParser

open System
open System.Globalization

/// converts the result of a tryParse function to an option type
let triedAsOption = function | true, a -> Some a | false, _ -> None

// https://exercism.org/tracks/fsharp/exercises/octal/solutions/10df18e0be1f4bb492f71efe8481786a
let toRadix r (str: string) =
    let toInt (x: char) = (int x) - (int '0')
    
    str.ToCharArray()
    |> Array.rev
    |> Array.mapi (fun n x -> (toInt x) * (pown r n))
    |> Array.sum
// end stolen code :p

let stringIsHex (str: string) =
    str.ToLower()
    |> String.forall (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))

let stringIsOctal (str: string) =
    str.ToLower()
    |> String.forall (fun c -> c >= '0' && c <= '7')

let private (|HexInt|_|) (str: string) =
    if str.StartsWith "0x" && str.Substring 2 |> stringIsHex
    then Some <| Int32.Parse (str.Substring 2, NumberStyles.HexNumber)
    else None

let private (|OctInt|_|) (str: string) =
    if str.StartsWith "0o" && str.Substring 2 |> stringIsOctal
    then str.Substring 2 |> toRadix 8 |> Some
    else None

let private (|FloatVal|_|) =
    function
    | ".nan" -> Some Double.NaN
    | "-.nan" -> Some Double.NaN
    | ".inf" -> Some Double.PositiveInfinity
    | "-.inf" -> Some Double.NegativeInfinity
    | f -> Double.TryParse f |> triedAsOption

let private (|IntVal|_|) (x: string) = Int32.TryParse x |> triedAsOption

type IntOrFloat = | Int of int | Flt of float

let parse =
    function
    | HexInt i   -> Some (Int i)
    | OctInt i   -> Some (Int i)
    | FloatVal f -> Some (Flt f)
    | IntVal i   -> Some (Int i)
    | _ -> None

let (|Integer|_|) =
    parse
    >> function
        | Some (Int i) -> Some i
        | _ -> None

let (|Float|_|) =
    parse
    >> function
        | Some (Flt i) -> Some i
        | _ -> None