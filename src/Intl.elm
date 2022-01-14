module Intl exposing (Intl, decode, PluralOptions, plural, FormatNumberOptions, formatFloat, formatInt, formatDateTime, FormatDateTimeOptions)

{-| CodeGen for Intl functions. The Intl API will be given access to by a Proxy Object injected into the Elm Runtime via Flags.
This mechanism makes it possible to have synchronous communication with JS. In order to avoid a lot of methods on the JS side,
we are using a eval-like mechanism: We pass the information which Sub API to call and with which arguments as a JSON string.

@docs Intl, decode, PluralOptions, plural, FormatNumberOptions, formatFloat, formatInt, FormatDateTimeOptions, formatDateTime

-}

import Json.Decode as D
import Json.Encode as E
import Time


{-| Use this type for the JS Proxy Object you received via a flag or a port from the accompanying JS package.
-}
type alias Intl =
    D.Value


{-| Convienience decoder for Intl Type. This makes it possible for me to change the internal representation without a breaking change.
-}
decode : D.Decoder Intl
decode =
    D.value


type PluralType
    = Cardinal
    | Ordinal


{-| Options for the `plural` function.
-}
type alias PluralOptions number =
    { language : String
    , type_ : PluralType
    , number : number
    }


encodeArgs : List E.Value -> E.Value
encodeArgs =
    E.list identity


{-| Determine the CLDR plural category (see <https://www.unicode.org/cldr/cldr-aux/charts/30/supplemental/language_plural_rules.html>)
for a given number and language.

The possible categories are: `zero`, `one`, `two`, `few`, `many`, and `other`.
When the category cannot be determined for whatever reason, this function will default to "other".

-}
plural : Intl -> PluralOptions Int -> String
plural intl opts =
    let
        pluralArgs =
            encodeArgs <|
                List.filterMap identity
                    [ Just <| E.string opts.language
                    , case opts.type_ of
                        Ordinal ->
                            Just <| E.object [ ( "type", E.string "ordinal" ) ]

                        Cardinal ->
                            Nothing
                    ]

        encodedOptions : E.Value
        encodedOptions =
            encodeArgs [ E.string "PluralRules", pluralArgs, E.string "select", encodeArgs [ E.int opts.number ] ]
    in
    fromIntlField intl encodedOptions
        |> Maybe.withDefault "other"


{-| Options for the `formatInt` and `formatFloat` functions.

`args` can consist of any object entries you want to pass to the NumberFormat constructor.
The following serves as a hint to what is actually valid and will not result in an error:

    - currency: String, needs to be set if style is "currency". For example "EUR" is a valid currency.
    - currencyDisplay: "name", "symbol" or "code", defaults to "symbol"
    - maximumFractionDigits: Int
    - maximumSignificantDigits: Int
    - minimumFractionDigits: Int
    - minimumIntegerDigits: Int
    - minimumSignificantDigits: Int
    - style : one of "decimal", "currency", "percent"
    - useGrouping : Bool, True results in something like 123.456.789 while False will result in 123456789

-}
type alias FormatNumberOptions number =
    { language : String
    , args : List ( String, E.Value )
    , number : number
    }


{-| Format an Int with the given Options
-}
formatInt : Intl -> FormatNumberOptions Int -> String
formatInt =
    formatNumber E.int


{-| Format a Float with the given Options
-}
formatFloat : Intl -> FormatNumberOptions Float -> String
formatFloat =
    formatNumber E.float


formatNumber : (num -> E.Value) -> Intl -> FormatNumberOptions num -> String
formatNumber encodeNum intl opts =
    let
        formatNumberArgs =
            encodeArgs <|
                [ E.string opts.language
                , E.object opts.args
                ]

        encodedOptions : E.Value
        encodedOptions =
            encodeArgs [ E.string "NumberFormat", formatNumberArgs, E.string "format", encodeArgs [ encodeNum opts.number ] ]
    in
    fromIntlField intl encodedOptions
        |> Maybe.withDefault "other"


{-| Options for the `formatDate` function.

`args` can consist of any object entries you want to pass to the DateTimeFormat constructor.
The following serves as a hint to what is actually valid and will not result in an error:

    - timeZone: String, implementation specific. UTC is the default and works for all implementations. Stuff like Asia/Shanghai could work depending on browser.
    - hour12: Bool, determines whether AM/PM or 24h format should be used.
    - hourCycle: "h11", "h12", "h23" or "h24". Overrides the hour12 argument.
    - weekday: "narrow", "short or "long"
    - era: "narrow", "short or "long"
    - year: "numeric" or "2-digit"
    - month: "numeric", "2-digit", "narrow", "short or "long"
    - day: "numeric" or "2-digit"
    - hour: "numeric" or "2-digit"
    - minute: "numeric" or "2-digit"
    - second: "numeric" or "2-digit"
    - timeZoneName: "short" or "long"

-}
type alias FormatDateTimeOptions =
    { time : Time.Posix
    , args : List ( String, E.Value )
    , language : String
    }


{-| Format a Posix Time with the given Options
-}
formatDateTime : Intl -> FormatDateTimeOptions -> String
formatDateTime intl opts =
    let
        formatDateTimeArgs =
            encodeArgs <|
                [ E.string opts.language
                , E.object opts.args
                ]

        encodedOptions : E.Value
        encodedOptions =
            encodeArgs [ E.string "DateTimeFormat", formatDateTimeArgs, E.string "format", encodeArgs [ E.int <| Time.posixToMillis opts.time ] ]
    in
    fromIntlField intl encodedOptions
        |> Maybe.withDefault "other"


fromIntlField : Intl -> E.Value -> Maybe String
fromIntlField intl encodedField =
    D.decodeValue (D.field (E.encode 0 encodedField) D.string) intl
        |> Result.toMaybe
