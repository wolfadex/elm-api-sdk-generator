module Gen.Int64 exposing (add, and, annotation_, call_, complement, decoder, encoder, fromInt, fromInt32s, moduleName_, or, rotateLeftBy, rotateRightBy, shiftLeftBy, shiftRightZfBy, signedCompare, subtract, toBitString, toBits, toByteValues, toHex, toSignedString, toUnsignedString, unsignedCompare, values_, xor)

{-| 
@docs moduleName_, fromInt, fromInt32s, add, subtract, and, or, xor, complement, shiftLeftBy, shiftRightZfBy, rotateLeftBy, rotateRightBy, signedCompare, unsignedCompare, toSignedString, toUnsignedString, toHex, toBitString, decoder, encoder, toByteValues, toBits, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Int64" ]


{-| Convert a `Int` to `Int64`. This is guaranteed to work for integers in the [safe JS range](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER).

    fromInt 42
        |> toSignedString
        --> "42"

fromInt: Int -> Int64.Int64
-}
fromInt : Int -> Elm.Expression
fromInt fromIntArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "fromInt"
             , annotation =
                 Just
                     (Type.function
                          [ Type.int ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ Elm.int fromIntArg ]


{-| Give two integers, corresponding to the upper and lower 32 bits

    fromInt32s 4 2
        |> toHex
        --> "0000000400000002"

fromInt32s: Int -> Int -> Int64.Int64
-}
fromInt32s : Int -> Int -> Elm.Expression
fromInt32s fromInt32sArg fromInt32sArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "fromInt32s"
             , annotation =
                 Just
                     (Type.function
                          [ Type.int, Type.int ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ Elm.int fromInt32sArg, Elm.int fromInt32sArg0 ]


{-| 64-bit addition, with correct overflow

    fromInt32s 0xFFFFFFFF 0xFFFFFFFF
        |> Int64.add (Int64.fromInt 1)
        |> Int64.toUnsignedString
        --> "0"

    fromInt32s 0xFFFFFFFF 0xFFFFFFFF
        |> Int64.add (Int64.fromInt 2)
        |> Int64.toUnsignedString
        --> "1"

add: Int64.Int64 -> Int64.Int64 -> Int64.Int64
-}
add : Elm.Expression -> Elm.Expression -> Elm.Expression
add addArg addArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "add"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" []
                          , Type.namedWith [ "Int64" ] "Int64" []
                          ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ addArg, addArg0 ]


{-| 64-bit subtraction, with correct overflow

    -- equivalent to `0 - 1`
    Int64.subtract  (Int64.fromInt 0) (Int64.fromInt 1)
        |> Int64.toUnsignedString
        --> "18446744073709551615"

    -- equivalent to `0 - 1`
    Int64.subtract  (Int64.fromInt 0) (Int64.fromInt 1)
        |> Int64.toSignedString
        --> "-1"


    -- equivalent to `1 - 0`
    Int64.subtract  (Int64.fromInt 1) (Int64.fromInt 0)
        |> Int64.toUnsignedString
        --> "1"

subtract: Int64.Int64 -> Int64.Int64 -> Int64.Int64
-}
subtract : Elm.Expression -> Elm.Expression -> Elm.Expression
subtract subtractArg subtractArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "subtract"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" []
                          , Type.namedWith [ "Int64" ] "Int64" []
                          ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ subtractArg, subtractArg0 ]


{-| Bitwise and

and: Int64.Int64 -> Int64.Int64 -> Int64.Int64
-}
and : Elm.Expression -> Elm.Expression -> Elm.Expression
and andArg andArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "and"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" []
                          , Type.namedWith [ "Int64" ] "Int64" []
                          ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ andArg, andArg0 ]


{-| Bitwise or

or: Int64.Int64 -> Int64.Int64 -> Int64.Int64
-}
or : Elm.Expression -> Elm.Expression -> Elm.Expression
or orArg orArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "or"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" []
                          , Type.namedWith [ "Int64" ] "Int64" []
                          ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ orArg, orArg0 ]


{-| Bitwise xor

xor: Int64.Int64 -> Int64.Int64 -> Int64.Int64
-}
xor : Elm.Expression -> Elm.Expression -> Elm.Expression
xor xorArg xorArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "xor"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" []
                          , Type.namedWith [ "Int64" ] "Int64" []
                          ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ xorArg, xorArg0 ]


{-| Bitwise complement

complement: Int64.Int64 -> Int64.Int64
-}
complement : Elm.Expression -> Elm.Expression
complement complementArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "complement"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" [] ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ complementArg ]


{-| Left bitwise shift, typically written `<<`

Fills in zeros from the right.

    Int64.fromInt32s 0xDEADBEAF 0xBAAAAAAD
        |> Int64.shiftLeftBy 16
        |> Int64.toHex
        --> "beafbaaaaaad0000"

shiftLeftBy: Int -> Int64.Int64 -> Int64.Int64
-}
shiftLeftBy : Int -> Elm.Expression -> Elm.Expression
shiftLeftBy shiftLeftByArg shiftLeftByArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "shiftLeftBy"
             , annotation =
                 Just
                     (Type.function
                          [ Type.int, Type.namedWith [ "Int64" ] "Int64" [] ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ Elm.int shiftLeftByArg, shiftLeftByArg0 ]


{-| Right bitwise shift, typically written `>>` (but `>>>` in JavaScript)

Fills in zeros from the left.

    Int64.fromInt32s 0xDEADBEAF 0xBAAAAAAD
        |> Int64.shiftRightZfBy 16
        |> Int64.toHex
        --> "0000deadbeafbaaa"

shiftRightZfBy: Int -> Int64.Int64 -> Int64.Int64
-}
shiftRightZfBy : Int -> Elm.Expression -> Elm.Expression
shiftRightZfBy shiftRightZfByArg shiftRightZfByArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "shiftRightZfBy"
             , annotation =
                 Just
                     (Type.function
                          [ Type.int, Type.namedWith [ "Int64" ] "Int64" [] ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ Elm.int shiftRightZfByArg, shiftRightZfByArg0 ]


{-| Left bitwise rotation

    Int64.fromInt32s 0xDEADBEAF 0xBAAAAAAD
        |> Int64.rotateLeftBy 16
        |> Int64.toHex
        --> "beafbaaaaaaddead"

rotateLeftBy: Int -> Int64.Int64 -> Int64.Int64
-}
rotateLeftBy : Int -> Elm.Expression -> Elm.Expression
rotateLeftBy rotateLeftByArg rotateLeftByArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "rotateLeftBy"
             , annotation =
                 Just
                     (Type.function
                          [ Type.int, Type.namedWith [ "Int64" ] "Int64" [] ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ Elm.int rotateLeftByArg, rotateLeftByArg0 ]


{-| Right bitwise rotation

    Int64.fromInt32s 0xDEADBEAF 0xBAAAAAAD
        |> Int64.rotateRightBy 16
        |> Int64.toHex
        --> "aaaddeadbeafbaaa"

rotateRightBy: Int -> Int64.Int64 -> Int64.Int64
-}
rotateRightBy : Int -> Elm.Expression -> Elm.Expression
rotateRightBy rotateRightByArg rotateRightByArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "rotateRightBy"
             , annotation =
                 Just
                     (Type.function
                          [ Type.int, Type.namedWith [ "Int64" ] "Int64" [] ]
                          (Type.namedWith [ "Int64" ] "Int64" [])
                     )
             }
        )
        [ Elm.int rotateRightByArg, rotateRightByArg0 ]


{-| Compare two `Int64` values, intepreting the bits as a signed integer.

signedCompare: Int64.Int64 -> Int64.Int64 -> Basics.Order
-}
signedCompare : Elm.Expression -> Elm.Expression -> Elm.Expression
signedCompare signedCompareArg signedCompareArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "signedCompare"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" []
                          , Type.namedWith [ "Int64" ] "Int64" []
                          ]
                          (Type.namedWith [ "Basics" ] "Order" [])
                     )
             }
        )
        [ signedCompareArg, signedCompareArg0 ]


{-| Compare two `Int64` values, intepreting the bits as an unsigned integer.

unsignedCompare: Int64.Int64 -> Int64.Int64 -> Basics.Order
-}
unsignedCompare : Elm.Expression -> Elm.Expression -> Elm.Expression
unsignedCompare unsignedCompareArg unsignedCompareArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "unsignedCompare"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" []
                          , Type.namedWith [ "Int64" ] "Int64" []
                          ]
                          (Type.namedWith [ "Basics" ] "Order" [])
                     )
             }
        )
        [ unsignedCompareArg, unsignedCompareArg0 ]


{-| Interpret a `Int64` as an unsigned integer, and give its string representation

    toSignedString (fromInt 10)
        --> "10"

    toSignedString (fromInt -10)
        --> "-10"

toSignedString: Int64.Int64 -> String
-}
toSignedString : Elm.Expression -> Elm.Expression
toSignedString toSignedStringArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "toSignedString"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" [] ]
                          Type.string
                     )
             }
        )
        [ toSignedStringArg ]


{-| Interpret a `Int64` as an unsigned integer, and give its string representation

    toUnsignedString (fromInt 10)
        --> "10"

    toUnsignedString (fromInt -10)
        --> "18446744073709551606"

toUnsignedString: Int64.Int64 -> String
-}
toUnsignedString : Elm.Expression -> Elm.Expression
toUnsignedString toUnsignedStringArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "toUnsignedString"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" [] ]
                          Type.string
                     )
             }
        )
        [ toUnsignedStringArg ]


{-| Convert a `Int64` to a hexadecimal string

    toHex (fromInt (256 - 1))
        -->  "00000000000000ff"

toHex: Int64.Int64 -> String
-}
toHex : Elm.Expression -> Elm.Expression
toHex toHexArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "toHex"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" [] ]
                          Type.string
                     )
             }
        )
        [ toHexArg ]


{-| Bits as a string of `0`s and `1`s in big-endian order.

    toBitString (fromInt 42)
        --> "0000000000000000000000000000000000000000000000000000000000101010"

toBitString: Int64.Int64 -> String
-}
toBitString : Elm.Expression -> Elm.Expression
toBitString toBitStringArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "toBitString"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" [] ]
                          Type.string
                     )
             }
        )
        [ toBitStringArg ]


{-| A `elm/bytes` Decoder for `Int64`

decoder: Bytes.Endianness -> Bytes.Decode.Decoder Int64.Int64
-}
decoder : Elm.Expression -> Elm.Expression
decoder decoderArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "decoder"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Bytes" ] "Endianness" [] ]
                          (Type.namedWith
                               [ "Bytes", "Decode" ]
                               "Decoder"
                               [ Type.namedWith [ "Int64" ] "Int64" [] ]
                          )
                     )
             }
        )
        [ decoderArg ]


{-| A `elm/bytes` Encoder for `Int64`

encoder: Bytes.Endianness -> Int64.Int64 -> Bytes.Encode.Encoder
-}
encoder : Elm.Expression -> Elm.Expression -> Elm.Expression
encoder encoderArg encoderArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "encoder"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Bytes" ] "Endianness" []
                          , Type.namedWith [ "Int64" ] "Int64" []
                          ]
                          (Type.namedWith [ "Bytes", "Encode" ] "Encoder" [])
                     )
             }
        )
        [ encoderArg, encoderArg0 ]


{-| Convert an `Int64` to its 8 byte values in big-endian order

    toByteValues  (fromInt 0xDEADBEAF)
        --> [0,0,0,0,222,173,190,175]

toByteValues: Int64.Int64 -> List Int
-}
toByteValues : Elm.Expression -> Elm.Expression
toByteValues toByteValuesArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "toByteValues"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" [] ]
                          (Type.list Type.int)
                     )
             }
        )
        [ toByteValuesArg ]


{-| The individual bits as a list of `Bool` in big-endian order.

    toBits (fromInt 10)
        --> [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False,True,False]

toBits: Int64.Int64 -> List Bool
-}
toBits : Elm.Expression -> Elm.Expression
toBits toBitsArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Int64" ]
             , name = "toBits"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Int64" ] "Int64" [] ]
                          (Type.list Type.bool)
                     )
             }
        )
        [ toBitsArg ]


annotation_ : { int64 : Type.Annotation }
annotation_ =
    { int64 = Type.namedWith [ "Int64" ] "Int64" [] }


call_ :
    { fromInt : Elm.Expression -> Elm.Expression
    , fromInt32s : Elm.Expression -> Elm.Expression -> Elm.Expression
    , add : Elm.Expression -> Elm.Expression -> Elm.Expression
    , subtract : Elm.Expression -> Elm.Expression -> Elm.Expression
    , and : Elm.Expression -> Elm.Expression -> Elm.Expression
    , or : Elm.Expression -> Elm.Expression -> Elm.Expression
    , xor : Elm.Expression -> Elm.Expression -> Elm.Expression
    , complement : Elm.Expression -> Elm.Expression
    , shiftLeftBy : Elm.Expression -> Elm.Expression -> Elm.Expression
    , shiftRightZfBy : Elm.Expression -> Elm.Expression -> Elm.Expression
    , rotateLeftBy : Elm.Expression -> Elm.Expression -> Elm.Expression
    , rotateRightBy : Elm.Expression -> Elm.Expression -> Elm.Expression
    , signedCompare : Elm.Expression -> Elm.Expression -> Elm.Expression
    , unsignedCompare : Elm.Expression -> Elm.Expression -> Elm.Expression
    , toSignedString : Elm.Expression -> Elm.Expression
    , toUnsignedString : Elm.Expression -> Elm.Expression
    , toHex : Elm.Expression -> Elm.Expression
    , toBitString : Elm.Expression -> Elm.Expression
    , decoder : Elm.Expression -> Elm.Expression
    , encoder : Elm.Expression -> Elm.Expression -> Elm.Expression
    , toByteValues : Elm.Expression -> Elm.Expression
    , toBits : Elm.Expression -> Elm.Expression
    }
call_ =
    { fromInt =
        \fromIntArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "fromInt"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.int ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ fromIntArg ]
    , fromInt32s =
        \fromInt32sArg fromInt32sArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "fromInt32s"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.int, Type.int ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ fromInt32sArg, fromInt32sArg0 ]
    , add =
        \addArg addArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "add"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" []
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ addArg, addArg0 ]
    , subtract =
        \subtractArg subtractArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "subtract"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" []
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ subtractArg, subtractArg0 ]
    , and =
        \andArg andArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "and"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" []
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ andArg, andArg0 ]
    , or =
        \orArg orArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "or"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" []
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ orArg, orArg0 ]
    , xor =
        \xorArg xorArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "xor"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" []
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ xorArg, xorArg0 ]
    , complement =
        \complementArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "complement"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" [] ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ complementArg ]
    , shiftLeftBy =
        \shiftLeftByArg shiftLeftByArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "shiftLeftBy"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.int
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ shiftLeftByArg, shiftLeftByArg0 ]
    , shiftRightZfBy =
        \shiftRightZfByArg shiftRightZfByArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "shiftRightZfBy"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.int
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ shiftRightZfByArg, shiftRightZfByArg0 ]
    , rotateLeftBy =
        \rotateLeftByArg rotateLeftByArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "rotateLeftBy"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.int
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ rotateLeftByArg, rotateLeftByArg0 ]
    , rotateRightBy =
        \rotateRightByArg rotateRightByArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "rotateRightBy"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.int
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Int64" ] "Int64" [])
                             )
                     }
                )
                [ rotateRightByArg, rotateRightByArg0 ]
    , signedCompare =
        \signedCompareArg signedCompareArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "signedCompare"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" []
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Basics" ] "Order" [])
                             )
                     }
                )
                [ signedCompareArg, signedCompareArg0 ]
    , unsignedCompare =
        \unsignedCompareArg unsignedCompareArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "unsignedCompare"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" []
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith [ "Basics" ] "Order" [])
                             )
                     }
                )
                [ unsignedCompareArg, unsignedCompareArg0 ]
    , toSignedString =
        \toSignedStringArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "toSignedString"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" [] ]
                                  Type.string
                             )
                     }
                )
                [ toSignedStringArg ]
    , toUnsignedString =
        \toUnsignedStringArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "toUnsignedString"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" [] ]
                                  Type.string
                             )
                     }
                )
                [ toUnsignedStringArg ]
    , toHex =
        \toHexArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "toHex"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" [] ]
                                  Type.string
                             )
                     }
                )
                [ toHexArg ]
    , toBitString =
        \toBitStringArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "toBitString"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" [] ]
                                  Type.string
                             )
                     }
                )
                [ toBitStringArg ]
    , decoder =
        \decoderArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "decoder"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Bytes" ] "Endianness" [] ]
                                  (Type.namedWith
                                       [ "Bytes", "Decode" ]
                                       "Decoder"
                                       [ Type.namedWith [ "Int64" ] "Int64" [] ]
                                  )
                             )
                     }
                )
                [ decoderArg ]
    , encoder =
        \encoderArg encoderArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "encoder"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Bytes" ] "Endianness" []
                                  , Type.namedWith [ "Int64" ] "Int64" []
                                  ]
                                  (Type.namedWith
                                       [ "Bytes", "Encode" ]
                                       "Encoder"
                                       []
                                  )
                             )
                     }
                )
                [ encoderArg, encoderArg0 ]
    , toByteValues =
        \toByteValuesArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "toByteValues"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" [] ]
                                  (Type.list Type.int)
                             )
                     }
                )
                [ toByteValuesArg ]
    , toBits =
        \toBitsArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Int64" ]
                     , name = "toBits"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Int64" ] "Int64" [] ]
                                  (Type.list Type.bool)
                             )
                     }
                )
                [ toBitsArg ]
    }


values_ :
    { fromInt : Elm.Expression
    , fromInt32s : Elm.Expression
    , add : Elm.Expression
    , subtract : Elm.Expression
    , and : Elm.Expression
    , or : Elm.Expression
    , xor : Elm.Expression
    , complement : Elm.Expression
    , shiftLeftBy : Elm.Expression
    , shiftRightZfBy : Elm.Expression
    , rotateLeftBy : Elm.Expression
    , rotateRightBy : Elm.Expression
    , signedCompare : Elm.Expression
    , unsignedCompare : Elm.Expression
    , toSignedString : Elm.Expression
    , toUnsignedString : Elm.Expression
    , toHex : Elm.Expression
    , toBitString : Elm.Expression
    , decoder : Elm.Expression
    , encoder : Elm.Expression
    , toByteValues : Elm.Expression
    , toBits : Elm.Expression
    }
values_ =
    { fromInt =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "fromInt"
            , annotation =
                Just
                    (Type.function
                         [ Type.int ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , fromInt32s =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "fromInt32s"
            , annotation =
                Just
                    (Type.function
                         [ Type.int, Type.int ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , add =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "add"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" []
                         , Type.namedWith [ "Int64" ] "Int64" []
                         ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , subtract =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "subtract"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" []
                         , Type.namedWith [ "Int64" ] "Int64" []
                         ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , and =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "and"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" []
                         , Type.namedWith [ "Int64" ] "Int64" []
                         ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , or =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "or"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" []
                         , Type.namedWith [ "Int64" ] "Int64" []
                         ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , xor =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "xor"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" []
                         , Type.namedWith [ "Int64" ] "Int64" []
                         ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , complement =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "complement"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" [] ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , shiftLeftBy =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "shiftLeftBy"
            , annotation =
                Just
                    (Type.function
                         [ Type.int, Type.namedWith [ "Int64" ] "Int64" [] ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , shiftRightZfBy =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "shiftRightZfBy"
            , annotation =
                Just
                    (Type.function
                         [ Type.int, Type.namedWith [ "Int64" ] "Int64" [] ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , rotateLeftBy =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "rotateLeftBy"
            , annotation =
                Just
                    (Type.function
                         [ Type.int, Type.namedWith [ "Int64" ] "Int64" [] ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , rotateRightBy =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "rotateRightBy"
            , annotation =
                Just
                    (Type.function
                         [ Type.int, Type.namedWith [ "Int64" ] "Int64" [] ]
                         (Type.namedWith [ "Int64" ] "Int64" [])
                    )
            }
    , signedCompare =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "signedCompare"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" []
                         , Type.namedWith [ "Int64" ] "Int64" []
                         ]
                         (Type.namedWith [ "Basics" ] "Order" [])
                    )
            }
    , unsignedCompare =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "unsignedCompare"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" []
                         , Type.namedWith [ "Int64" ] "Int64" []
                         ]
                         (Type.namedWith [ "Basics" ] "Order" [])
                    )
            }
    , toSignedString =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "toSignedString"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" [] ]
                         Type.string
                    )
            }
    , toUnsignedString =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "toUnsignedString"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" [] ]
                         Type.string
                    )
            }
    , toHex =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "toHex"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" [] ]
                         Type.string
                    )
            }
    , toBitString =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "toBitString"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" [] ]
                         Type.string
                    )
            }
    , decoder =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "decoder"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Bytes" ] "Endianness" [] ]
                         (Type.namedWith
                              [ "Bytes", "Decode" ]
                              "Decoder"
                              [ Type.namedWith [ "Int64" ] "Int64" [] ]
                         )
                    )
            }
    , encoder =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "encoder"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Bytes" ] "Endianness" []
                         , Type.namedWith [ "Int64" ] "Int64" []
                         ]
                         (Type.namedWith [ "Bytes", "Encode" ] "Encoder" [])
                    )
            }
    , toByteValues =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "toByteValues"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" [] ]
                         (Type.list Type.int)
                    )
            }
    , toBits =
        Elm.value
            { importFrom = [ "Int64" ]
            , name = "toBits"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Int64" ] "Int64" [] ]
                         (Type.list Type.bool)
                    )
            }
    }