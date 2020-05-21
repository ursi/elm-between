module Between exposing
    ( Between
    , create
    , first
    , indexedMapInner
    , indexedMapOuter
    , indexedPad
    , indexedSurround
    , indexedToList
    , inner
    , last
    , length
    , mapOuter
    , outer
    , pad
    , push
    , setInner
    , setOuter
    , singleton
    , surround
    , toList
    )

import Array exposing (Array)


type Between outer inner
    = Between ( outer, Array ( inner, outer ) )


create : outer -> List ( inner, outer ) -> Between outer inner
create first_ rest =
    Between ( first_, Array.fromList rest )


pad : i -> List o -> Maybe (Between o i)
pad inner_ list =
    case list of
        first_ :: rest ->
            Just <|
                Between
                    ( first_
                    , Array.fromList <| List.map (Tuple.pair inner_) rest
                    )

        [] ->
            Nothing


indexedPad : (Int -> i) -> List o -> Maybe (Between o i)
indexedPad f list =
    case list of
        first_ :: rest ->
            Just <|
                Between
                    ( first_
                    , Array.fromList <| List.indexedMap (\i outer_ -> ( f i, outer_ )) rest
                    )

        [] ->
            Nothing


toBeNamed2 : (o -> o -> i) -> List o -> Maybe (Between o i)
toBeNamed2 f list =
    case list of
        only :: [] ->
            Just <| Between ( only, Array.empty )

        first_ :: rest ->
            Just <|
                Between
                    ( first_
                    , Array.fromList <|
                        List.map2 Tuple.pair
                            (toBeNamed2Helper f list)
                            list
                    )

        [] ->
            Nothing


toBeNamed2Helper : (o -> o -> i) -> List o -> List i
toBeNamed2Helper f list =
    case list of
        first_ :: last_ :: [] ->
            [ f first_ last_ ]

        first_ :: second :: rest ->
            f first_ second :: toBeNamed2Helper f rest

        _ ->
            []


surround : o -> List i -> Between o i
surround outer_ list =
    Between ( outer_, Array.fromList <| List.map (Tuple.pair >> (|>) outer_) list )


indexedSurround : (Int -> o) -> List i -> Between o i
indexedSurround f list =
    Between
        ( f 0
        , Array.fromList <|
            List.indexedMap
                (\i inner_ -> ( inner_, f (i + 1) ))
                list
        )


toBeNamed : (Maybe i -> Maybe i -> o) -> List i -> Between o i
toBeNamed f list =
    let
        array =
            Array.fromList list
    in
    Between
        ( f Nothing <| List.head list
        , Array.fromList <|
            List.indexedMap
                (\i inner_ ->
                    ( inner_, f (Array.get i array) (Array.get (i + 1) array) )
                )
                list
        )


push : ( i, o ) -> Between o i -> Between o i
push last_ (Between ( first_, array )) =
    Between
        ( first_, Array.push last_ array )


map : (o -> a) -> (i -> b) -> Between o i -> Between a b
map mapOuter_ innerMap_ (Between ( first_, array )) =
    Between
        ( mapOuter_ first_
        , Array.map (\( i, o ) -> ( innerMap_ i, mapOuter_ o )) array
        )


mapOuter : (o -> a) -> Between o i -> Between a i
mapOuter map_ (Between ( first_, array )) =
    Between
        ( map_ first_
        , Array.map
            (\( i, o ) -> ( i, map_ o ))
            array
        )


indexedMapOuter : (Int -> o -> a) -> Between o i -> Between a i
indexedMapOuter map_ (Between ( first_, array )) =
    Between
        ( map_ 0 first_
        , Array.indexedMap
            (\i ( inner_, outer_ ) -> ( inner_, map_ (i + 1) outer_ ))
            array
        )



-- TODO neighbor map


innerMap : (i -> a) -> Between o i -> Between o a
innerMap map_ (Between ( first_, array )) =
    Between
        ( first_
        , Array.map
            (\( i, o ) -> ( map_ i, o ))
            array
        )


indexedMapInner : (Int -> i -> a) -> Between o i -> Between o a
indexedMapInner map_ (Between ( first_, array )) =
    Between
        ( first_
        , Array.indexedMap
            (\i ( inner_, outer_ ) -> ( map_ i inner_, outer_ ))
            array
        )



-- TODO indexedMapInner
-- TODO neighbor map


toList : (o -> a) -> (i -> a) -> Between o i -> List a
toList mapOuter_ innerMap_ (Between ( first_, array )) =
    let
        list =
            Array.toList array
    in
    mapOuter_ first_
        :: (List.concat <|
                List.map
                    (\( i, o ) -> [ innerMap_ i, mapOuter_ o ])
                    list
           )


indexedToList :
    (Int -> o -> a)
    -> (Int -> i -> a)
    -> Between o i
    -> List a
indexedToList mapOuter_ innerMap_ (Between ( first_, array )) =
    let
        list =
            Array.toList array
    in
    mapOuter_ 0 first_
        :: (List.concat <|
                List.indexedMap
                    (\index ( i, o ) -> [ innerMap_ index i, mapOuter_ (index + 1) o ])
                    list
           )


inner : Between o i -> Array i
inner (Between ( _, array )) =
    Array.map Tuple.first array


outer : Between o i -> Array o
outer (Between between) =
    Array.fromList <|
        Tuple.first between
            :: (List.map Tuple.second <|
                    Array.toList <|
                        Tuple.second between
               )


first : Between o i -> o
first (Between ( first_, array )) =
    first_


last : Between o i -> o
last (Between ( first_, array )) =
    case Array.get (Array.length array - 1) array of
        Just last_ ->
            Tuple.second last_

        Nothing ->
            first_


setOuter : Int -> o -> Between o i -> Between o i
setOuter index new (Between ( first_, array )) =
    if index == 0 then
        Between ( new, array )

    else
        case Array.get (index - 1) array of
            Just ( inner_, _ ) ->
                Between ( first_, Array.set (index - 1) ( inner_, new ) array )

            Nothing ->
                Between ( first_, array )


setInner : Int -> i -> Between o i -> Between o i
setInner index new (Between ( first_, array )) =
    case Array.get index array of
        Just ( _, outer_ ) ->
            Between ( first_, Array.set index ( new, outer_ ) array )

        Nothing ->
            Between ( first_, array )


length : Between o i -> Int
length between =
    Array.length (outer between) + Array.length (inner between)


singleton : o -> Between o i
singleton outer_ =
    Between ( outer_, Array.empty )
