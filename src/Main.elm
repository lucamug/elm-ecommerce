port module Main exposing (..)

import Browser
import Browser.Dom
import Codec
import Element.WithContext exposing (..)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Events as Events
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import FeatherIcons
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Process
import Regex
import Set
import Task
import Url
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query



-- | MAIN TYPES
-- |     Model
-- |     LocalStorage
-- |     Language
-- |     ProductId
-- |     Sort
-- |     SortDirection
-- |     Page
-- |     Mode
-- |     HttpRequest
-- |     Product
-- |     Flags
-- | CONFIGURATION
-- | INIT
-- |     init
-- |     productInit
-- | ROUTING
-- | MSG
-- | UPDATE
-- |     update
-- |     updateMain
-- |     updateErrors
-- |     updateLocalStorage
-- | UPDATE HELPERS
-- | PORTS
-- | MAIN
-- | GENERIC HELPERS
-- | ATOMS
-- |     atomLogo
-- |     atomLinkInternal
-- |     atomPrice
-- |     atomImageProduct
-- |     atomTagNew
-- |     atomTagNumber
-- |     atomIcon
-- | MOLECULES
-- |     molMenuMain
-- |     molMenuSortBy
-- |     molAddToFavorites
-- |     molAddToCart
-- |     molAddToCartLarge
-- |     molProductForStrip
-- |     molProductForGrid
-- |     molProductForRow
-- |     molProductNotFound
-- |     molClose
-- |     molStars
-- |     molEmptyList
-- | ORGANISMS
-- |     orgHeader
-- |     orgProductsStrip
-- |     orgProductDetails
-- |     orgProductsGrid
-- |     orgProductsRows
-- |     orgFooter
-- |     orgError
-- | PALETTE
-- |     paletteLight
-- |     paletteDark
-- | VIEWS
-- |     view
-- |     viewCartAndFavoritesOverview
-- |     viewPage
-- | VIEW HELPERS
-- | VIEW ATTRIBUTES
-- |     mainAttrs
-- | CONTEXT
-- | DEBUGGING
-- | LOCAL STORAGE
-- | CODECS
-- | TRANSLATIONS
-- | PRODUCTS DATA
--
--
-- ███    ███  █████  ██ ███    ██     ████████ ██    ██ ██████  ███████ ███████
-- ████  ████ ██   ██ ██ ████   ██        ██     ██  ██  ██   ██ ██      ██
-- ██ ████ ██ ███████ ██ ██ ██  ██        ██      ████   ██████  █████   ███████
-- ██  ██  ██ ██   ██ ██ ██  ██ ██        ██       ██    ██      ██           ██
-- ██      ██ ██   ██ ██ ██   ████        ██       ██    ██      ███████ ███████
--| MAIN TYPES
--|     Model


type alias Model =
    -- Saved in Local Storage
    { cart : List ProductId
    , favorites : List ProductId
    , mode : Mode
    , language : Language
    , pwd : String
    , infoClosed : Bool

    -- NOT saved in Local Storage
    , temp : List ProductId
    , sort : Sort
    , sortDirection : SortDirection
    , page : Page
    , httpRequest : HttpRequest
    , warnings : List String
    , hideOverview : Bool
    , useEmbeddedData : Bool
    }



--|     LocalStorage


type alias LocalStorage =
    { cart : List String
    , favorites : List String
    , mode : Mode
    , language : Language
    , pwd : String
    , infoClosed : Bool
    }



--|     Language


type Language
    = EN_US
    | JA_JP


languageToString : Language -> String
languageToString language =
    case language of
        EN_US ->
            "English"

        JA_JP ->
            "Japanese"



--|     ProductId


type ProductId
    = ProductId String



--|     Sort


type Sort
    = Price
    | Stars
    | Alpha



--|     SortDirection


type SortDirection
    = Ascending
    | Descending



--|     Page


type Page
    = Top (Maybe String)
    | Favorites (Maybe String)
    | Cart (Maybe String)
    | Details ProductId
    | NotFound String



--|     Mode


type Mode
    = Light
    | Dark



--|     HttpRequest


type HttpRequest
    = Fetching
    | Failure String
    | Success (List Product)



--|     Product


type alias Product =
    { id : ProductId
    , name : String
    , description : String
    , stars : Int
    , price : Int
    , tag : String
    , bg : Int
    }



--|     Flags


type alias Flags =
    { localStorage : String
    , locationHref : String
    }



--  ██████  ██████  ███    ██ ███████ ██  ██████  ██    ██ ██████   █████  ████████ ██  ██████  ███    ██
-- ██      ██    ██ ████   ██ ██      ██ ██       ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ████   ██
-- ██      ██    ██ ██ ██  ██ █████   ██ ██   ███ ██    ██ ██████  ███████    ██    ██ ██    ██ ██ ██  ██
-- ██      ██    ██ ██  ██ ██ ██      ██ ██    ██ ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ██  ██ ██
--  ██████  ██████  ██   ████ ██      ██  ██████   ██████  ██   ██ ██   ██    ██    ██  ██████  ██   ████
--| CONFIGURATION


type alias Configuration =
    { nameWebsite : String
    , mediaLocation : String
    , qrCodeContent : List ( String, String )
    , gridMinimum : Int
    , gridMaximum : Int
    , maxWidth : Int
    , debugging : Bool
    , paletteLight : Palette
    , paletteDark : Palette
    }


configuration : Configuration
configuration =
    -- conf contins things that don't change during the life of the app
    { nameWebsite = "elm eCommerce"
    , mediaLocation = "https://training-assets.surge.sh/media/"
    , qrCodeContent =
        [ ( "elm eCommerce", "https://elm-ecommerce.guupa.com " )
        , ( "elm eCommerce + dashboard", "https://elm-ecommerce-dashboard.guupa.com" )
        , ( "elm eCommerce in Ellie", "https://ellie-app.com/d2dKW5XHVcCa1" )
        , ( "source code", "https://github.com/lucamug/elm-ecommerce" )
        , ( "illustrations (いらすとや)", "https://www.irasutoya.com/" )
        , ( "icons", "https://feathericons.com/" )
        ]
    , gridMinimum = 165
    , gridMaximum = 340
    , maxWidth = 1000
    , debugging = False
    , paletteLight = paletteLight
    , paletteDark = paletteDark
    }



-- ██ ███    ██ ██ ████████
-- ██ ████   ██ ██    ██
-- ██ ██ ██  ██ ██    ██
-- ██ ██  ██ ██ ██    ██
-- ██ ██   ████ ██    ██
--| INIT
--|     init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { cart = []
            , favorites = []
            , mode = Light
            , language = EN_US
            , temp = []
            , sort = Stars
            , sortDirection = Descending
            , page = Top Nothing
            , httpRequest = Fetching
            , warnings = []
            , hideOverview = False
            , useEmbeddedData = True
            , pwd = ""
            , infoClosed = False
            }
                |> modelUpdatePage flags.locationHref
                |> (\m ->
                        --
                        -- `flags` is a string stored in the local storage containing
                        -- the model used to initialize our app on start.
                        -- https://guide.elm-lang.org/interop/flags.html
                        --
                        -- The string required to be parsed to check if it is well
                        -- formed.
                        --
                        case stringToLocalStorage flags.localStorage of
                            Ok localStorage ->
                                localStorageToModel localStorage m

                            Err _ ->
                                --
                                -- If no data in the local storage or the data is broken,
                                -- we initialize the model from scratch.
                                --
                                m
                   )
    in
    ( model, commandToRequestProducts model.useEmbeddedData model.language )
        |> andThen updateErrors DoNothing



--|     productInit


productInit : Product
productInit =
    { id = stringToProductId "spinner"
    , name = ""
    , description = ""
    , stars = 0
    , price = 0
    , tag = ""
    , bg = 4
    }



-- ██████   ██████  ██    ██ ████████ ██ ███    ██  ██████
-- ██   ██ ██    ██ ██    ██    ██    ██ ████   ██ ██
-- ██████  ██    ██ ██    ██    ██    ██ ██ ██  ██ ██   ███
-- ██   ██ ██    ██ ██    ██    ██    ██ ██  ██ ██ ██    ██
-- ██   ██  ██████   ██████     ██    ██ ██   ████  ██████
--| ROUTING


search : Url.Parser.Query.Parser (Maybe String)
search =
    Url.Parser.Query.string "search"


urlParser : Url.Parser.Parser (Page -> a) a
urlParser =
    let
        productIdToDetailPage productId =
            Details (stringToProductId productId)
    in
    Url.Parser.oneOf
        [ Url.Parser.map Top (Url.Parser.top <?> search)
        , Url.Parser.map Favorites (Url.Parser.s "favorites" <?> search)
        , Url.Parser.map Cart (Url.Parser.s "cart" <?> search)
        , Url.Parser.map productIdToDetailPage (Url.Parser.s "p" </> Url.Parser.string)
        ]


locationHrefToPage : String -> Page
locationHrefToPage locationHref =
    case Url.fromString locationHref of
        Nothing ->
            NotFound (locationHref ++ " is not a valid URL")

        Just url ->
            case Url.Parser.parse urlParser url of
                Nothing ->
                    NotFound ("Page " ++ locationHref ++ " not found")

                Just page ->
                    page


pageToSearchQuery : Page -> Maybe String
pageToSearchQuery page =
    case page of
        Top searchQuery ->
            searchQuery

        Favorites searchQuery ->
            searchQuery

        Cart searchQuery ->
            searchQuery

        Details _ ->
            Nothing

        NotFound _ ->
            Nothing


pageFromSearchQuery : Maybe String -> Page -> Page
pageFromSearchQuery searchQuery page =
    case page of
        Top _ ->
            Top searchQuery

        Favorites _ ->
            Favorites searchQuery

        Cart _ ->
            Cart searchQuery

        Details _ ->
            page

        NotFound _ ->
            page


pageToString : Page -> String
pageToString page =
    let
        ( path, maybeSearchQuery ) =
            case page of
                Top s ->
                    ( [], s )

                Favorites s ->
                    ( [ "favorites" ], s )

                Cart s ->
                    ( [ "cart" ], s )

                Details productId ->
                    ( [ "p", productIdToString productId ], Nothing )

                NotFound _ ->
                    ( [ "not_found" ], Nothing )
    in
    Url.Builder.absolute path
        (case maybeSearchQuery of
            Nothing ->
                []

            Just string ->
                [ Url.Builder.string "search" string ]
        )



-- ███    ███ ███████ ███████ ███████  █████   ██████  ███████ ███████
-- ████  ████ ██      ██      ██      ██   ██ ██       ██      ██
-- ██ ████ ██ █████   ███████ ███████ ███████ ██   ███ █████   ███████
-- ██  ██  ██ ██           ██      ██ ██   ██ ██    ██ ██           ██
-- ██      ██ ███████ ███████ ███████ ██   ██  ██████  ███████ ███████
--| MSG


type Msg
    = DoNothing
    | ChangeSearch String
    | ChangeSearchDelayed Page
    | ChangeUrl String
    | ChangeLocalStorage String
    | ChangePage Page
    | ChangeSort Sort
    | ChangeCart Product Int
    | ChangeLanguage Language
    | ChangePwd String
    | ToggleFavorite Product
    | ToggleSearch
    | ToggleMode
    | ToggleDirection
    | ToggleOverview
    | ToggleInfo
    | RemoveHttpError
    | Response (Result Http.Error (List Product))
    | EmptyCart
    | EmptyFavorite
    | EmptyWarnings



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████
--| UPDATE
--|     update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> andThen updateMain msg
        |> andThen updateErrors msg
        |> andThen updateLocalStorage msg



--|     updateMain


updateMain : Msg -> Model -> ( Model, Cmd Msg )
updateMain msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        ChangeSearch searchQuery ->
            let
                newPage =
                    pageFromSearchQuery (Just searchQuery) model.page
            in
            ( { model | page = newPage }
            , emitMessageWithDelay 1000 (ChangeSearchDelayed newPage)
            )

        ChangeSearchDelayed page ->
            if model.page == page then
                ( model, pushUrl (pageToString model.page) )

            else
                ( model, Cmd.none )

        ChangeLocalStorage localStorageAsString ->
            ( case stringToLocalStorage localStorageAsString of
                Ok localStorage ->
                    localStorageToModel localStorage model

                Err _ ->
                    model
            , Cmd.none
            )

        ChangeUrl locationHref ->
            --
            -- Message from JavaScript that the url changed
            --
            -- This is either
            --
            -- * The user moved back/forward
            --
            -- * A feedback from sending the command `pushUrl`
            --   (see message `ChangePage`) through a port.
            --
            ( model
                |> modelUpdatePage locationHref
                |> modelResetTemp
            , Cmd.none
            )

        ChangePage page ->
            let
                newPage =
                    if model.page == page then
                        Top Nothing

                    else
                        page
            in
            ( model, Cmd.batch [ resetViewport, pushUrl (pageToString newPage) ] )

        ChangeSort sort ->
            ( { model
                | sort = sort
                , sortDirection =
                    if sort == model.sort then
                        toggleDirection model.sortDirection

                    else
                        model.sortDirection
              }
            , Cmd.none
            )

        ChangeCart product quantity ->
            if 0 <= quantity && quantity <= 99 then
                ( { model
                    | cart =
                        model.cart
                            |> List.filter ((/=) product.id)
                            |> (++) (List.repeat quantity product.id)
                    , temp =
                        if quantity == 0 then
                            model.temp
                                |> (::) product.id

                        else
                            model.temp
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ChangeLanguage language ->
            ( { model | language = language }, commandToRequestProducts model.useEmbeddedData language )

        ChangePwd pwd_ ->
            ( { model | pwd = pwd_ }, Cmd.none )

        ToggleDirection ->
            ( { model | sortDirection = toggleDirection model.sortDirection }, Cmd.none )

        ToggleSearch ->
            ( { model
                | page =
                    case model.page of
                        Details _ ->
                            Top (Just "")

                        _ ->
                            case pageToSearchQuery model.page of
                                Just _ ->
                                    pageFromSearchQuery Nothing model.page

                                Nothing ->
                                    pageFromSearchQuery (Just "") model.page
              }
            , Task.attempt (\_ -> DoNothing) (Browser.Dom.focus "search-box")
            )

        ToggleFavorite product ->
            let
                isFavorite =
                    List.member product.id model.favorites
            in
            ( { model
                | favorites =
                    if isFavorite then
                        List.filter ((/=) product.id) model.favorites

                    else
                        product.id :: model.favorites
                , temp =
                    if isFavorite then
                        product.id :: model.temp

                    else
                        List.filter ((/=) product.id) model.temp
              }
            , Cmd.none
            )

        ToggleMode ->
            ( { model
                | mode =
                    case model.mode of
                        Light ->
                            Dark

                        Dark ->
                            Light
              }
            , Cmd.none
            )

        ToggleOverview ->
            ( { model | hideOverview = not model.hideOverview }, Cmd.none )

        ToggleInfo ->
            ( { model | infoClosed = not model.infoClosed }, Cmd.none )

        Response response ->
            ( model |> modelUpdateHttpRequest response, Cmd.none )

        RemoveHttpError ->
            ( { model | httpRequest = Fetching }, Cmd.none )

        EmptyCart ->
            ( { model | cart = [] }, Cmd.none )

        EmptyFavorite ->
            ( { model | favorites = [] }, Cmd.none )

        EmptyWarnings ->
            -- This is called when an error is closed
            ( { model | warnings = [] }, Cmd.none )



--|     updateErrors


updateErrors : Msg -> Model -> ( Model, Cmd Msg )
updateErrors _ model =
    ( -- Here we transfer errors to the warning system and then we show
      -- the Top page so that users can interact with the app also in
      -- case of errors.
      case model.page of
        NotFound string ->
            { model
                | page = Top Nothing
                , warnings = string :: model.warnings
            }

        Details productId ->
            case productIdToProduct model.httpRequest productId of
                Just _ ->
                    model

                Nothing ->
                    { model
                        | page = Top Nothing
                        , warnings = ("Product " ++ productIdToString productId ++ " not found") :: model.warnings
                    }

        _ ->
            model
    , Cmd.none
    )



--|     updateLocalStorage


updateLocalStorage : Msg -> Model -> ( Model, Cmd Msg )
updateLocalStorage msg model =
    ( model
    , case msg of
        ChangeLocalStorage _ ->
            -- This is the only case where we don't "pushLocalStorage"
            -- to avoid generating an infinite loop
            Cmd.none

        _ ->
            -- We save the model to the local storage
            model
                |> modelToLocalStorage
                |> localStorageToString
                |> pushLocalStorage
    )



-- ██    ██ ██████  ██████   █████  ████████ ███████     ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██          ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████       ███████ █████   ██      ██████  █████   ██████  ███████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██          ██   ██ ██      ██      ██      ██      ██   ██      ██
--  ██████  ██      ██████  ██   ██    ██    ███████     ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
--| UPDATE HELPERS


andThen : (msg -> model -> ( model, Cmd a )) -> msg -> ( model, Cmd a ) -> ( model, Cmd a )
andThen updater msg ( model, cmd ) =
    let
        ( modelNew, cmdNew ) =
            updater msg model
    in
    ( modelNew, Cmd.batch [ cmd, cmdNew ] )


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> DoNothing) (Browser.Dom.setViewport 0 0)


emitMessageWithDelay : Float -> msg -> Cmd msg
emitMessageWithDelay millis msg =
    Task.perform (always msg) (Process.sleep millis)


toggleDirection : SortDirection -> SortDirection
toggleDirection sortDirection =
    case sortDirection of
        Ascending ->
            Descending

        Descending ->
            Ascending


modelResetTemp : Model -> Model
modelResetTemp m =
    { m | temp = [] }


modelUpdatePage : String -> Model -> Model
modelUpdatePage locationHref model =
    { model
        | page =
            locationHref
                |> locationHrefToPage
    }


modelUpdateHttpRequest : Result Http.Error (List Product) -> Model -> Model
modelUpdateHttpRequest response model =
    case response of
        Ok listProducts ->
            { model | httpRequest = Success listProducts }

        Err error ->
            { model | httpRequest = Failure <| httpErrorToString error }



-- ██████   ██████  ██████  ████████ ███████
-- ██   ██ ██    ██ ██   ██    ██    ██
-- ██████  ██    ██ ██████     ██    ███████
-- ██      ██    ██ ██   ██    ██         ██
-- ██       ██████  ██   ██    ██    ███████
--| PORTS


port pushLocalStorage : String -> Cmd msg


port pushUrl : String -> Cmd msg


port onLocalStorageChange : (String -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg



-- ███    ███  █████  ██ ███    ██
-- ████  ████ ██   ██ ██ ████   ██
-- ██ ████ ██ ███████ ██ ██ ██  ██
-- ██  ██  ██ ██   ██ ██ ██  ██ ██
-- ██      ██ ██   ██ ██ ██   ████
--| MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title =
                    if Maybe.withDefault "" (pageToSearchQuery model.page) == "" then
                        configuration.nameWebsite

                    else
                        "\"" ++ Maybe.withDefault "" (pageToSearchQuery model.page) ++ "\" " ++ configuration.nameWebsite
                , body = [ view configuration model ]
                }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onUrlChange ChangeUrl
        , onLocalStorageChange ChangeLocalStorage
        ]



-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
--| GENERIC HELPERS


commandToRequestProducts : Bool -> Language -> Cmd Msg
commandToRequestProducts useEmbeddedData language =
    if useEmbeddedData then
        emitMessageWithDelay 1000
            (Response
                (case Codec.decodeString (Codec.list codecProduct) (productsDataToString <| productsData language) of
                    Ok products ->
                        Ok products

                    Err err ->
                        Err <| Http.BadBody (Json.Decode.errorToString err)
                )
            )

    else
        Http.get
            { url = "/products.json"
            , expect = Http.expectJson Response decoderListProducts
            }


productIdToProduct : HttpRequest -> ProductId -> Maybe Product
productIdToProduct httpRequest productId =
    case httpRequest of
        Success products ->
            products
                |> List.filter (\product -> product.id == productId)
                |> List.head

        _ ->
            Just productInit


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl string ->
            "Bad URL " ++ string

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus status ->
            "Bad Status " ++ String.fromInt status

        Http.BadBody string ->
            string


quantityInTheCart : List ProductId -> Product -> Int
quantityInTheCart cart product =
    cart
        |> List.filter ((==) product.id)
        |> List.length


filterSearch : Maybe String -> List Product -> List Product
filterSearch maybeSearchQuery products =
    case maybeSearchQuery of
        Just searchQuery ->
            List.filter (\product -> String.contains (String.toLower (String.trim searchQuery)) (String.toLower product.name)) products

        Nothing ->
            products


sortBy : Sort -> SortDirection -> List Product -> List Product
sortBy sort sortDirection products =
    products
        |> (case sort of
                Price ->
                    List.sortBy .price

                Stars ->
                    List.sortBy (\p -> String.fromInt p.stars ++ p.name)

                Alpha ->
                    List.sortBy .name
           )
        |> (case sortDirection of
                Ascending ->
                    List.reverse

                Descending ->
                    identity
           )


removeNothings : List (Maybe a) -> List a
removeNothings =
    let
        cons item list =
            case item of
                Just v ->
                    v :: list

                Nothing ->
                    list
    in
    List.foldr cons []


addThousandSeparator : Int -> String
addThousandSeparator int =
    int
        |> String.fromInt
        |> String.split ""
        |> List.reverse
        |> splitEvery 3
        |> intercalate [ "," ]
        |> List.reverse
        |> String.join ""


splitEvery : Int -> List a -> List (List a)
splitEvery n l =
    let
        loop : Int -> List a -> List (List a) -> List (List a)
        loop n_ rem acc =
            case rem of
                [] ->
                    List.reverse acc

                _ ->
                    loop n_ (List.drop n_ rem) (List.take n_ rem :: acc)
    in
    loop n l []


intercalate : List a -> List (List a) -> List a
intercalate list listOfList =
    List.concat (List.intersperse list listOfList)


correctPwd : String
correctPwd =
    -- String.reverse "netukar"
    ""



--  █████  ████████  ██████  ███    ███ ███████
-- ██   ██    ██    ██    ██ ████  ████ ██
-- ███████    ██    ██    ██ ██ ████ ██ ███████
-- ██   ██    ██    ██    ██ ██  ██  ██      ██
-- ██   ██    ██     ██████  ██      ██ ███████
--| ATOMS
--|     atomLogo


atomLogo : List (Attribute msg) -> { b | color : Color, size : Float } -> Element msg
atomLogo attrs args =
    row
        ([ tag "atomLogo"
         , spacing (round (args.size / 3))
         , Font.size (round args.size)
         , Font.light
         , Font.color args.color
         ]
            ++ attrs
        )
        [ text "elm"
        , FeatherIcons.shoppingCart
            |> FeatherIcons.withSize (args.size + 17)
            |> FeatherIcons.withStrokeWidth 1
            |> FeatherIcons.toHtml
                [ Html.Attributes.style "stroke" (colorToCssString args.color) ]
            |> html
            |> el [ moveLeft (args.size / 7), moveUp (args.size / 3) ]
        , text "eCommerce"
        ]



--|     atomLinkInternal


atomLinkInternal : List (Attribute Msg) -> { label : Element Msg, page : Page } -> Element Msg
atomLinkInternal attrs args =
    link
        ([ tag "atomLinkInternal", preventDefault (ChangePage args.page) ] ++ attrs)
        { label = args.label
        , url = pageToString args.page
        }



--|     atomPrice


atomPrice : List (Attribute msg) -> Int -> Int -> Element msg
atomPrice attrs fontSize price =
    row
        ([ tag "atomPrice"
         , Font.size fontSize
         , Font.bold
         , attrWithContext <| \c -> Font.color c.palette.onBackgroundDim
         , spacing 2
         ]
            ++ attrs
        )
        [ text <| addThousandSeparator price
        , el [ Font.size <| round (toFloat fontSize / 5 * 3), alignBottom, moveUp 2 ] <| text "円"
        ]



--|     atomImageProduct


atomImageProduct : List (Attribute msg) -> Int -> Product -> Element msg
atomImageProduct attrs paddingSize product =
    elementWithContext <|
        \c ->
            let
                backgroundColor =
                    if product.bg == 0 then
                        -- violet
                        rgb255 198 149 255

                    else if product.bg == 1 then
                        -- pink
                        rgb255 247 183 196

                    else if product.bg == 2 then
                        -- yellow
                        rgb255 238 249 1

                    else if product.bg == 3 then
                        -- light blue
                        rgb255 80 181 237

                    else
                        rgb255 245 245 245

                file =
                    fileName product.id c.conf.mediaLocation
            in
            el
                ([ tag "atomImageProduct"
                 , Background.color <| backgroundColor
                 , width fill
                 , height fill
                 , padding paddingSize
                 ]
                    ++ attrs
                )
            <|
                image
                    [ width (fill |> minimum 30)
                    , Background.uncropped file
                    , inFront <| image [ width fill, clip, alpha 0 ] { description = "", src = file }
                    , clip
                    ]
                <|
                    -- This data:image is a 400px X 400px transparent filler png file
                    { src = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAZAAAAGQAQMAAAC6caSPAAAAA1BMVEUAAACnej3aAAAAAXRSTlMAQObYZgAAACpJREFUeNrtwYEAAAAAw6D7Ux9gCtUAAAAAAAAAAAAAAAAAAAAAAAAAgDhPsAABa0Zz3QAAAABJRU5ErkJggg=="
                    , description = product.name
                    }



--|     atomTagNew


atomTagNew : List (Attribute msg) -> String -> Element msg
atomTagNew attrs tag_ =
    el
        ([ tag "atomTagNew"
         , paddingXY 5 4
         , attrWithContext <| \c -> Background.color c.palette.primary
         , attrWithContext <| \c -> Font.color c.palette.onPrimary
         , Font.size 11
         , Border.rounded 4
         ]
            ++ attrs
        )
    <|
        text tag_



--|     atomTagNumber


atomTagNumber : List (Attribute msg) -> Int -> Element msg
atomTagNumber attrs quantity =
    if quantity == 0 then
        none

    else
        el
            ([ tag "atomTagNumber"
             , paddingEach { top = 3, right = 6, bottom = 3, left = 6 }
             , Border.rounded 20
             , Font.center
             , Font.size 13
             , attrWithContext <| \c -> Background.color c.palette.onExternal
             , attrWithContext <| \c -> Font.color c.palette.external
             ]
                ++ attrs
            )
        <|
            text <|
                String.fromInt quantity



--|     atomIcon


atomIcon :
    List (Attribute msg)
    -> { color : Palette -> Color, fill : Bool, shape : FeatherIcons.Icon, size : Float }
    -> Element msg
atomIcon attrs { shape, color, size, fill } =
    elementWithContext <|
        \c ->
            el attrs <|
                html <|
                    (shape
                        |> FeatherIcons.withSize size
                        |> FeatherIcons.withStrokeWidth 1
                        |> FeatherIcons.toHtml
                            (Html.Attributes.style "stroke" (colorToCssString (color c.palette))
                                :: (if fill && shape /= FeatherIcons.arrowUp && shape /= FeatherIcons.chevronLeft && shape /= FeatherIcons.chevronRight then
                                        [ Html.Attributes.style "fill" (colorToCssString (changeAlpha 1 (color c.palette))) ]
                                        -- [ Html.Attributes.style "fill" "rgb(255, 111 ,255)" ]

                                    else
                                        []
                                   )
                            )
                    )



-- ███    ███  ██████  ██      ███████  ██████ ██    ██ ██      ███████ ███████
-- ████  ████ ██    ██ ██      ██      ██      ██    ██ ██      ██      ██
-- ██ ████ ██ ██    ██ ██      █████   ██      ██    ██ ██      █████   ███████
-- ██  ██  ██ ██    ██ ██      ██      ██      ██    ██ ██      ██           ██
-- ██      ██  ██████  ███████ ███████  ██████  ██████  ███████ ███████ ███████
--| MOLECULES
--|     molMenuMain


molMenuMain : List (Attribute Msg) -> Model -> Element Msg
molMenuMain attrs model =
    let
        viewSearchInputField =
            row
                [ spacing 10
                , width fill
                , transition "transform 0.2s"
                , scale <|
                    case pageToSearchQuery model.page of
                        Just _ ->
                            1

                        Nothing ->
                            0
                ]
                [ Input.button []
                    { label =
                        atomIcon
                            []
                            { shape = FeatherIcons.x
                            , color = .onExternal
                            , size = 30
                            , fill = False
                            }
                    , onPress = Just <| ToggleSearch
                    }
                , Input.text
                    [ width fill
                    , paddingXY 6 6
                    , centerY
                    , attrWithContext <| \c -> Background.color c.palette.surface
                    , htmlAttribute <| Html.Attributes.id "search-box"
                    , Border.width 0
                    ]
                    { label = Input.labelHidden "Search"
                    , onChange = ChangeSearch
                    , placeholder = Just <| Input.placeholder [] <| text "Search"
                    , text = Maybe.withDefault "" (pageToSearchQuery model.page)
                    }
                ]
    in
    el
        ([ tag "molMenuMain"
         , inFront viewSearchInputField
         ]
            ++ attrs
        )
    <|
        row
            [ spacing 15
            , centerX
            , transition "transform 0.2s"
            , scale <|
                case pageToSearchQuery model.page of
                    Just _ ->
                        0

                    Nothing ->
                        1
            ]
            [ Input.button [ title "Search" ]
                { label =
                    atomIcon []
                        { shape = FeatherIcons.search
                        , color = .onExternal
                        , size = 30
                        , fill = False
                        }
                , onPress = Just <| ToggleSearch
                }
            , Input.button [ title "Change Mode" ]
                { label =
                    let
                        degree =
                            if model.mode == Light then
                                pi

                            else
                                0
                    in
                    atomIcon
                        [ rotate degree
                        , transition "transform 1s"
                        , mouseOver
                            [ scale 1.2
                            , rotate degree
                            ]
                        ]
                        { shape =
                            if model.mode == Light then
                                FeatherIcons.sun

                            else
                                FeatherIcons.moon
                        , color = .onExternal
                        , size = 30
                        , fill = False
                        }
                , onPress = Just <| ToggleMode
                }
            , Input.button [ title "Info" ]
                { label =
                    atomIcon []
                        { shape = FeatherIcons.info
                        , color = .onExternal
                        , size = 30
                        , fill = False
                        }
                , onPress = Just <| ToggleInfo
                }
            , Input.button [ title "View Top" ]
                { label =
                    buttonEffect []
                        { active = model.page == Top Nothing
                        , height = 30
                        , icon1 = FeatherIcons.home
                        , icon2 = FeatherIcons.home
                        , width = 30
                        , color = .onExternal
                        }
                , onPress = Just <| ChangePage (Top Nothing)
                }
            , Input.button [ title "View Favorites" ]
                { label =
                    buttonEffect (attrsWithCounter (List.length model.favorites))
                        { active = model.page == Favorites Nothing
                        , height = 30
                        , icon1 = FeatherIcons.heart
                        , icon2 = FeatherIcons.heart
                        , width = 30
                        , color = .onExternal
                        }
                , onPress = Just <| ChangePage (Favorites Nothing)
                }
            , Input.button [ title "View Cart" ]
                { label =
                    buttonEffect (attrsWithCounter (List.length model.cart))
                        { active = model.page == Cart Nothing
                        , height = 30
                        , icon1 = FeatherIcons.shoppingCart
                        , icon2 = FeatherIcons.shoppingCart
                        , width = 30
                        , color = .onExternal
                        }
                , onPress = Just <| ChangePage (Cart Nothing)
                }
            ]



--|     molMenuSortBy


molMenuSortBy : List (Attribute Msg) -> Sort -> SortDirection -> Element Msg
molMenuSortBy attrs2 sort sortDirection =
    row
        ([ tag "molMenuSortBy", spacing 20, alpha 0.7 ] ++ attrs2)
        [ elementWithContext <| \c -> text <| trSortBy c.language
        , Input.button [ title "Sort Alphabetically" ]
            { label =
                buttonEffect []
                    { active = sort == Alpha
                    , height = 24
                    , width = 24
                    , icon1 = FeatherIcons.bold
                    , icon2 = FeatherIcons.bold
                    , color = .onBackground
                    }
            , onPress = Just <| ChangeSort Alpha
            }
        , Input.button [ title "Sort by Price" ]
            { label =
                buttonEffect []
                    { active = sort == Price
                    , height = 24
                    , width = 24
                    , icon1 = FeatherIcons.tag
                    , icon2 = FeatherIcons.tag
                    , color = .onBackground
                    }
            , onPress = Just <| ChangeSort Price
            }
        , Input.button [ title "Sort by Rating" ]
            { label =
                buttonEffect []
                    { active = sort == Stars
                    , height = 24
                    , width = 24
                    , icon1 = FeatherIcons.star
                    , icon2 = FeatherIcons.star
                    , color = .onBackground
                    }
            , onPress = Just <| ChangeSort Stars
            }
        , Input.button [ title "Sorting sortDirection" ]
            { label =
                buttonEffect []
                    { active = sortDirection == Descending
                    , height = 24
                    , width = 24
                    , icon1 = FeatherIcons.arrowUp
                    , icon2 = FeatherIcons.arrowDown
                    , color = .onBackground
                    }
            , onPress = Just ToggleDirection
            }
        ]



--|     molAddToFavorites


molAddToFavorites : List (Attribute Msg) -> Product -> Bool -> Element Msg
molAddToFavorites attrs product isFavorite =
    Input.button
        ([ tag "molAddToFavorites"
         , title "Add to Favorites"
         , Border.rounded 50
         , attrWithContext <| \c -> Border.color c.palette.primary
         , height <| px 40
         , transition "box-shadow 0.3s, background-color 0.3s"
         , mouseOver
            [ shadowHigh
            , decorationWithContext <| \c -> Background.color c.palette.surface2dp
            ]
         , width <| px 40
         , Border.width 1
         , attrWithContext <| \c -> Background.color c.palette.surface
         ]
            ++ attrs
        )
        { label =
            buttonEffect [ moveDown 1 ]
                { active = isFavorite
                , height = 20
                , width = 20
                , icon1 = FeatherIcons.heart
                , icon2 = FeatherIcons.heart
                , color = .primary
                }
        , onPress = Just <| ToggleFavorite product
        }



--|     molAddToCart


molAddToCart : List (Attribute Msg) -> Product -> Bool -> Element Msg
molAddToCart attrs product isCart =
    Input.button
        ([ tag "molAddToCart"
         , title "Add to Cart"
         , Border.rounded 50
         , attrWithContext <| \c -> Border.color c.palette.primary
         , height <| px 40
         , transition "box-shadow 0.3s, background-color 0.3s"
         , mouseOver
            [ shadowHigh
            , decorationWithContext <| \c -> Background.color c.palette.surface2dp
            ]
         , width <| px 40
         , Border.width 1
         , attrWithContext <| \c -> Background.color c.palette.surface
         ]
            ++ attrs
        )
        { label =
            buttonEffect [ moveLeft 1, moveDown 2 ]
                { active = isCart
                , height = 20
                , width = 20
                , icon1 = FeatherIcons.shoppingCart
                , icon2 = FeatherIcons.shoppingCart
                , color = .primary
                }
        , onPress =
            Just <|
                if isCart then
                    ChangeCart product 0

                else
                    ChangeCart product 1
        }



--|     molAddToCartLarge


molAddToCartLarge : List (Attribute Msg) -> Product -> Int -> Element Msg
molAddToCartLarge attrs product quantity =
    row
        ([ tag "molAddToCartLarge"
         , title "Add to Cart"
         , Border.rounded 50
         , height <| px 40
         , transition "box-shadow 0.3s"
         , mouseOver [ shadowHigh ]
         , attrWithContext <| \c -> Background.color c.palette.primary
         , attrWithContext <| \c -> Font.color <| c.palette.onPrimary
         ]
            ++ attrs
        )
        [ Input.button [ paddingEach { top = 0, right = 10, bottom = 0, left = 10 } ]
            { label =
                buttonEffect []
                    { active = modBy 2 quantity == 0
                    , height = 20
                    , width = 20
                    , icon1 = FeatherIcons.chevronLeft
                    , icon2 = FeatherIcons.chevronLeft
                    , color = .onPrimary
                    }
            , onPress = Just <| ChangeCart product (quantity - 1)
            }
        , el [] <|
            Input.text
                [ width <| px 30
                , Font.center
                , centerY
                , Border.width 0
                , attrWithContext <| \c -> Background.color c.palette.surface2dp
                , attrWithContext <| \c -> Font.color c.palette.onBackground
                , padding 5
                ]
                { label = Input.labelHidden "Quantity"
                , onChange =
                    \string ->
                        (if string == "" then
                            "0"

                         else
                            string
                        )
                            |> String.toInt
                            |> Maybe.withDefault quantity
                            |> ChangeCart product
                , placeholder = Just <| Input.placeholder [] <| text "0"
                , text =
                    if quantity == 0 then
                        ""

                    else
                        String.fromInt quantity
                }
        , Input.button
            [ paddingEach { top = 0, right = 15, bottom = 0, left = 10 } ]
            { label =
                buttonEffect []
                    { active = modBy 2 quantity == 0
                    , height = 20
                    , width = 20
                    , icon1 = FeatherIcons.chevronRight
                    , icon2 = FeatherIcons.chevronRight
                    , color = .onPrimary
                    }
            , onPress = Just <| ChangeCart product (quantity + 1)
            }
        , Input.button
            [ paddingEach { top = 0, right = 15, bottom = 0, left = 0 }
            ]
            { label =
                buttonEffect []
                    { active = modBy 2 quantity == 0
                    , height = 20
                    , width = 20
                    , icon1 = FeatherIcons.shoppingCart
                    , icon2 = FeatherIcons.shoppingCart
                    , color = .onPrimary
                    }
            , onPress = Just <| ChangeCart product (quantity + 1)
            }
        ]



--|     molProductForStrip


molProductForStrip : List (Attribute Msg) -> Product -> Element Msg
molProductForStrip attrs product =
    let
        label =
            atomImageProduct
                ([ tag "molProductForStrip"
                 , alpha 0.9
                 , transition "opacity 0.3s"
                 , mouseOver [ alpha 1 ]
                 , width <| px 200
                 , inFront <|
                    el
                        [ height fill
                        , width fill
                        , style "background-image" "linear-gradient(to bottom, rgba(0, 0, 0, 0), rgba(255, 255, 255, 0.3))"
                        ]
                        none
                 , if product == productInit then
                    noneAttr

                   else
                    inFront <|
                        atomPrice
                            [ Font.color <| rgba 0 0 0 0.6
                            , paddingXY 15 10
                            , alignBottom
                            , Font.alignRight
                            ]
                            20
                            product.price
                 ]
                    ++ attrs
                )
                30
                product
    in
    linkWrapperForProduct product label



--|     molProductForGrid


molProductForGrid : List (Attribute Msg) -> List ProductId -> List ProductId -> Maybe String -> Product -> Element Msg
molProductForGrid attrs cart favorites maybeSearchQuery product =
    let
        mask =
            globalMask (product == productInit)

        label =
            column
                [ spacing 10
                , attrWithContext <| \c -> width (fill |> maximum c.conf.gridMaximum |> minimum c.conf.gridMinimum)
                , shadowLow
                , alpha 0.9
                , if product.tag == "" then
                    noneAttr

                  else
                    inFront <| atomTagNew [ moveRight 15, moveDown 15 ] product.tag
                , transition "box-shadow 0.3s, background-color 0.3s, opacity 0.3s"
                , mouseOver
                    [ shadowHigh
                    , alpha 1
                    , decorationWithContext <| \c -> Background.color c.palette.surface2dp
                    ]
                ]
                [ atomImageProduct [] 20 product
                , column [ padding 20, spacing 20, width fill ]
                    [ column [ spacing 20, height <| px 86, clip, width fill ]
                        [ mask ( ( 25, 75 ), 1, [] ) (atomPrice [] 25 product.price)
                        , mask ( ( 16, 0 ), 2, [ width fill ] )
                            (paragraph
                                [ attrWithContext <| \c -> Font.color c.palette.onBackgroundDim
                                ]
                             <|
                                highlightSearchQuery product.name maybeSearchQuery
                            )
                        ]
                    , mask ( ( 16, 100 ), 1, [] ) (molStars [] product.stars)
                    ]
                ]
    in
    el
        ([ tag "molProductForGrid", width fill ]
            ++ (if product == productInit then
                    []

                else
                    [ inFront <|
                        molAddToFavorites
                            [ alignBottom
                            , alignRight
                            , moveUp 185
                            , moveLeft 10
                            , height <| px 32
                            , width <| px 32
                            , Border.width 0
                            ]
                            product
                            (List.member product.id favorites)
                    , inFront <|
                        molAddToCart
                            [ alignBottom
                            , alignLeft
                            , moveUp 185
                            , moveRight 10
                            , height <| px 32
                            , width <| px 32
                            , Border.width 0
                            ]
                            product
                            (List.member product.id cart)
                    ]
               )
            ++ attrs
        )
        (linkWrapperForProduct product label)



--|     molProductForRow


molProductForRow : List ProductId -> List ProductId -> Maybe String -> Page -> Product -> Element Msg
molProductForRow cart favorites maybeSearchQuery page product =
    let
        quantity =
            quantityInTheCart cart product

        isFavorite =
            List.member product.id favorites

        mask =
            globalMask (product == productInit)

        sectionPriceAndQuantity =
            el [ width fill ] <|
                el [ alignRight, alpha 0.6 ] <|
                    mask ( ( 16, 100 ), 1, [] )
                        (row [ spacing 7 ]
                            [ atomPrice [] 16 product.price
                            , text "x"
                            , text <| String.fromInt quantity
                            , text "="
                            ]
                        )

        sectionPriceXquantity =
            el [ width fill ] <|
                el [ alignRight ] <|
                    mask ( ( 30, 120 ), 1, [] )
                        (atomPrice [] 30 (product.price * quantity))

        sectionPrice =
            el [ width fill ] <|
                el [ alignRight ] <|
                    mask ( ( 30, 120 ), 1, [] ) <|
                        atomPrice [] 30 product.price

        sectionAddToFavorites =
            el [ width fill ] <|
                mask ( ( 40, 40 ), 1, [ centerX ] ) <|
                    molAddToFavorites [ centerX ] product isFavorite

        sectionAddToCart =
            el [ width fill ] <|
                mask ( ( 40, 150 ), 1, [ centerX ] ) <|
                    molAddToCartLarge [ centerX ] product quantity

        sectionNameAndStars =
            column [ width fill, spacing 10 ]
                [ mask ( ( 16, 130 ), 1, [] ) <|
                    paragraph [ attrWithContext <| \c -> Font.color c.palette.onBackgroundDim ] <|
                        highlightSearchQuery product.name maybeSearchQuery
                , mask ( ( 16, 100 ), 1, [] ) (molStars [] product.stars)
                ]
    in
    row
        [ tag "molProductForRow"
        , width fill
        , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
        , attrWithContext <| \c -> Border.color c.palette.mask
        , paddingEach { top = 10, right = 0, bottom = 0, left = 0 }
        ]
        [ atomImageProduct [ width <| px 120, height <| px 120, alignTop ] 2 product
        , wrappedRow [ width fill, paddingEach { top = 0, right = 0, bottom = 0, left = 10 }, spacing 10 ]
            (case page of
                Cart _ ->
                    [ sectionNameAndStars
                    , sectionAddToFavorites
                    , sectionAddToCart
                    , sectionPriceAndQuantity
                    , sectionPriceXquantity
                    ]

                _ ->
                    [ sectionNameAndStars
                    , sectionPrice
                    , sectionAddToFavorites
                    , sectionAddToCart
                    ]
            )
        ]



--|     molProductNotFound


molProductNotFound : Page -> Element Msg
molProductNotFound page =
    column
        [ tag "molProductNotFound"
        , centerX
        , paddingEach { top = 50, right = 0, bottom = 100, left = 0 }
        , spacing 20
        , width (fill |> minimum 150)
        ]
        [ atomIcon [ centerX, alpha 0.2 ]
            { shape = FeatherIcons.search
            , color = .onBackground
            , size = 100
            , fill = False
            }
        , paragraph [ Font.center ]
            [ text <| "No products containing \"" ++ Maybe.withDefault "" (pageToSearchQuery page) ++ "\""
            , text <|
                case page of
                    Cart _ ->
                        " in the Cart"

                    Favorites _ ->
                        " among the Favorites"

                    _ ->
                        ""
            ]
        , Input.button (attrsButton ++ [ centerX ])
            { label =
                row [ spacing 8 ]
                    [ el [] <| text "Remove filter" ]
            , onPress = Just <| ToggleSearch
            }
        ]



--|     molClose


molSecondaryButton : List (Attribute msg) -> FeatherIcons.Icon -> msg -> Element msg
molSecondaryButton attrs shape msg =
    Input.button
        ([ tag "molSecondaryButton"
         , title "Close"
         , Border.rounded 50
         , Border.width 1
         , width <| px 40
         , height <| px 40
         , attrWithContext <| \c -> Background.color c.palette.surface
         , attrWithContext <| \c -> Border.color c.palette.secondary
         , transition "box-shadow 0.2s, background-color 0.2s, transform 0.6s"
         , mouseOver
            [ shadowHigh
            , decorationWithContext <| \c -> Background.color c.palette.surface2dp
            ]
         ]
            ++ attrs
        )
        { label =
            atomIcon
                [ centerX
                , centerY
                , transition "transform 0.4s"
                , mouseOver [ scale 1.2 ]
                ]
                { shape = shape
                , color = .secondary
                , size = 30
                , fill = False
                }
        , onPress = Just <| msg
        }



--|     molStars


molStars : List (Attribute msg) -> Int -> Element msg
molStars attrs rating =
    row ([ tag "molStars", spacing 1 ] ++ attrs)
        (List.repeat (min 5 rating)
            (atomIcon []
                { shape = FeatherIcons.star
                , color = .primary
                , size = 16
                , fill = True
                }
            )
            ++ List.repeat (5 - min 5 rating)
                (atomIcon []
                    { shape = FeatherIcons.star
                    , color = .primary
                    , size = 16
                    , fill = False
                    }
                )
        )



--|     molEmptyList


molEmptyList : Page -> Element Msg
molEmptyList page =
    column
        [ tag "molEmptyList"
        , centerX
        , paddingEach { top = 0, right = 0, bottom = 60, left = 0 }
        , spacing 20
        , width (fill |> minimum 150)
        ]
        ((case page of
            Cart _ ->
                [ atomIcon [ centerX, alpha 0.2 ]
                    { shape = FeatherIcons.shoppingCart
                    , color = .onBackground
                    , size = 100
                    , fill = False
                    }
                , paragraph [ Font.center ] [ elementWithContext <| \c -> text <| trTheCartIsEmpty c.language ]
                ]

            _ ->
                [ atomIcon [ centerX, alpha 0.2 ]
                    { shape = FeatherIcons.heart
                    , color = .onBackground
                    , size = 100
                    , fill = False
                    }
                , paragraph [ Font.center ] [ elementWithContext <| \c -> text <| trTheListOfFavoritesIsEmpty c.language ]
                ]
         )
            ++ [ Input.button (attrsButton ++ [ centerX ])
                    { label =
                        atomIcon []
                            { shape = FeatherIcons.home
                            , color = .onPrimary
                            , size = 20
                            , fill = False
                            }
                    , onPress = Just <| ChangePage (Top Nothing)
                    }
               ]
        )



--  ██████  ██████   ██████   █████  ███    ██ ██ ███████ ███    ███ ███████
-- ██    ██ ██   ██ ██       ██   ██ ████   ██ ██ ██      ████  ████ ██
-- ██    ██ ██████  ██   ███ ███████ ██ ██  ██ ██ ███████ ██ ████ ██ ███████
-- ██    ██ ██   ██ ██    ██ ██   ██ ██  ██ ██ ██      ██ ██  ██  ██      ██
--  ██████  ██   ██  ██████  ██   ██ ██   ████ ██ ███████ ██      ██ ███████
--| ORGANISMS
--|     orgHeader


orgHeader : Page -> Element Msg
orgHeader page =
    column [ tag "orgHeader", centerX, width fill ]
        [ el [ centerX, attrWidth ] <|
            row [ alignRight, spacing 6, padding 5 ] <|
                List.map
                    (\lang ->
                        elementWithContext <|
                            \c ->
                                if lang == c.language then
                                    el
                                        [ paddingXY 8 6
                                        , Font.size 12
                                        , Font.color c.palette.onExternal
                                        , Background.color <| changeAlpha 0.2 c.palette.onExternal
                                        , transition "background-color 0.3s"
                                        ]
                                    <|
                                        text <|
                                            String.toUpper <|
                                                languageToString lang

                                else
                                    Input.button
                                        [ paddingXY 8 6
                                        , Font.size 12
                                        , Font.color c.palette.onExternal
                                        , Background.color <| changeAlpha 0 c.palette.onExternal
                                        , mouseOver [ Background.color (changeAlpha 0.2 c.palette.onExternal) ]
                                        , transition "background-color 0.3s"
                                        ]
                                        { label = text <| String.toUpper <| languageToString lang
                                        , onPress = Just <| ChangeLanguage lang
                                        }
                    )
                    [ EN_US, JA_JP ]
        , elementWithContext <|
            \c ->
                Input.button
                    [ centerX
                    , paddingXY 0 20
                    , moveDown 20
                    , pointer
                    , Font.color c.palette.primary
                    ]
                    { label =
                        el
                            ((height <| px 40)
                                :: (case page of
                                        Cart _ ->
                                            [ inFront <|
                                                atomIcon [ centerX, moveUp 20 ]
                                                    { shape = FeatherIcons.shoppingCart
                                                    , color = .onExternal
                                                    , size = 54
                                                    , fill = False
                                                    }
                                            ]

                                        Favorites _ ->
                                            [ inFront <|
                                                atomIcon [ centerX, moveUp 20 ]
                                                    { shape = FeatherIcons.heart
                                                    , color = .onExternal
                                                    , size = 54
                                                    , fill = False
                                                    }
                                            ]

                                        _ ->
                                            [ inFront <|
                                                atomLogo [ centerX ] { color = c.palette.onExternal, size = 28 }
                                            ]
                                   )
                            )
                            none
                    , onPress = Just <| ChangePage (Top Nothing)
                    }
        ]



--|     orgProductsStrip


orgProductsStrip : HttpRequest -> Element Msg
orgProductsStrip httpRequest =
    let
        products =
            case httpRequest of
                Success ps ->
                    ps

                _ ->
                    List.repeat 15 productInit
    in
    row
        [ tag "orgProductsStrip"
        , attrWidth
        , centerX
        , inFront <|
            el [ transparentToMouse, width <| px 50, alpha 0.8, centerY ] <|
                atomIcon []
                    { shape = FeatherIcons.chevronLeft
                    , color = .onPrimary
                    , size = 50
                    , fill = False
                    }
        , inFront <|
            el [ transparentToMouse, width <| px 50, alpha 0.8, centerY, alignRight ] <|
                atomIcon []
                    { shape = FeatherIcons.chevronRight
                    , color = .onPrimary
                    , size = 50
                    , fill = False
                    }
        ]
        [ row [ scrollbarX, width fill, spacing 5 ] <|
            List.map (molProductForStrip []) products
        ]



--|     orgProductDetails


orgProductDetails : Product -> Int -> Bool -> Element Msg
orgProductDetails product quantity isFavorite =
    let
        minimumWidth =
            250

        mask =
            globalMask (product == productInit)
    in
    el
        attrsContent
    <|
        wrappedRow
            [ tag "orgProductDetails"
            , centerX
            , spacing 20
            , width fill
            ]
            [ atomImageProduct
                [ width (fill |> minimum minimumWidth)
                , alignTop
                , height shrink
                ]
                40
                product
            , column
                [ spacing 50
                , width (fill |> minimum minimumWidth)
                , alignTop
                ]
              <|
                [ column [ paddingEach { top = 60, right = 0, bottom = 0, left = 0 }, Font.center, centerX, spacing 20, width fill ]
                    [ mask ( ( 30, 0 ), 2, [ width fill ] ) <| paragraph [ Font.size 30 ] [ text <| product.name ]
                    , mask ( ( 25, 100 ), 1, [ centerX ] ) <| atomPrice [ Font.size 25, centerX ] 30 product.price
                    , mask ( ( 16, 80 ), 1, [ centerX ] ) <| molStars [ centerX ] product.stars
                    ]
                , row [ spacing 20, centerX ]
                    [ mask ( ( 40, 100 ), 1, [] ) <| molAddToCartLarge [] product quantity
                    , mask ( ( 40, 40 ), 1, [] ) <| molAddToFavorites [] product isFavorite
                    ]
                , mask ( ( 16, 10 ), 7, [ width fill ] ) <| paragraph [] [ text <| product.description ]
                ]
            ]



--|     orgProductsGrid


orgProductsGrid : Model -> Int -> Element Msg
orgProductsGrid model maxProduct =
    let
        products =
            case model.httpRequest of
                Success products_ ->
                    products_
                        |> filterSearch (pageToSearchQuery model.page)

                _ ->
                    List.repeat maxProduct productInit
    in
    column
        (tag "orgProductsGrid" :: attrsContent)
        [ molMenuSortBy [ alignRight, paddingXY 0 10 ] model.sort model.sortDirection
        , if products == [] then
            molProductNotFound model.page

          else
            wrappedRow
                [ spacing 10 ]
            <|
                (products
                    |> sortBy model.sort model.sortDirection
                    |> List.map
                        (molProductForGrid [] model.cart model.favorites (pageToSearchQuery model.page))
                )
        ]



--|     orgProductsRows


orgProductsRows : Model -> Element Msg
orgProductsRows model =
    let
        total =
            case ( totalPrice model.cart model.httpRequest, model.page ) of
                ( Just price, Cart _ ) ->
                    el
                        [ Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                        , attrWithContext <| \c -> Border.color c.palette.mask
                        , width fill
                        ]
                    <|
                        wrappedRow
                            [ spacing 20
                            , alignRight
                            , paddingXY 0 30
                            ]
                            [ el [ Font.size 25, alignBottom ] <| elementWithContext <| \c -> text <| trTotal c.language
                            , atomPrice [] 30 price
                            ]

                _ ->
                    none

        listProductIds =
            (case model.page of
                Cart _ ->
                    model.cart

                _ ->
                    model.favorites
            )
                ++ model.temp

        products =
            listIdsToListProducts model listProductIds (pageToSearchQuery model.page)

        content =
            if List.length listProductIds == 0 then
                [ molEmptyList model.page ]

            else if List.length products == 0 then
                [ molProductNotFound model.page ]

            else
                products
                    |> List.map (molProductForRow model.cart model.favorites (pageToSearchQuery model.page) model.page)
    in
    column
        (tag "orgProductsRows" :: attrsContent)
        [ molMenuSortBy [ alignRight, paddingXY 0 10 ] model.sort model.sortDirection
        , column [ spacing 10, width fill ] content
        , total
        ]



--|     orgFooter


orgFooter : Element Msg
orgFooter =
    let
        attrs =
            attrsButton
                ++ [ attrWithContext <| \c -> Background.color (changeAlpha 0 c.palette.onExternal)
                   , transition "background-color 0.2s"
                   , mouseOver
                        [ decorationWithContext <| \c -> Background.color (changeAlpha 0.2 c.palette.onExternal)
                        ]
                   , attrWithContext <| \c -> Font.color c.palette.onExternal
                   , attrWithContext <| \c -> Border.color c.palette.onExternal
                   , Border.width 1
                   ]
    in
    column [ tag "orgFooter", attrWidth, centerX, Font.center, paddingXY 30 50, spacing 40 ]
        [ column [ padding 10, centerX, spacing 10, Font.size 13 ]
            [ paragraph [ attrWithContext <| \c -> Font.color c.palette.onExternal ] [ text "Danger Zone" ]
            , wrappedRow
                [ spacing 10 ]
                [ Input.button []
                    { label = el attrs <| text "Empty Cart", onPress = Just EmptyCart }
                , Input.button []
                    { label = el attrs <| text "Empty Favorites", onPress = Just EmptyFavorite }
                ]
            ]
        ]



--|     orgError


orgError : msg -> List (Element msg) -> Element msg
orgError msg content =
    column
        [ tag "orgError"
        , alignBottom
        , width fill
        , attrWithContext <| \c -> Background.color c.palette.secondary
        , attrWithContext <| \c -> Font.color c.palette.onSecondary
        , inFront <| molSecondaryButton [ moveUp 20, moveLeft 40, alignRight ] FeatherIcons.x msg
        , paddingEach { top = 25, right = 10, bottom = 10, left = 10 }
        , spacing 15
        , Border.shadow { offset = ( 0, 0 ), size = 2, blur = 10, color = rgba 0 0 0 0.1 }
        ]
        content



-- ██████   █████  ██      ███████ ████████ ████████ ███████
-- ██   ██ ██   ██ ██      ██         ██       ██    ██
-- ██████  ███████ ██      █████      ██       ██    █████
-- ██      ██   ██ ██      ██         ██       ██    ██
-- ██      ██   ██ ███████ ███████    ██       ██    ███████
--| PALETTE


type alias Palette =
    { primary : Color
    , secondary : Color
    , external : Color
    , background : Color
    , surface : Color
    , surface2dp : Color
    , onExternal : Color
    , onPrimary : Color
    , onSecondary : Color
    , onBackground : Color
    , onBackgroundDim : Color
    , separator : Color
    , mask : Color
    , error : Color
    }



--|     paletteLight


paletteLight : Palette
paletteLight =
    { primary = rgb255 17 147 215
    , secondary = rgb255 80 181 237
    , external = rgb255 17 147 215
    , background = rgb255 245 245 245
    , surface = rgb255 250 250 250
    , surface2dp = rgb255 255 255 255
    , onExternal = rgb255 255 255 255
    , onPrimary = rgb255 255 255 255
    , onSecondary = rgb255 255 255 255
    , onBackground = rgb255 0 0 0
    , onBackgroundDim = rgb255 100 100 100
    , mask = rgb255 235 235 235
    , separator = rgb255 200 200 200
    , error = rgb255 200 0 0
    }



--|     paletteDark


paletteDark : Palette
paletteDark =
    { primary = rgb255 255 255 255
    , secondary = rgb255 17 147 215
    , external = rgb255 6 103 154
    , background = rgb255 20 20 20
    , surface = rgb255 50 50 50
    , surface2dp = rgb255 60 60 60
    , onExternal = rgb255 255 255 255
    , onPrimary = rgb255 0 0 0
    , onSecondary = rgb255 255 255 255
    , onBackground = rgb255 230 230 230
    , onBackgroundDim = rgb255 180 180 180
    , mask = rgb255 100 100 100
    , separator = rgb255 110 110 110
    , error = rgb255 255 100 100
    }



-- ██    ██ ██ ███████ ██     ██ ███████
-- ██    ██ ██ ██      ██     ██ ██
-- ██    ██ ██ █████   ██  █  ██ ███████
--  ██  ██  ██ ██      ██ ███ ██      ██
--   ████   ██ ███████  ███ ███  ███████
--| VIEWS
--|     view


view : Configuration -> Model -> Html.Html Msg
view conf model =
    layoutWith (contextBuilder conf model.mode model.language)
        { options = options }
        (mainAttrs conf model)
        (if model.pwd == correctPwd then
            viewPage model

         else
            viewPwd model.pwd
        )



--|     viewCartAndFavoritesOverview


viewCartAndFavoritesOverview : Model -> Element Msg
viewCartAndFavoritesOverview model =
    let
        viewSideColumProducts listProductIds =
            let
                products =
                    listIdsToListProducts model listProductIds Nothing
            in
            List.map
                (\product ->
                    let
                        quantity =
                            quantityInTheCart listProductIds product
                    in
                    linkWrapperForProduct product
                        (atomImageProduct
                            [ width <| px 80
                            , if quantity > 1 then
                                inFront <| atomTagNumber [ alignRight, moveUp 3, moveRight 3 ] quantity

                              else
                                noneAttr
                            ]
                            5
                            product
                        )
                )
                products
    in
    row
        [ height fill
        , spacing 10
        , padding 10
        ]
        ([]
            ++ (if model.cart == [] then
                    []

                else
                    Input.button []
                        { onPress = Just <| ChangePage <| Cart Nothing
                        , label =
                            column [ spacing 10 ]
                                [ el [ width (fill |> minimum 80), moveDown 5 ] <|
                                    atomIcon (attrsWithCounter (List.length model.cart) ++ [ centerX ])
                                        { shape = FeatherIcons.shoppingCart
                                        , color = .onSecondary
                                        , size = 40
                                        , fill = False
                                        }
                                , case totalPrice model.cart model.httpRequest of
                                    Just price ->
                                        atomPrice [ centerX, attrWithContext <| \c -> Font.color c.palette.onSecondary ] 16 price

                                    Nothing ->
                                        none
                                ]
                        }
                        :: viewSideColumProducts model.cart
               )
            ++ (if model.favorites == [] then
                    []

                else
                    Input.button []
                        { onPress = Just <| ChangePage <| Favorites Nothing
                        , label =
                            el [ width (fill |> minimum 80), moveDown 5 ] <|
                                atomIcon
                                    (attrsWithCounter (List.length model.favorites) ++ [ centerX ])
                                    { shape = FeatherIcons.heart
                                    , color = .onSecondary
                                    , size = 40
                                    , fill = False
                                    }
                        }
                        :: viewSideColumProducts model.favorites
               )
        )



--|     viewPage


viewPage : Model -> Element Msg
viewPage model =
    column
        [ width fill
        , spacing 30
        , alignTop
        , if withCartFavoritesOverview model && not model.hideOverview then
            paddingEach { top = 0, right = 0, bottom = 100, left = 0 }

          else
            noneAttr
        ]
        [ orgHeader model.page
        , molMenuMain [ centerX, moveLeft 4 ] model
        , case model.page of
            Top _ ->
                orgProductsGrid model 15

            Favorites _ ->
                orgProductsRows model

            Cart _ ->
                orgProductsRows model

            Details productId ->
                case productIdToProduct model.httpRequest productId of
                    Just product ->
                        model.favorites
                            |> List.member product.id
                            |> orgProductDetails product (quantityInTheCart model.cart product)

                    Nothing ->
                        none

            NotFound err ->
                paragraph [] [ text err ]
        , orgProductsStrip model.httpRequest
        , orgFooter
        , css
        , cssDebugging
        ]



-- ██    ██ ██ ███████ ██     ██     ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██    ██ ██ ██      ██     ██     ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ██    ██ ██ █████   ██  █  ██     ███████ █████   ██      ██████  █████   ██████  ███████
--  ██  ██  ██ ██      ██ ███ ██     ██   ██ ██      ██      ██      ██      ██   ██      ██
--   ████   ██ ███████  ███ ███      ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
--| VIEW HELPERS


inFrontErrors : String -> Element Msg
inFrontErrors error =
    orgError RemoveHttpError <|
        [ paragraph [] [ text "Problem with the API reponse, try reloading the page" ]
        , el [ scrollbarX, width fill ] <|
            el
                []
            <|
                html <|
                    Html.pre [] [ Html.text error ]
        ]


inFrontWarnings : List String -> Element Msg
inFrontWarnings warnings =
    orgError EmptyWarnings <|
        List.map (\string -> paragraph [ Font.center, style "word-break" "break-all" ] [ text string ]) warnings


inFrontCartAndFavoritesOverview : Model -> Element Msg
inFrontCartAndFavoritesOverview model =
    el
        [ alignBottom
        , attrWithContext <| \c -> Background.color c.palette.secondary
        , attrWidth
        , centerX
        , transition "transform 0.4s"
        , if model.hideOverview then
            moveDown 100

          else
            noneAttr
        , inFront <|
            molSecondaryButton
                [ moveLeft 40
                , alignRight
                , if model.hideOverview then
                    moveUp 60

                  else
                    moveUp 20
                ]
                (if model.hideOverview then
                    FeatherIcons.arrowUp

                 else
                    FeatherIcons.arrowDown
                )
                ToggleOverview
        ]
    <|
        el [ width fill, scrollbarX ] <|
            viewCartAndFavoritesOverview model


inFrontInfoModal : Element Msg
inFrontInfoModal =
    el
        [ width fill
        , height fill
        , inFront <|
            el
                [ width fill
                , height fill
                , Background.color <| rgba 0 0 0 0.3
                , Events.onClick ToggleInfo
                ]
                none
        , inFront <|
            el
                [ centerX
                , centerY
                , padding 20
                , inFront <|
                    molSecondaryButton
                        [ moveLeft 40
                        , alignRight
                        ]
                        FeatherIcons.x
                        ToggleInfo
                ]
            <|
                column
                    [ attrWithContext <| \c -> Background.color c.palette.external
                    , attrWithContext <| \c -> Font.color c.palette.onExternal
                    , shadowHigh
                    , Border.rounded 10
                    , padding 30
                    , spacing 20
                    , height <| px 350
                    , width (fill |> maximum 400)
                    , scrollbarY
                    ]
                    [ elementWithContext <| \c -> atomLogo [ centerX ] { size = 28, color = c.palette.onSecondary }
                    , paragraph
                        [ Font.center ]
                        [ text "🍇🍉🍏🍊🍋🍌🍍🍎🍐🍑🍒🍈🍓🥝"
                        ]
                    , paragraph [ Font.center ]
                        [ text "A simple eCommerce demo application in 2,500 lines of "
                        , newTabLink [ Font.underline ] { url = "https://elm-lang.org/", label = text "Elm" }
                        , text " code, all shrank into 40KB of JavaScript, including styling and icons."
                        ]
                    , paragraph [ Font.size 20 ] [ text "Characteristics" ]
                    , column [ spacing 4 ]
                        [ paragraph [] [ text "• Responsive" ]
                        , paragraph [] [ text "• Cart and Favorites management" ]
                        , paragraph [] [ text "• Persistent (local storage)" ]
                        , paragraph [] [ text "• Synced across tabs" ]
                        , paragraph [] [ text "• Light / Dark mode" ]
                        , paragraph [] [ text "• Bilingual" ]
                        , paragraph [] [ text "• Searchable/Sortable results" ]
                        , paragraph [] [ text "• Dashboard (+1,100 lines of Elm)" ]
                        , paragraph [] [ text "• Atomic design" ]
                        , paragraph [] [ text "• Fault tolerant" ]
                        ]
                    , paragraph [ Font.size 20 ]
                        [ text "It is made of"
                        ]
                    , html <|
                        Html.pre []
                            [ Html.text """2,500 lines of Elm
    0 lines of CSS
   25 lines of JavaScript
   15 lines of HTML"""
                            ]
                    , paragraph [ Font.size 20 ] [ text "Links" ]
                    , elementWithContext <|
                        \c ->
                            column [ spacing 4 ] <|
                                List.map
                                    (\( message, url ) ->
                                        paragraph [] [ text "• ", newTabLink [ Font.underline ] { label = text message, url = url } ]
                                    )
                                    c.conf.qrCodeContent
                    ]
        ]
        none


linkWrapperForProduct : Product -> Element Msg -> Element Msg
linkWrapperForProduct product label =
    if product == productInit then
        label

    else
        atomLinkInternal []
            { page = Details product.id
            , label = label
            }


palette : Mode -> { b | paletteDark : a, paletteLight : a } -> a
palette mode conf =
    case mode of
        Light ->
            conf.paletteLight

        Dark ->
            conf.paletteDark


fileName : ProductId -> String -> String
fileName productId mediaLocation =
    mediaLocation
        ++ productIdToString productId
        ++ (if productIdToString productId == "spinner" then
                ".gif"

            else
                ".png"
           )


options : List Option
options =
    [ focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing } ]


withCartFavoritesOverview : { c | cart : List a, favorites : List b } -> Bool
withCartFavoritesOverview model =
    if model.cart == [] && model.favorites == [] then
        False

    else
        True


colorToCssString : Color -> String
colorToCssString color =
    -- Copied from https://github.com/avh4/elm-color/blob/1.0.0/src/Color.elm#L555
    let
        { red, green, blue, alpha } =
            toRgb color

        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    String.concat
        [ "rgba("
        , String.fromFloat (pct red)
        , "%,"
        , String.fromFloat (pct green)
        , "%,"
        , String.fromFloat (pct blue)
        , "%,"
        , String.fromFloat (roundTo alpha)
        , ")"
        ]


changeAlpha : Float -> Color -> Color
changeAlpha alpha color =
    let
        { red, green, blue } =
            toRgb color
    in
    rgba red green blue alpha


totalPrice : List ProductId -> HttpRequest -> Maybe Int
totalPrice cart httpRequest =
    if cart == [] then
        Just 0

    else
        case httpRequest of
            Success _ ->
                cart
                    |> List.map (productIdToProduct httpRequest)
                    |> removeNothings
                    |> List.foldl (\product acc -> product.price + acc) 0
                    |> Just

            _ ->
                -- There are items in the cart but we don't know the price
                -- yet, so we cannot assume any number here.
                Nothing


listIdsToListProducts : Model -> List ProductId -> Maybe String -> List Product
listIdsToListProducts model listProductIds searchQuery =
    case model.httpRequest of
        Success _ ->
            listProductIds
                |> List.map productIdToString
                |> Set.fromList
                |> Set.toList
                |> List.map stringToProductId
                |> List.map (productIdToProduct model.httpRequest)
                |> removeNothings
                |> filterSearch searchQuery
                |> sortBy model.sort model.sortDirection

        _ ->
            List.repeat
                (listProductIds
                    |> List.map productIdToString
                    |> Set.fromList
                    |> Set.size
                )
                productInit


css : Element msg
css =
    -- This CSS is to remove the blue highlight when a button is pressed,
    -- in Chrome for Android and to fix a regression in elm-ui 1.1.8
    html <| Html.node "style" [] [ Html.text "div[role=button] { -webkit-tap-highlight-color: transparent} .s.c > .s {flex-basis: auto}" ]


globalMask : Bool -> ( ( Int, Int ), Int, List (Attribute msg) ) -> Element msg -> Element msg
globalMask condition ( ( y, x ), qty, attrs ) content =
    if condition then
        column ([ spacing 8, width <| px x ] ++ attrs) <|
            List.map
                (\_ ->
                    el
                        [ attrWithContext <| \c -> Background.color c.palette.mask
                        , width fill
                        , height <| px y
                        ]
                    <|
                        none
                )
                (List.repeat qty ())

    else
        content


highlightSearchQuery : String -> Maybe String -> List (Element msg)
highlightSearchQuery string maybeSearchQuery =
    --
    -- This code should be written using elm/parser
    -- instead of elm/regex
    --
    case maybeSearchQuery of
        Nothing ->
            [ text string ]

        Just searchQuery ->
            let
                regex : Regex.Regex
                regex =
                    "(.*?)("
                        ++ String.trim searchQuery
                        ++ ")(.*)"
                        |> Regex.fromStringWith { caseInsensitive = True, multiline = True }
                        |> Maybe.withDefault Regex.never
            in
            string
                |> Regex.find regex
                |> List.head
                |> Maybe.map .submatches
                |> Maybe.map (List.map (Maybe.withDefault ""))
                |> Maybe.map
                    (\list ->
                        case list of
                            before :: middle :: after :: _ ->
                                [ text before
                                , el [ attrWithContext <| \c -> Font.color c.palette.primary, Font.bold ] <| text middle
                                , text after
                                ]

                            _ ->
                                [ text string ]
                    )
                |> Maybe.withDefault [ text string ]


buttonEffect :
    List (Attribute msg)
    ->
        { active : Bool
        , height : Int
        , icon1 : FeatherIcons.Icon
        , icon2 : FeatherIcons.Icon
        , width : Float
        , color : Palette -> Color
        }
    -> Element msg
buttonEffect attrs args =
    el
        ([ width <| px <| round args.width
         , height <| px args.height
         , centerX
         , centerY
         , transition "transform 0.2s"
         , mouseOver [ scale 1.2 ]
         , inFront <|
            atomIcon
                [ transition "transform 0.2s"
                , scale <|
                    if args.active then
                        1

                    else
                        0
                ]
                { shape = args.icon1
                , color = args.color
                , size = args.width
                , fill = args.active

                -- , fill = False
                }
         , inFront <|
            atomIcon
                [ transition "transform 0.2s"
                , scale <|
                    if args.active then
                        0

                    else
                        1
                ]
                { shape = args.icon2
                , color = args.color
                , size = args.width
                , fill = args.active

                -- , fill = False
                }
         ]
            ++ attrs
        )
    <|
        none


viewPwd : String -> Element Msg
viewPwd pwd =
    Input.text [ centerX, centerY, width (fill |> maximum 200) ]
        { label = Input.labelLeft [ Font.size 28, moveDown 4 ] <| text "🔒"
        , onChange = ChangePwd
        , placeholder = Nothing
        , text = pwd
        }



-- ██    ██ ██ ███████ ██     ██      █████  ████████ ████████ ██████  ██ ██████  ██    ██ ████████ ███████ ███████
-- ██    ██ ██ ██      ██     ██     ██   ██    ██       ██    ██   ██ ██ ██   ██ ██    ██    ██    ██      ██
-- ██    ██ ██ █████   ██  █  ██     ███████    ██       ██    ██████  ██ ██████  ██    ██    ██    █████   ███████
--  ██  ██  ██ ██      ██ ███ ██     ██   ██    ██       ██    ██   ██ ██ ██   ██ ██    ██    ██    ██           ██
--   ████   ██ ███████  ███ ███      ██   ██    ██       ██    ██   ██ ██ ██████   ██████     ██    ███████ ███████
--| VIEW ATTRIBUTES
--|     mainAttrs


mainAttrs : Configuration -> Model -> List (Attribute Msg)
mainAttrs conf model =
    let
        c =
            contextBuilder conf model.mode model.language
    in
    [ tag "mainAttrs"
    , Font.size 16
    , Font.color c.palette.onBackground
    , Font.family [ Font.typeface "IBM Plex Sans", Font.sansSerif ]
    , Font.light
    , transition "background-color 2s"
    , Background.color c.palette.external

    -- In front stuff
    , if model.infoClosed then
        noneAttr

      else
        inFront <| inFrontInfoModal
    , if withCartFavoritesOverview model then
        inFront <| inFrontCartAndFavoritesOverview model

      else
        noneAttr
    , if model.warnings == [] then
        noneAttr

      else
        inFront <| inFrontWarnings model.warnings
    , case model.httpRequest of
        Failure error ->
            inFront <| inFrontErrors error

        _ ->
            noneAttr
    ]


style : String -> String -> Attribute msg
style string1 string2 =
    htmlAttribute <| Html.Attributes.style string1 string2


transition : String -> Attribute msg
transition string =
    style "transition" string


lineHeight : Attribute msg
lineHeight =
    style "line-height" "20px"


transparentToMouse : Attribute msg
transparentToMouse =
    style "pointer-events" "none"


title : String -> Attribute msg
title string =
    htmlAttribute <| Html.Attributes.title string


shadowLow : Attr decorative msg
shadowLow =
    Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgba 0 0 0 0 }


shadowHigh : Attr decorative msg
shadowHigh =
    Border.shadow { offset = ( 0, 2 ), size = 0, blur = 10, color = rgba 0 0 0 0.2 }


attrWidth : Attribute msg
attrWidth =
    attrWithContext <| \c -> width (fill |> maximum c.conf.maxWidth)


noneAttr : Attribute msg
noneAttr =
    htmlAttribute <| Html.Attributes.style "" ""


preventDefault : msg -> Attribute msg
preventDefault msg =
    -- From https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md
    htmlAttribute <|
        Html.Events.preventDefaultOn
            "click"
            (Json.Decode.succeed ( msg, True ))


attrsButton : List (Attribute msg)
attrsButton =
    [ tag "attrsButton"
    , lineHeight
    , Border.rounded 5
    , attrWithContext <| \c -> Background.color c.palette.primary
    , attrWithContext <| \c -> Font.color c.palette.onPrimary
    , paddingXY 20 12
    ]


attrsContent : List (Attribute msg)
attrsContent =
    [ tag "attrsContent"
    , padding 10
    , spacing 10
    , centerX
    , attrWidth
    , transition "background-color 1s"
    , attrWithContext <| \c -> Background.color c.palette.surface
    ]


attrsWithCounter : Int -> List (Attribute msg)
attrsWithCounter counter =
    [ tag "attrsWithCounter", onRight <| atomTagNumber [ moveUp 10, moveLeft 10 ] counter ]



--  ██████  ██████  ███    ██ ████████ ███████ ██   ██ ████████
-- ██      ██    ██ ████   ██    ██    ██       ██ ██     ██
-- ██      ██    ██ ██ ██  ██    ██    █████     ███      ██
-- ██      ██    ██ ██  ██ ██    ██    ██       ██ ██     ██
--  ██████  ██████  ██   ████    ██    ███████ ██   ██    ██
--| CONTEXT
--
-- We use miniBill/elm-ui-with-context, an extension of mdgriffith/elm-ui
-- to facilitate passing some value where in needed down the tree of views.
-- For more info, see:
-- https://package.elm-lang.org/packages/miniBill/elm-ui-with-context/latest/


type alias Element msg =
    Element.WithContext.Element Context msg


type alias Attribute msg =
    Element.WithContext.Attribute Context msg


type alias Attr decorative msg =
    Element.WithContext.Attr Context decorative msg


elementWithContext : (Context -> Element msg) -> Element msg
elementWithContext =
    with identity


attrWithContext : (Context -> Attribute msg) -> Attribute msg
attrWithContext =
    withAttribute identity


decorationWithContext : (msg -> Element.WithContext.Decoration msg) -> Element.WithContext.Decoration msg
decorationWithContext =
    withDecoration identity


type alias Context =
    -- Context contain data that is needed to draw the views but it doesn't
    -- change much. It include the entire configuration that doesn't change
    -- at all during the life of the application.
    { conf : Configuration
    , palette : Palette
    , language : Language
    }


contextBuilder : Configuration -> Mode -> Language -> Context
contextBuilder conf mode langauge =
    { conf = conf
    , palette = palette mode conf
    , language = langauge
    }



--| DEBUGGING


cssDebugging : Element msg
cssDebugging =
    elementWithContext <|
        \c ->
            if c.conf.debugging then
                -- https://css-tricks.com/a-complete-guide-to-data-attributes/
                html <| Html.node "style" [] [ Html.text """[data-tag]:hover:after { opacity: 1; } [data-tag]::after { transition: opacity 0.2s;  opacity: 0.2; content: attr(data-tag); background-color: #ff0; z-index: 100; color: #660; border: 1px solid #660; border-radius: 10px 0 0 0; position: absolute; bottom: 0; right: 0; font-size: 13px; padding: 2px 2px 2px 4px; } [data-tag^="atom"]::after { background-color: #faf; } [data-tag^="org"]::after { background-color: #0f0; } [data-tag^="mol"]::after { background-color: #aaf; }""" ]

            else
                none


tag : String -> Attribute msg
tag string =
    attrWithContext <|
        \c ->
            if c.conf.debugging then
                htmlAttribute <| Html.Attributes.attribute "data-tag" string

            else
                htmlAttribute <| Html.Attributes.style "" ""



--| LOCAL STORAGE


changeLocalStorage : String -> Model -> Model
changeLocalStorage localStorageAsString model =
    case stringToLocalStorage localStorageAsString of
        Ok localStorage ->
            localStorageToModel localStorage model

        Err _ ->
            model


localStorageToModel : LocalStorage -> Model -> Model
localStorageToModel localStorage model =
    { model
        | cart = List.map stringToProductId localStorage.cart
        , favorites = List.map stringToProductId localStorage.favorites
        , mode = localStorage.mode
        , language = localStorage.language
        , pwd = localStorage.pwd
        , infoClosed = localStorage.infoClosed
    }


modelToLocalStorage : Model -> LocalStorage
modelToLocalStorage model =
    { cart = List.map productIdToString model.cart
    , favorites = List.map productIdToString model.favorites
    , mode = model.mode
    , language = model.language
    , pwd = model.pwd
    , infoClosed = model.infoClosed
    }



--  ██████  ██████  ██████  ███████  ██████ ███████
-- ██      ██    ██ ██   ██ ██      ██      ██
-- ██      ██    ██ ██   ██ █████   ██      ███████
-- ██      ██    ██ ██   ██ ██      ██           ██
--  ██████  ██████  ██████  ███████  ██████ ███████
--| CODECS
--
-- We use the library miniBill/elm-codec instead using directly elm/json
-- conders and decoders because with elm-codec we only need to write one
-- thing.
-- For more info, see:
-- https://package.elm-lang.org/packages/miniBill/elm-codec/latest/


productIdToString : ProductId -> String
productIdToString (ProductId string) =
    string


stringToProductId : String -> ProductId
stringToProductId string =
    ProductId string


codecProduct : Codec.Codec Product
codecProduct =
    Codec.object Product
        |> Codec.field "id" .id (Codec.map stringToProductId productIdToString Codec.string)
        |> Codec.field "name" .name Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.field "stars" .stars Codec.int
        |> Codec.field "price" .price Codec.int
        |> Codec.field "tag" .tag Codec.string
        |> Codec.field "bg" .bg Codec.int
        |> Codec.buildObject


decoderListProducts : Json.Decode.Decoder (List Product)
decoderListProducts =
    Codec.decoder (Codec.list codecProduct)


stringToLocalStorage : String -> Result Codec.Error LocalStorage
stringToLocalStorage string =
    Codec.decodeString codecModel string


localStorageToString : LocalStorage -> String
localStorageToString model =
    Codec.encodeToString 4 codecModel model


codecModel : Codec.Codec LocalStorage
codecModel =
    Codec.object LocalStorage
        |> Codec.field "cart" .cart (Codec.list Codec.string)
        |> Codec.field "favorites" .favorites (Codec.list Codec.string)
        |> Codec.field "mode" .mode codecMode
        |> Codec.field "language" .language codecLanguage
        |> Codec.field "pwd" .pwd Codec.string
        |> Codec.field "infoClosed" .infoClosed Codec.bool
        |> Codec.buildObject


codecMode : Codec.Codec Mode
codecMode =
    Codec.custom
        (\light dark value ->
            case value of
                Light ->
                    light

                Dark ->
                    dark
        )
        |> Codec.variant0 "Light" Light
        |> Codec.variant0 "Dark" Dark
        |> Codec.buildCustom


codecLanguage : Codec.Codec Language
codecLanguage =
    Codec.custom
        (\enUS jaJP value ->
            case value of
                EN_US ->
                    enUS

                JA_JP ->
                    jaJP
        )
        |> Codec.variant0 "en_US" EN_US
        |> Codec.variant0 "ja_JP" JA_JP
        |> Codec.buildCustom



-- ████████ ██████   █████  ███    ██ ███████ ██       █████  ████████ ██  ██████  ███    ██ ███████
--    ██    ██   ██ ██   ██ ████   ██ ██      ██      ██   ██    ██    ██ ██    ██ ████   ██ ██
--    ██    ██████  ███████ ██ ██  ██ ███████ ██      ███████    ██    ██ ██    ██ ██ ██  ██ ███████
--    ██    ██   ██ ██   ██ ██  ██ ██      ██ ██      ██   ██    ██    ██ ██    ██ ██  ██ ██      ██
--    ██    ██   ██ ██   ██ ██   ████ ███████ ███████ ██   ██    ██    ██  ██████  ██   ████ ███████
--| TRANSLATIONS
--
-- Translations are kept in separated functions so that, in case some of them
-- became unused, it will not be included in the build, automatically.


trSortBy : Language -> String
trSortBy language =
    case language of
        EN_US ->
            "Sort by"

        JA_JP ->
            "注文者"


trTotal : Language -> String
trTotal language =
    case language of
        EN_US ->
            "Total"

        JA_JP ->
            "合計"


trTheCartIsEmpty : Language -> String
trTheCartIsEmpty language =
    case language of
        EN_US ->
            "The cart is empty"

        JA_JP ->
            "カートは空です"


trTheListOfFavoritesIsEmpty : Language -> String
trTheListOfFavoritesIsEmpty language =
    case language of
        EN_US ->
            "The list of favorites products is empty"

        JA_JP ->
            "お気に入りの商品のリストは空です"



-- ██████  ██████   ██████  ██████  ██    ██  ██████ ████████ ███████     ██████   █████  ████████  █████
-- ██   ██ ██   ██ ██    ██ ██   ██ ██    ██ ██         ██    ██          ██   ██ ██   ██    ██    ██   ██
-- ██████  ██████  ██    ██ ██   ██ ██    ██ ██         ██    ███████     ██   ██ ███████    ██    ███████
-- ██      ██   ██ ██    ██ ██   ██ ██    ██ ██         ██         ██     ██   ██ ██   ██    ██    ██   ██
-- ██      ██   ██  ██████  ██████   ██████   ██████    ██    ███████     ██████  ██   ██    ██    ██   ██
--| PRODUCTS DATA
--
-- This code simulates an HTTP response


productsDataToString : List Product -> String
productsDataToString data =
    Codec.encodeToString 4 (Codec.list codecProduct) data


tr :
    Language
    ->
        { drink_fruit_flavor_detox_water : String
        , energy_lemon_denchi_battery : String
        , food_fruit_sandwich : String
        , fruit_dragonfruit : String
        , fruit_jelly : String
        , fruit_suika_red : String
        , fruitice_kiwi : String
        , fruitice_orange : String
        , fruitice_strawberry : String
        , fruits_basket : String
        , kandume1_pineapple : String
        , kandume6_mix : String
        , lorem : String
        , sweets_daifuku_ichigo : String
        , sweets_fruit_mitsumame : String
        , sweets_fruit_tarte : String
        }
tr language =
    case language of
        EN_US ->
            { lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In ac nunc in mi mollis interdum ac et velit. In id elementum turpis. Fusce ornare tortor at magna viverra iaculis. Integer eget purus gravida, ultricies quam nec, dictum dolor. Morbi sit amet leo ac leo laoreet lobortis ornare ac erat. Phasellus gravida a nibh in tempus. Aenean dictum augue id urna egestas, eget tristique ipsum lobortis. Nunc tempor dictum quam ut pharetra. Sed tincidunt nisi mi, id porta orci facilisis eget. Sed et ultrices tellus. Morbi ac diam condimentum, accumsan lectus sed, scelerisque massa. In maximus tortor nec pharetra posuere. Pellentesque eget sapien urna. Vestibulum pulvinar nisi ultrices enim volutpat, a placerat tortor laoreet."
            , fruit_dragonfruit = "Dragon Fruit (Pitaya)"
            , drink_fruit_flavor_detox_water = "Fruit Drink, with Lemons, Kiwi Fruits, Oranges, Blueberry and Raspberry"
            , kandume6_mix = "Fruit Can, with Pineapple, Peach, Cherries and Mandarin"
            , sweets_fruit_tarte = "Fruit Tart with Kiwi Fruits, Peach, Banana, Strawberry and Blackberry"
            , kandume1_pineapple = "Pineapple Can"
            , sweets_daifuku_ichigo = "Sweets Daifuku Strawberry"
            , sweets_fruit_mitsumame = "Sweets Fruit Mitsumame with Peach, Cherries, Banana and Mandarin"
            , energy_lemon_denchi_battery = "Energy Lemon Battery"
            , fruit_jelly = "Fruit Jelly with Mandarin, Kiwi Fruits and Peach"
            , fruitice_strawberry = "Fruit Ice Strawberry"
            , fruitice_orange = "Fruit Ice Orange"
            , fruits_basket = "Fruit Basket"
            , food_fruit_sandwich = "Fruit Sandwich with Strawberry, Kiwi Fruits and Banana"
            , fruitice_kiwi = "Fruit Ice Kiwi Fruits"
            , fruit_suika_red = "Watermelon"
            }

        JA_JP ->
            { lorem = "果ワムヱホ保問へち庁済ケチヲ際約ノキネリ差定みぽかぱ客心テ救掛8試の杯定スフこを積者よトべを索2実ツリ文表振けト。間ケエニ違紙6順リぞど馬徳芸ーず女崎ロニヤ夫問ゃむざぽ目態らーる悪写リヱ場課ナ発1首せ読団今世すずら校訃美内よトの。携くてしは対歌モカヒタ気察オス高喫際段トイ批情ぞぐ当戦景フひみぶ会片株キ浜左けな長理ゆくふ併読がぱ毎裁エヌアナ探策ウ完知ラマ営開局ずる。島べや区目ハレエマ乱地マシ咲城九せずむび作向こンあ細信ぱ殺城だ無三増ノレル選市ーてつ歳保ほ施村け官村頼超星ぼ。申サキ重三要スチカ提意68隊くづっ必平げクぱだ祭初フテラリ貸経ちな国闘治リケネ識磐キ選隊サセヘ紙制俳寺博及ぎとへ。新化レ反57原払縄1利4面モイノ配医前けルラざ手熱ご供況おへ講略け両大るーぞ禁災な持議ヌホキ統出ぎンち米死ヱ覚属ヲ辞大イホヤ堀慶秒形己そスぞで。"
            , fruit_dragonfruit = "ドラゴンフルーツ（ピタヤ）"
            , drink_fruit_flavor_detox_water = "フルーツドリンク、レモン、キウイフルーツ、オレンジ、ブルーベリー、ラズベリー"
            , kandume6_mix = "パイナップル、ピーチ、チェリー、マンダリンのフルーツ缶"
            , sweets_fruit_tarte = "キウイフルーツ、ピーチ、バナナ、ストロベリー、ブラックベリーのフルーツタルト"
            , kandume1_pineapple = "パイナップル缶"
            , sweets_daifuku_ichigo = "お菓子大福いちご"
            , sweets_fruit_mitsumame = "桃、さくらんぼ、バナナ、マンダリンのスイーツフルーツみつまめ"
            , energy_lemon_denchi_battery = "エナジーレモンデンチバッテリー"
            , fruit_jelly = "マンダリン、キウイフルーツ、ピーチのフルーツゼリー"
            , fruitice_strawberry = "フルーツアイスストロベリー"
            , fruitice_orange = "フルーツアイスオレンジ"
            , fruits_basket = "フルーツバスケット"
            , food_fruit_sandwich = "ストロベリー、キウイフルーツ、バナナのフルーツサンドイッチ"
            , fruitice_kiwi = "フルーツアイスキウイ"
            , fruit_suika_red = "スイカ"
            }


productsData : Language -> List Product
productsData language =
    [ { id = stringToProductId "fruit_dragonfruit", name = .fruit_dragonfruit <| tr language, description = .lorem <| tr language, stars = 4, price = 1220, tag = "5% OFF", bg = 2 }
    , { id = stringToProductId "drink_fruit_flavor_detox_water", name = .drink_fruit_flavor_detox_water <| tr language, description = .lorem <| tr language, stars = 3, price = 890, tag = "NEW", bg = 0 }
    , { id = stringToProductId "kandume6_mix", name = .kandume6_mix <| tr language, description = .lorem <| tr language, stars = 4, price = 220, tag = "10% OFF", bg = 0 }
    , { id = stringToProductId "sweets_fruit_tarte", name = .sweets_fruit_tarte <| tr language, description = .lorem <| tr language, stars = 5, price = 2450, tag = "NEW", bg = 2 }
    , { id = stringToProductId "kandume1_pineapple", name = .kandume1_pineapple <| tr language, description = .lorem <| tr language, stars = 2, price = 180, tag = "NEW", bg = 1 }
    , { id = stringToProductId "sweets_daifuku_ichigo", name = .sweets_daifuku_ichigo <| tr language, description = .lorem <| tr language, stars = 4, price = 1850, tag = "15% OFF", bg = 1 }
    , { id = stringToProductId "sweets_fruit_mitsumame", name = .sweets_fruit_mitsumame <| tr language, description = .lorem <| tr language, stars = 5, price = 1990, tag = "15% OFF", bg = 2 }
    , { id = stringToProductId "energy_lemon_denchi_battery", name = .energy_lemon_denchi_battery <| tr language, description = .lorem <| tr language, stars = 4, price = 740, tag = "", bg = 3 }
    , { id = stringToProductId "fruit_jelly", name = .fruit_jelly <| tr language, description = .lorem <| tr language, stars = 4, price = 1240, tag = "", bg = 0 }
    , { id = stringToProductId "fruitice_strawberry", name = .fruitice_strawberry <| tr language, description = .lorem <| tr language, stars = 4, price = 380, tag = "", bg = 3 }
    , { id = stringToProductId "fruitice_orange", name = .fruitice_orange <| tr language, description = .lorem <| tr language, stars = 4, price = 380, tag = "", bg = 3 }
    , { id = stringToProductId "fruitice_kiwi", name = .fruitice_kiwi <| tr language, description = .lorem <| tr language, stars = 4, price = 380, tag = "", bg = 3 }
    , { id = stringToProductId "fruits_basket", name = .fruits_basket <| tr language, description = .lorem <| tr language, stars = 4, price = 9900, tag = "", bg = 1 }
    , { id = stringToProductId "food_fruit_sandwich", name = .food_fruit_sandwich <| tr language, description = .lorem <| tr language, stars = 4, price = 850000, tag = "", bg = 1 }
    , { id = stringToProductId "fruit_suika_red", name = .fruit_suika_red <| tr language, description = .lorem <| tr language, stars = 4, price = 3400, tag = "", bg = 2 }
    ]
