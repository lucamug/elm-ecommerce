module Dashboard exposing (main)

import Browser
import Codec
import Color
import ColorPicker
import Element.WithContext exposing (..)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import FeatherIcons
import Html
import Html.Attributes
import Json.Decode
import Main



-- TABLE OF CONTENT
--
-- | MAIN TYPES
-- |     Model
-- |     Page
-- |     DeviceAttrs
-- |     Device
-- | INIT
-- | MESSAGES
-- | UPDATE
-- | CONFIGURATION
-- | MAIN
-- | GENERIC HELPERS
-- | VIEWS
-- | VIEW HELPERS
-- | CODECS
-- | DESIGN SYSTEM
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
    { mainModel : Main.Model
    , mainConfiguration : Main.Configuration
    , device : Device
    , deviceZoom : Float
    , deviceWidth : Float
    , deviceHeight : Float
    , deviceBorder : Float
    , deviceStand : Bool
    , deviceStandScale : Float
    , deviceNotch : Bool
    , deviceCrown : Bool
    , sidebarShow : Bool
    , colorPickerState : ColorPicker.State
    , page : Page
    , jsonAsString : String
    , localStorageAsString : String
    }



--|     Page


type Page
    = Top
    | DesignSystem
    | Simulator


pageToString : Page -> String
pageToString page =
    case page of
        Top ->
            "Top"

        DesignSystem ->
            "Design System"

        Simulator ->
            "Simulator"



--|     DeviceAttrs


type alias DeviceAttrs =
    { name : String
    , border : Float
    , height : Float
    , notch : Bool
    , crown : Bool
    , stand : Bool
    , standScale : Float
    , width : Float
    , zoom : Float
    , icon : FeatherIcons.Icon
    }


injectDevice : DeviceAttrs -> Model -> Model
injectDevice device model =
    { model
        | deviceZoom = device.zoom
        , deviceWidth = device.width
        , deviceHeight = device.height
        , deviceBorder = device.border
        , deviceStand = device.stand
        , deviceStandScale = device.standScale
        , deviceNotch = device.notch
        , deviceCrown = device.crown
    }



--|     Device


type Device
    = Watch
    | Phone
    | Tablet
    | Monitor
    | TV


devices : List Device
devices =
    [ Watch
    , Phone
    , Tablet
    , Monitor
    , TV
    ]


deviceAttrs : Device -> DeviceAttrs
deviceAttrs device =
    case device of
        Watch ->
            { name = "watch", icon = FeatherIcons.watch, width = 312, height = 390, zoom = 0.6, stand = False, standScale = 1, notch = False, crown = True, border = 20 }

        Phone ->
            { name = "phone", icon = FeatherIcons.smartphone, width = 375, height = 812, zoom = 0.852, stand = False, standScale = 1, notch = True, crown = False, border = 12 }

        Tablet ->
            { name = "tablet", icon = FeatherIcons.tablet, width = 768, height = 1024, zoom = 0.643, stand = False, standScale = 1, notch = False, crown = False, border = 50 }

        Monitor ->
            { name = "monitor", icon = FeatherIcons.monitor, width = 1440, height = 900, zoom = 0.548, stand = True, standScale = 1, notch = False, crown = False, border = 30 }

        TV ->
            { name = "4k tv", icon = FeatherIcons.tv, width = 3840, height = 2160, zoom = 0.201, stand = True, standScale = 2, notch = False, crown = False, border = 50 }



-- ██ ███    ██ ██ ████████
-- ██ ████   ██ ██    ██
-- ██ ██ ██  ██ ██    ██
-- ██ ██  ██ ██ ██    ██
-- ██ ██   ████ ██    ██
--| INIT


init : Main.Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( mainModel, cmd ) =
            Main.init flags

        defaultDevice =
            Phone

        defaultDeviceAttrs =
            deviceAttrs defaultDevice
    in
    ( injectDevice defaultDeviceAttrs
        { mainModel = mainModel
        , mainConfiguration = Main.configuration
        , device = defaultDevice
        , deviceZoom = 0
        , deviceWidth = 0
        , deviceHeight = 0
        , deviceBorder = 0
        , deviceStand = False
        , deviceStandScale = 0
        , deviceNotch = False
        , deviceCrown = False
        , sidebarShow = False
        , colorPickerState = ColorPicker.empty
        , page = Top
        , jsonAsString = Main.productsDataToString <| Main.productsData Main.EN_US
        , localStorageAsString = Main.localStorageToString <| Main.modelToLocalStorage mainModel
        }
    , Cmd.map MsgMain cmd
    )



-- ███    ███ ███████ ███████ ███████  █████   ██████  ███████ ███████
-- ████  ████ ██      ██      ██      ██   ██ ██       ██      ██
-- ██ ████ ██ █████   ███████ ███████ ███████ ██   ███ █████   ███████
-- ██  ██  ██ ██           ██      ██ ██   ██ ██    ██ ██           ██
-- ██      ██ ███████ ███████ ███████ ██   ██  ██████  ███████ ███████
--| MESSAGES


type Msg
    = DoNothing
    | MsgMain Main.Msg
    | MsgColorPicker ColorPicker.Msg
    | ToggleDebugging
    | ToggleSidebarShow
    | ChangeZoom Float
    | ChangeWidth Float
    | ChangeHeight Float
    | ChangeBorder Float
    | ChangeMaxWidth Float
    | ChangeDevice Device
    | ChangePage Page
    | ChangeJsonAsString String
    | ChangeLocalStorageAsString String
    | ChangeJsonAsStringForced
    | FixJson
    | FixLocalStorage



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████
--| UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        MsgMain msgMain ->
            let
                ( mainModel, cmd ) =
                    Main.update msgMain model.mainModel
            in
            ( { model
                | mainModel = mainModel
                , localStorageAsString =
                    mainModel
                        |> Main.modelToLocalStorage
                        |> Main.localStorageToString
              }
            , Cmd.map MsgMain cmd
            )

        ToggleDebugging ->
            let
                mainConfiguration =
                    model.mainConfiguration
            in
            ( { model
                | mainConfiguration = { mainConfiguration | debugging = not mainConfiguration.debugging }
                , page =
                    if model.page == DesignSystem && not mainConfiguration.debugging then
                        Simulator

                    else
                        model.page
              }
            , Cmd.none
            )

        ToggleSidebarShow ->
            ( { model | sidebarShow = not model.sidebarShow }, Cmd.none )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        ChangeZoom deviceZoom ->
            ( { model | deviceZoom = deviceZoom }, Cmd.none )

        ChangeWidth value ->
            ( { model | deviceWidth = value }, Cmd.none )

        ChangeHeight value ->
            ( { model | deviceHeight = value }, Cmd.none )

        ChangeBorder value ->
            ( { model | deviceBorder = value }, Cmd.none )

        ChangeMaxWidth value ->
            let
                mainConfiguration =
                    model.mainConfiguration
            in
            ( { model | mainConfiguration = { mainConfiguration | maxWidth = round value } }, Cmd.none )

        ChangeDevice device ->
            let
                deviceAttrs_ =
                    deviceAttrs device
            in
            ( injectDevice deviceAttrs_
                { model
                    | page = Simulator
                    , device = device
                }
            , Cmd.none
            )

        ChangeJsonAsStringForced ->
            let
                mainModel =
                    model.mainModel
            in
            ( { model
                | mainModel =
                    { mainModel | httpRequest = stringToMainHttpRequest model.jsonAsString }
              }
            , Cmd.none
            )

        ChangeJsonAsString jsonAsString ->
            let
                mainModel =
                    model.mainModel
            in
            ( { model
                | jsonAsString = jsonAsString
                , mainModel =
                    case stringToMainHttpRequest jsonAsString of
                        Main.Success products ->
                            { mainModel | httpRequest = Main.Success products }

                        _ ->
                            mainModel
              }
            , Cmd.none
            )

        ChangeLocalStorageAsString localStorageAsString ->
            let
                mainModel =
                    Main.changeLocalStorage localStorageAsString model.mainModel
            in
            ( { model
                | localStorageAsString = localStorageAsString
                , mainModel = mainModel
              }
            , Cmd.none
            )

        FixLocalStorage ->
            ( { model
                | localStorageAsString =
                    model.mainModel
                        |> Main.modelToLocalStorage
                        |> Main.localStorageToString
              }
            , Cmd.none
            )

        FixJson ->
            let
                mainModel =
                    model.mainModel

                ( jsonAsString, httpRequest ) =
                    case model.mainModel.httpRequest of
                        Main.Success products ->
                            ( Main.productsDataToString products, Main.Success products )

                        _ ->
                            let
                                initProducts =
                                    Main.productsData Main.EN_US
                            in
                            ( Main.productsDataToString initProducts, Main.Success initProducts )
            in
            ( { model
                | jsonAsString = jsonAsString
                , mainModel = { mainModel | httpRequest = httpRequest }
              }
            , Cmd.none
            )

        MsgColorPicker msgColorPicker ->
            let
                color_ =
                    elementColorToColor model.mainConfiguration.paletteLight.primary

                ( m, color ) =
                    ColorPicker.update
                        msgColorPicker
                        color_
                        model.colorPickerState

                mainConfiguration =
                    model.mainConfiguration

                paletteLight =
                    mainConfiguration.paletteLight
            in
            ( { model
                | colorPickerState = m

                -- , color = color |> Maybe.withDefault model.color
                , mainConfiguration =
                    { mainConfiguration
                        | paletteLight =
                            { paletteLight
                                | primary = colorToElementColor (Maybe.withDefault color_ color)
                                , external = colorToElementColor (Maybe.withDefault color_ color)
                            }
                    }
              }
            , Cmd.none
            )



--  ██████  ██████  ███    ██ ███████ ██  ██████  ██    ██ ██████   █████  ████████ ██  ██████  ███    ██
-- ██      ██    ██ ████   ██ ██      ██ ██       ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ████   ██
-- ██      ██    ██ ██ ██  ██ █████   ██ ██   ███ ██    ██ ██████  ███████    ██    ██ ██    ██ ██ ██  ██
-- ██      ██    ██ ██  ██ ██ ██      ██ ██    ██ ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ██  ██ ██
--  ██████  ██████  ██   ████ ██      ██  ██████   ██████  ██   ██ ██   ██    ██    ██  ██████  ██   ████
--| CONFIGURATION


configuration : { sidebarWidth : number1 }
configuration =
    { sidebarWidth = 400 }



-- ███    ███  █████  ██ ███    ██
-- ████  ████ ██   ██ ██ ████   ██
-- ██ ████ ██ ███████ ██ ██ ██  ██
-- ██  ██  ██ ██   ██ ██ ██  ██ ██
-- ██      ██ ██   ██ ██ ██   ████
--| MAIN


main : Program Main.Flags Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "elm eCommerce", body = view model }
        , update = update
        , subscriptions = \model -> Sub.map MsgMain (Main.subscriptions model)
        }



-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
--| GENERIC HELPERS


elementColorToColor : Color -> Color.Color
elementColorToColor elementColor =
    let
        { red, green, blue, alpha } =
            toRgb elementColor
    in
    Color.rgba red green blue alpha


colorToElementColor : Color.Color -> Color
colorToElementColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    rgba red green blue alpha



-- ██    ██ ██ ███████ ██     ██ ███████
-- ██    ██ ██ ██      ██     ██ ██
-- ██    ██ ██ █████   ██  █  ██ ███████
--  ██  ██  ██ ██      ██ ███ ██      ██
--   ████   ██ ███████  ███ ███  ███████
--| VIEWS


view : Model -> List (Html.Html Msg)
view model =
    let
        context =
            Main.contextBuilder model.mainConfiguration model.mainModel.mode model.mainModel.language
    in
    [ layoutWith context { options = Main.options } [] <|
        if model.mainModel.pwd == Main.correctPwd then
            viewPage model

        else
            map MsgMain <| Main.viewPwd model.mainModel.pwd
    ]


viewPage : Model -> Element Main.Context Msg
viewPage model =
    el
        [ width fill
        , paddingEach { top = 0, right = 0, bottom = 0, left = 70 }
        , inFront <|
            el
                [ width <| px 70
                , Main.attrWithContext <| \c -> Background.color c.palette.surface2dp
                , Main.attrWithContext <| \c -> Font.color c.palette.onBackground
                , alignTop
                , Main.style "position" "fixed"
                , Main.style "height" "100vh"
                , Border.widthEach { bottom = 0, left = 0, right = 0, top = 0 }
                , Main.attrWithContext <| \c -> Border.color c.palette.separator
                ]
            <|
                Main.elementWithContext <|
                    \c ->
                        column [ spacing 15, scrollbarY ]
                            ([ Input.button [ width fill ]
                                { onPress = Just <| ChangePage Top
                                , label =
                                    column
                                        [ paddingXY 0 10
                                        , width fill
                                        , centerX
                                        , spacing 3
                                        , Background.color <| Main.paletteLight.external
                                        , Font.color c.palette.onExternal
                                        ]
                                        [ paragraph [ centerX, Font.center ] [ text "elm" ]
                                        , FeatherIcons.shoppingCart
                                            |> FeatherIcons.withSize 30
                                            |> FeatherIcons.withStrokeWidth 1
                                            |> FeatherIcons.toHtml
                                                [ Html.Attributes.style "stroke" (Main.colorToCssString c.palette.onExternal) ]
                                            |> html
                                            |> el [ centerX ]
                                        , paragraph [ Font.size 11, Font.center ] [ text "eCommerce" ]
                                        , paragraph [ Font.size 12, Font.center ] [ text "dashboard" ]
                                        ]
                                }
                             , map MsgMain <|
                                menuIcon
                                    (case model.mainModel.mode of
                                        Main.Light ->
                                            FeatherIcons.sun

                                        Main.Dark ->
                                            FeatherIcons.moon
                                    )
                                    "dark / light"
                                    Main.ToggleMode
                                    False
                             , menuIcon FeatherIcons.tag
                                "tags"
                                ToggleDebugging
                                model.mainConfiguration.debugging
                             , menuIcon FeatherIcons.grid
                                (String.toLower <| pageToString DesignSystem)
                                (ChangePage
                                    (if model.page == DesignSystem then
                                        Simulator

                                     else
                                        DesignSystem
                                    )
                                )
                                (model.page == DesignSystem)
                             , menuIcon FeatherIcons.settings "settings" ToggleSidebarShow model.sidebarShow
                             ]
                                ++ List.map
                                    (\device ->
                                        let
                                            deviceAttrs_ =
                                                deviceAttrs device
                                        in
                                        menuIcon deviceAttrs_.icon
                                            deviceAttrs_.name
                                            (ChangeDevice device)
                                            (model.device == device && model.page /= DesignSystem && model.page /= Top)
                                    )
                                    devices
                                ++ [ newTabLink [ centerX ]
                                        { label =
                                            column [ centerX, spacing 5 ]
                                                [ Main.atomIcon [ centerX ] { color = .onBackground, fill = False, shape = FeatherIcons.github, size = 30 }
                                                , paragraph [ Font.size 12, centerX, Font.center, spacing 1 ] [ text "source code" ]
                                                ]
                                        , url = "https://github.com/lucamug/elm-ecommerce.git"
                                        }
                                   ]
                            )
        , inFront <|
            el
                [ width <| px configuration.sidebarWidth
                , alignRight
                , Main.style "height" "100vh"
                , Main.style "position" "fixed"
                , Main.style "transition" "0.5s"
                , Border.widthEach { bottom = 0, left = 1, right = 0, top = 0 }
                , Main.attrWithContext <| \c -> Background.color c.palette.surface2dp
                , Main.attrWithContext <| \c -> Font.color c.palette.onBackground
                , Main.attrWithContext <| \c -> Border.color c.palette.separator
                , if model.sidebarShow then
                    moveRight 0

                  else
                    moveRight configuration.sidebarWidth
                ]
            <|
                viewSidebar model
        , inFront <|
            Input.button
                [ alignRight
                , moveDown 10
                , moveLeft 10
                , Main.attrWithContext <| \c -> Background.color c.palette.surface2dp
                , padding 10
                , Border.rounded 100
                , Main.style "position" "fixed"
                ]
                { label =
                    Main.atomIcon []
                        { color = .onBackground
                        , fill = False
                        , shape =
                            if model.sidebarShow then
                                FeatherIcons.x

                            else
                                FeatherIcons.moreVertical
                        , size = 25
                        }
                , onPress = Just ToggleSidebarShow
                }
        ]
        (case model.page of
            Top ->
                el
                    [ htmlAttribute <| Html.Attributes.style "height" "100vh"
                    , Background.color <| Main.paletteLight.external
                    , width fill
                    ]
                <|
                    image [ width (fill |> maximum 900), centerX, centerY ] { src = "elm-ecommerce-logo.png", description = "Logo" }

            Simulator ->
                viewDevice model

            DesignSystem ->
                el
                    [ width fill
                    , paddingEach
                        { bottom = 0
                        , left = 0
                        , right =
                            if model.sidebarShow then
                                configuration.sidebarWidth

                            else
                                0
                        , top = 0
                        }
                    , Main.attrWithContext <| \c -> Font.color c.palette.onBackground
                    , Main.attrWithContext <| \c -> Background.color c.palette.surface
                    ]
                <|
                    map MsgMain <|
                        viewDesignSystem model.mainModel
        )


stand : Color -> Float -> List (Attribute context msg)
stand colorDevice scale =
    [ inFront <|
        image
            [ alignBottom
            , moveDown <| 56 * scale
            , width <| px <| round <| 280 * scale
            , centerX
            , rotate pi
            ]
            { src = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 25 5'%3E%3Cpath fill='rgb(35,35,35)' d='M0,0 c5,0 5,5 5,5 h15 c0,0 0,-5 5,-5 z'/%3E%3C/svg%3E", description = "" }
    , inFront <|
        el
            [ width <| px <| round <| 900 * scale
            , height <| px <| round <| 20 * scale
            , Background.color colorDevice
            , alignBottom
            , centerX
            , moveDown <| 70 * scale
            , Border.roundEach { topLeft = 40, topRight = 40, bottomLeft = 20, bottomRight = 20 }
            ]
        <|
            none
    ]


viewDevice : Model -> Element Main.Context Msg
viewDevice model =
    let
        colorDevice =
            rgb255 35 35 35
    in
    el
        [ Main.style "height" "100vh"
        , width fill

        -- , Main.attrWithContext <| \c -> Background.color c.palette.surface
        , Background.color <| Main.paletteLight.external
        , Main.transition "transform 0.3s"
        , moveLeft <|
            if model.sidebarShow then
                configuration.sidebarWidth / 2

            else
                0
        ]
    <|
        el
            [ centerX
            , centerY
            ]
        <|
            el
                (Main.style "zoom" (String.fromFloat model.deviceZoom)
                    :: (if model.deviceCrown then
                            [ inFront <|
                                el
                                    [ width <| px 25
                                    , height <| px 50
                                    , Background.color colorDevice
                                    , Border.rounded 100
                                    , alignTop
                                    , alignRight
                                    , moveDown 60
                                    , moveRight 15
                                    ]
                                <|
                                    none
                            , inFront <|
                                image
                                    [ alignTop
                                    , moveUp 56
                                    , width <| px 280
                                    , centerX
                                    , rotate pi
                                    ]
                                    { src = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 25 5'%3E%3Cpath fill='rgb(35,35,35)' d='M0,0 c5,0 5,5 5,5 h15 c0,0 0,-5 5,-5 z'/%3E%3C/svg%3E", description = "" }
                            , inFront <|
                                image
                                    [ alignBottom
                                    , moveDown 56
                                    , width <| px 280
                                    , centerX
                                    ]
                                    { src = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 25 5'%3E%3Cpath fill='rgb(35,35,35)' d='M0,0 c5,0 5,5 5,5 h15 c0,0 0,-5 5,-5 z'/%3E%3C/svg%3E", description = "" }
                            ]

                        else
                            []
                       )
                    ++ (if model.deviceStand then
                            stand colorDevice model.deviceStandScale

                        else
                            []
                       )
                )
            <|
                el
                    ([ width <| px <| round <| model.deviceWidth + (model.deviceBorder * 2)
                     , height <| px <| round <| model.deviceHeight + (model.deviceBorder * 2)
                     , clip
                     , Border.color colorDevice
                     , Main.attrWithContext <| \c -> Background.color c.palette.background
                     , Border.width <| round model.deviceBorder
                     , Border.rounded 40
                     , Border.shadow { offset = ( 5, 5 ), size = 0, blur = 20, color = rgba 0 0 0 0.15 }
                     , if model.deviceNotch then
                        inFront <|
                            el
                                [ width <| px 180
                                , height <| px 35
                                , Background.color colorDevice
                                , Border.rounded 100
                                , alignTop
                                , centerX
                                , moveUp 17
                                ]
                            <|
                                none

                       else
                        Main.noneAttr
                     ]
                        ++ List.map (\attr -> mapAttribute MsgMain attr) (Main.mainAttrs model.mainConfiguration model.mainModel)
                    )
                    (map MsgMain <|
                        el [ width fill, height fill, scrollbarY ] <|
                            Main.viewPage
                                model.mainModel
                    )


viewSidebar : Model -> Element Main.Context Msg
viewSidebar model =
    column
        [ width fill
        , padding 30
        , scrollbarY
        , Font.size 16
        , spacing 10
        ]
        [ paragraph [ Font.size 30, Font.center, paddingXY 0 20 ] [ text "Settings" ]
        , paragraph [ Font.size 20, Font.center, paddingXY 0 20 ] [ text "Device" ]
        , slider { textBefore = "Zoom", value = model.deviceZoom, max = 2, min = 0.1, step = 0.001, onChange = ChangeZoom, enabled = True, textAfter = "" }
        , slider { textBefore = "Width", value = model.deviceWidth, max = 3840, min = 280, step = 1, onChange = ChangeWidth, enabled = True, textAfter = " px" }
        , slider { textBefore = "Height", value = model.deviceHeight, max = 2000, min = 280, step = 1, onChange = ChangeHeight, enabled = True, textAfter = " px" }
        , slider { textBefore = "Border", value = model.deviceBorder, max = 100, min = 0, step = 1, onChange = ChangeBorder, enabled = True, textAfter = " px" }
        , paragraph [ Font.size 20, Font.center, paddingXY 0 20 ] [ text "Content" ]
        , slider { textBefore = "Max Width", value = toFloat model.mainConfiguration.maxWidth, max = 3840, min = 280, step = 1, onChange = ChangeMaxWidth, enabled = True, textAfter = " px" }
        , paragraph [ Font.size 20, Font.center, paddingXY 0 20 ] [ text "Primary Color" ]
        , el [ centerX ] <|
            html <|
                Html.map MsgColorPicker <|
                    ColorPicker.view
                        (elementColorToColor model.mainConfiguration.paletteLight.primary)
                        model.colorPickerState
        , paragraph [ Font.size 20, Font.center, paddingXY 0 20 ] [ text "Local Storage Response" ]
        , el
            [ width fill
            , inFront <|
                case Main.stringToLocalStorage model.localStorageAsString of
                    Err error ->
                        column
                            [ moveUp 220
                            , Background.color <| rgb 1 0.9 0.9
                            , Font.color <| rgb 0.8 0 0
                            , width fill
                            , Border.width 1
                            , Border.color <| rgb 0.8 0 0
                            ]
                            [ el
                                [ Font.family [ Font.monospace ]
                                , height <| px 150
                                , width fill
                                , scrollbars
                                , padding 10
                                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                                ]
                                (text <| Json.Decode.errorToString error)
                            , row [ width fill ]
                                [ Input.button
                                    [ width fill
                                    , paddingXY 0 10
                                    , Font.center
                                    , mouseOver [ Background.color <| rgb 1 0.95 0.95 ]
                                    ]
                                    { label = text "Fix the problem", onPress = Just FixLocalStorage }
                                ]
                            ]

                    _ ->
                        none
            ]
          <|
            Input.multiline
                [ height <| px 300
                , Font.family [ Font.monospace ]
                , Font.size 13
                , Main.attrWithContext <| \c -> Background.color c.palette.background
                , Main.attrWithContext <| \c -> Font.color c.palette.onBackground
                ]
                { onChange = ChangeLocalStorageAsString
                , text = model.localStorageAsString
                , placeholder = Nothing
                , label = Input.labelHidden ""
                , spellcheck = False
                }
        , paragraph [ Font.size 20, Font.center, paddingXY 0 20 ] [ text "Server Response" ]
        , paragraph [] [ text "From here you can change the HTTP response and see the application changing in real-time. Modifications are applied as you type if the JSON remains correct. In case JSON goes temporarily in the wrong state, the real-time updates are suspended. You can force the update of the wrong JSON in case you want to simulate such a situation with the application." ]
        , el
            [ width fill
            , inFront <|
                case stringToMainHttpRequest model.jsonAsString of
                    Main.Failure error ->
                        column
                            [ moveUp 220
                            , Background.color <| rgb 1 0.9 0.9
                            , Font.color <| rgb 0.8 0 0
                            , width fill
                            , Border.width 1
                            , Border.color <| rgb 0.8 0 0
                            ]
                            [ el
                                [ Font.family [ Font.monospace ]
                                , height <| px 150
                                , width fill
                                , scrollbars
                                , padding 10
                                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                                ]
                                (text error)
                            , row [ width fill ]
                                [ Input.button
                                    [ width fill
                                    , paddingXY 0 10
                                    , Font.center
                                    , mouseOver [ Background.color <| rgb 1 0.95 0.95 ]
                                    , Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }
                                    ]
                                    { label = text "Send the data anyway", onPress = Just ChangeJsonAsStringForced }
                                , Input.button
                                    [ width fill
                                    , paddingXY 0 10
                                    , Font.center
                                    , mouseOver [ Background.color <| rgb 1 0.95 0.95 ]
                                    ]
                                    { label = text "Fix the problem", onPress = Just FixJson }
                                ]
                            ]

                    _ ->
                        none
            ]
          <|
            Input.multiline
                [ height <| px 300
                , Font.family [ Font.monospace ]
                , Font.size 13
                , Main.attrWithContext <| \c -> Background.color c.palette.background
                , Main.attrWithContext <| \c -> Font.color c.palette.onBackground
                ]
                { onChange = ChangeJsonAsString
                , text = model.jsonAsString
                , placeholder = Nothing
                , label = Input.labelHidden ""
                , spellcheck = False
                }
        , paragraph [ Font.size 20, Font.center, paddingXY 0 20 ] [ text "Wrong Routes" ]
        , link [ Font.underline ] { url = "/x", label = paragraph [] [ text "Click here to test a wrong route" ] }
        , link [ Font.underline ] { url = "/p/x", label = paragraph [] [ text "Click here to test a wrong product id" ] }
        ]



-- ██    ██ ██ ███████ ██     ██     ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██    ██ ██ ██      ██     ██     ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ██    ██ ██ █████   ██  █  ██     ███████ █████   ██      ██████  █████   ██████  ███████
--  ██  ██  ██ ██      ██ ███ ██     ██   ██ ██      ██      ██      ██      ██   ██      ██
--   ████   ██ ███████  ███ ███      ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
--| VIEW HELPERS


menuIcon : FeatherIcons.Icon -> String -> msg -> Bool -> Element Main.Context msg
menuIcon shape string msg active =
    Input.button [ centerX ]
        { label =
            column [ centerX, spacing 5 ]
                [ Main.atomIcon [ centerX ] { color = .onBackground, fill = active, shape = shape, size = 30 }
                , paragraph [ Font.size 12, centerX, Font.center, spacing 1 ] [ text string ]
                ]
        , onPress = Just <| msg
        }



--  ██████  ██████  ██████  ███████  ██████ ███████
-- ██      ██    ██ ██   ██ ██      ██      ██
-- ██      ██    ██ ██   ██ █████   ██      ███████
-- ██      ██    ██ ██   ██ ██      ██           ██
--  ██████  ██████  ██████  ███████  ██████ ███████
--| CODECS


stringToMainHttpRequest : String -> Main.HttpRequest
stringToMainHttpRequest jsonAsString =
    case Codec.decodeString (Codec.list Main.codecProduct) jsonAsString of
        Ok products ->
            Main.Success products

        Err err ->
            Main.Failure (Json.Decode.errorToString err)



-- ██████  ███████ ███████ ██  ██████  ███    ██     ███████ ██    ██ ███████ ████████ ███████ ███    ███
-- ██   ██ ██      ██      ██ ██       ████   ██     ██       ██  ██  ██         ██    ██      ████  ████
-- ██   ██ █████   ███████ ██ ██   ███ ██ ██  ██     ███████   ████   ███████    ██    █████   ██ ████ ██
-- ██   ██ ██           ██ ██ ██    ██ ██  ██ ██          ██    ██         ██    ██    ██      ██  ██  ██
-- ██████  ███████ ███████ ██  ██████  ██   ████     ███████    ██    ███████    ██    ███████ ██      ██
--| DESIGN SYSTEM


listColor : List ( Main.Palette -> Color, String )
listColor =
    [ ( .primary, "primary" )
    , ( .secondary, "secondary" )
    , ( .external, "external" )
    , ( .background, "background" )
    , ( .surface, "surface" )
    , ( .surface2dp, "surface2dp" )
    , ( .onExternal, "onExternal" )
    , ( .onPrimary, "onPrimary" )
    , ( .onBackground, "onBackground" )
    , ( .onBackgroundDim, "onBackgroundDim" )
    , ( .onSecondary, "onSecondary" )
    , ( .mask, "mask" )
    , ( .separator, "separator" )
    , ( .error, "error" )
    ]


colorToString : Color -> String
colorToString color =
    let
        components =
            [ .red, .green, .blue ]

        rgb =
            toRgb color
    in
    "rgba( "
        ++ (String.join ", " <| List.map (\comp -> String.fromInt (round (comp rgb * 255))) components)
        ++ ", "
        ++ String.fromFloat rgb.alpha
        ++ ")"


viewPalette : Main.Palette -> List (Main.Element msg)
viewPalette p =
    let
        luminance color =
            let
                { red, green, blue } =
                    toRgb color
            in
            0.299 * red + 0.587 * green + 0.114 * blue
    in
    List.map
        (\( map, _ ) ->
            el
                [ Background.color <| map p
                , Main.attrWithContext <| \c -> Border.color c.palette.separator
                , Font.color <|
                    if (luminance <| map p) < 0.5 then
                        rgb 1 1 1

                    else
                        rgb 0 0 0
                , width fill
                , paddingXY 10 5
                ]
                (text <| colorToString <| map p)
        )
        listColor


lorem : String
lorem =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In ac nunc in mi mollis interdum ac et velit. In id elementum turpis. Fusce ornare tortor at magna viverra iaculis. Integer eget purus gravida, ultricies quam nec, dictum dolor. Morbi sit amet leo ac leo laoreet lobortis ornare ac erat. Phasellus gravida a nibh in tempus. Aenean dictum augue id urna egestas, eget tristique ipsum lobortis. Nunc tempor dictum quam ut pharetra. Sed tincidunt nisi mi, id porta orci facilisis eget. Sed et ultrices tellus. Morbi ac diam condimentum, accumsan lectus sed, scelerisque massa. In maximus tortor nec pharetra posuere. Pellentesque eget sapien urna. Vestibulum pulvinar nisi ultrices enim volutpat, a placerat tortor laoreet."


p1 : Main.Product
p1 =
    { id = Main.stringToProductId "fruit_dragonfruit"
    , name = "Dragon Fruit (Pitaya)"
    , description = lorem
    , stars = 4
    , price = 1220
    , tag = "5% OFF"
    , bg = 2
    }


p2 : Main.Product
p2 =
    { id = Main.stringToProductId "drink_fruit_flavor_detox_water"
    , name = "Fruit Drink, with Lemons, Kiwifruits and Organges"
    , description = lorem
    , stars = 3
    , price = 890
    , tag = "NEW"
    , bg = 0
    }


p3 : Main.Product
p3 =
    { id = Main.stringToProductId "kandume1_pineapple"
    , name = "Pinapple can"
    , description = lorem
    , stars = 2
    , price = 32850
    , tag = "NEW"
    , bg = 1
    }


p4 : Main.Product
p4 =
    { id = Main.stringToProductId "energy_lemon_denchi_battery"
    , name = "Lemon Battery"
    , description = lorem
    , stars = 4
    , price = 1990
    , tag = ""
    , bg = 3
    }


viewDesignSystem : Main.Model -> Main.Element Main.Msg
viewDesignSystem modelReal =
    let
        modelEmpty =
            Tuple.first
                (Main.init
                    { localStorage = ""
                    , locationHref = ""
                    }
                )

        modelFull =
            { modelEmpty
                | cart = [ p3.id, p3.id, p4.id ]
                , favorites = [ p1.id, p3.id ]
                , httpRequest = Main.Success [ p1, p2, p3, p4 ]
            }

        externalElement element =
            Main.elementWithContext (\c -> el [ Background.color c.palette.external, width fill ] element)
    in
    column
        [ width fill
        , alignTop
        , padding 20
        , spacing 30
        , Font.size 16
        ]
    <|
        paragraph [ Font.size 30, Font.center ] [ text <| pageToString DesignSystem ]
            --
            -- PALETTE
            :: separation "Palette"
            ++ [ el (attrsDesignSystem ++ [ centerX ]) <|
                    el
                        [ scrollbars
                        , Border.widthEach { bottom = 0, left = 1, right = 1, top = 1 }
                        , Main.attrWithContext <| \c -> Border.color c.palette.mask
                        , Border.width 5
                        ]
                    <|
                        let
                            headerAttrs =
                                [ Main.attrWithContext <| \c -> Background.color c.palette.mask
                                , width fill
                                , padding 10
                                , Font.center
                                ]
                        in
                        row []
                            [ column [] <|
                                ((el headerAttrs <| text "Name")
                                    :: List.map
                                        (\( _, name ) ->
                                            el
                                                [ paddingXY 10 5
                                                , Main.attrWithContext <| \c -> Border.color c.palette.mask
                                                ]
                                            <|
                                                text name
                                        )
                                        listColor
                                )
                            , Main.elementWithContext <| \c -> column [ width fill ] <| ((el headerAttrs <| text "In use") :: viewPalette c.palette)
                            , column [ width fill ] <| (el headerAttrs <| text "Light") :: viewPalette Main.configuration.paletteLight
                            , column [ width fill ] <| (el headerAttrs <| text "Dark") :: viewPalette Main.configuration.paletteDark
                            ]
               ]
            --
            -- ORGANISMS
            ++ separation "Organisms"
            ++ sectionB "orgHeader" (externalElement <| Main.orgHeader modelFull.page)
            ++ sectionB "orgProductsGrid (Loading)" (Main.orgProductsGrid modelEmpty 4)
            ++ sectionB "orgProductsGrid" (Main.orgProductsGrid modelFull 4)
            ++ sectionB "orgProductDetails (Loading)" (Main.orgProductDetails Main.productInit (Main.quantityInTheCart modelEmpty.cart Main.productInit) True)
            ++ sectionB "orgProductDetails" (Main.orgProductDetails p1 (Main.quantityInTheCart modelFull.cart p1) True)
            ++ sectionB "orgProductsRows (Loading)" (Main.orgProductsRows { modelFull | httpRequest = Main.Fetching })
            ++ sectionB "orgProductsRows (Empty Cart)" (Main.orgProductsRows { modelEmpty | page = Main.Cart Nothing })
            ++ sectionB "orgProductsRows (Cart)" (Main.orgProductsRows { modelFull | page = Main.Cart Nothing })
            ++ sectionB "orgProductsRows (Favorites)" (Main.orgProductsRows { modelFull | page = Main.Favorites Nothing })
            ++ sectionB "orgProductsStrip" (Main.orgProductsStrip modelFull.httpRequest)
            ++ sectionB "orgFooter" (externalElement Main.orgFooter)
            ++ sectionB "orgError" (Main.orgError Main.DoNothing [ text "Error" ])
            --
            -- MOLECULES
            ++ separation "Molecules"
            ++ sectionA "molAddToFavorites" (Main.molAddToFavorites [] p1 True)
            ++ sectionA "molAddToCart" (Main.molAddToCart [] p1 True)
            ++ sectionA "molAddToCartLarge" (Main.molAddToCartLarge [] p1 (Main.quantityInTheCart modelFull.cart p1))
            ++ sectionA "molClose" (Main.molSecondaryButton [] FeatherIcons.x Main.DoNothing)
            ++ sectionA "molStars" (Main.molStars [] p1.stars)
            ++ sectionA "molMenuMain" (externalElement <| Main.molMenuMain [] modelFull)
            ++ sectionA "molMenuSortBy" (Main.molMenuSortBy [] modelFull.sort modelFull.sortDirection)
            ++ sectionA "molProductForStrip" (Main.molProductForStrip [] p1)
            ++ sectionA "molProductForGrid" (Main.molProductForGrid [] modelFull.cart modelFull.favorites (Main.pageToSearchQuery modelFull.page) p1)
            ++ sectionA "molEmptyList (Cart)" (Main.molEmptyList (Main.Cart Nothing))
            ++ sectionA "molEmptyList (Favorites)" (Main.molEmptyList (Main.Favorites Nothing))
            ++ sectionA "molProductNotFound" (Main.molProductNotFound (Main.Favorites Nothing))
            --
            -- MOLECULES VARIATIONS
            ++ separation "Molecules Variations"
            ++ sectionA "molMenuMain (Functional)" (externalElement <| Main.molMenuMain [] modelReal)
            ++ sectionA "molMenuMain (Top)" (externalElement <| Main.molMenuMain [] { modelFull | page = Main.Top Nothing })
            ++ sectionA "molMenuMain (Cart)" (externalElement <| Main.molMenuMain [] { modelFull | page = Main.Cart Nothing })
            ++ sectionA "molMenuMain (Favorites)" (externalElement <| Main.molMenuMain [] { modelFull | page = Main.Favorites Nothing })
            ++ sectionA "molMenuMain (Search)" (externalElement <| Main.molMenuMain [] { modelFull | page = Main.pageFromSearchQuery (Just "search query") modelFull.page })
            --
            -- ATOMS
            ++ separation "Atoms"
            ++ sectionA "atomLogo" (Main.elementWithContext <| \c -> Main.atomLogo [] { size = 28, color = c.palette.onBackground })
            ++ sectionA "atomLinkInternal" (Main.atomLinkInternal [] { label = text "Example link", page = modelEmpty.page })
            ++ sectionA "atomPrice" (Main.atomPrice [] 20 99999)
            ++ sectionA "atomImageProduct" (Main.atomImageProduct [] 10 p1)
            ++ sectionA "atomTagNew" (Main.atomTagNew [] "TAG")
            ++ sectionA "atomTagNumber" (Main.atomTagNumber [] 99)
            ++ sectionA "atomIcon" (Main.atomIcon [] { color = .onBackground, fill = False, shape = FeatherIcons.shoppingCart, size = 30 })
            ++ [ Main.css ]


separation : String -> List (Main.Element msg)
separation name =
    [ el
        [ width fill
        , Font.size 25
        , Font.center
        , paddingEach { top = 80, right = 0, bottom = 0, left = 0 }
        , Font.family [ Font.monospace ]
        ]
      <|
        paragraph [] [ text <| String.toUpper name ]
    ]


sectionA : String -> Main.Element msg -> List (Main.Element msg)
sectionA name block =
    [ column
        attrsDesignSystem
        [ paragraph [ Font.size 20, Font.family [ Font.monospace ] ] [ text name ]
        , preview Nothing block
        ]
    ]


attrsDesignSystem : List (Attribute Main.Context msg)
attrsDesignSystem =
    [ spacing 30 ]


sectionB : String -> Main.Element msg -> List (Main.Element msg)
sectionB name block =
    [ column
        (attrsDesignSystem ++ [ width fill ])
        [ paragraph
            [ Font.size 20
            , Font.center
            , Font.family [ Font.monospace ]
            , Main.attrWithContext <| \c -> Background.color c.palette.mask
            , padding 10
            ]
            [ text name ]
        , preview (Just 360) block
        , preview (Just 800) block
        ]
    ]


preview : Maybe Int -> Main.Element msg -> Main.Element msg
preview size block =
    let
        paddingSize =
            5

        borderSize =
            5

        sizeString =
            case size of
                Just int ->
                    String.fromInt int

                Nothing ->
                    ""
    in
    column
        [ spacing 4
        , alignTop
        ]
        [ paragraph [ Font.size 13 ] [ text sizeString ]
        , row
            [ spacing (paddingSize * 2)
            , padding paddingSize
            ]
            [ column
                [ spacing 5
                , alignTop
                , Main.attrWithContext <| \c -> Background.color c.palette.surface2dp
                ]
                [ el
                    [ case size of
                        Just int ->
                            width <| px <| (int + borderSize * 2)

                        Nothing ->
                            Main.noneAttr
                    , Main.attrWithContext <| \c -> Border.color c.palette.separator
                    , Border.width borderSize
                    , Border.dashed
                    , alignTop
                    ]
                  <|
                    block
                ]
            ]
        ]


sliderStyle : List (Main.Attribute msg)
sliderStyle =
    [ height <| fill
    , width fill
    , behindContent
        (el
            [ width fill
            , height (px 6)
            , centerY
            , Background.color <| rgb 0.5 0.5 0.5
            , Border.rounded 2
            ]
            none
        )
    ]


slider :
    { max : Float
    , min : Float
    , onChange : Float -> Msg
    , step : Float
    , textAfter : String
    , textBefore : String
    , value : Float
    , enabled : Bool
    }
    -> Main.Element Msg
slider args =
    row
        [ spacing 20
        , width fill
        , alpha <|
            if args.enabled then
                1

            else
                0.3
        ]
        [ el [ width <| px 100, Font.alignRight ] <| text args.textBefore
        , Input.slider sliderStyle
            { onChange =
                if args.enabled then
                    args.onChange

                else
                    \_ -> DoNothing
            , label = Input.labelHidden ""
            , min = args.min
            , max = args.max
            , value = args.value
            , thumb = Input.defaultThumb
            , step = Just args.step
            }
        , el [ width <| px 60, Font.alignRight ] <| text <| String.fromFloat args.value ++ args.textAfter
        ]
