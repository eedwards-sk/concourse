port module Dashboard exposing (Model, init, subscriptions, update, view)

import Array
import Char
import Concourse
import Concourse.Cli as Cli
import Concourse.Pipeline
import Concourse.PipelineStatus
import Concourse.User
import Css
import Dashboard.APIData as APIData
import Dashboard.Details as Details
import Dashboard.Group as Group
import Dashboard.Models as Models
import Dashboard.Msgs as Msgs exposing (Msg(..))
import Dashboard.SubState as SubState
import Dashboard.Styles as Styles
import Dom
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes
    exposing
        ( attribute
        , css
        , class
        , classList
        , draggable
        , href
        , id
        , src
        , style
        )
import Html.Styled.Events exposing (onMouseEnter, onMouseLeave)
import Http
import Keyboard
import LoginRedirect
import Mouse
import Monocle.Common exposing ((=>), (<|>))
import Monocle.Optional
import Monocle.Lens
import MonocleHelpers exposing (..)
import Navigation
import NewTopBar
import NoPipeline
import Concourse.PipelineStatus as PipelineStatus exposing (PipelineStatus(..))
import Regex exposing (HowMany(All), regex, replace)
import RemoteData
import Routes
import ScreenSize
import SearchBar exposing (SearchBar(..))
import Simple.Fuzzy exposing (filter, match, root)
import Task
import Time exposing (Time)
import UserState
import Window


type alias Ports =
    { title : String -> Cmd Msg
    }


port tooltip : ( String, String ) -> Cmd msg


port tooltipHd : ( String, String ) -> Cmd msg


type alias Flags =
    { csrfToken : String
    , turbulencePath : String
    , search : String
    , highDensity : Bool
    , pipelineRunningKeyframes : String
    }


type DashboardError
    = NotAsked
    | Turbulence String
    | NoPipelines


type alias Model =
    { csrfToken : String
    , state : Result DashboardError SubState.SubState
    , turbulencePath : String
    , highDensity : Bool
    , hoveredPipeline : Maybe Models.Pipeline
    , pipelineRunningKeyframes : String
    , groups : List Group.Group
    , hoveredCliIcon : Maybe Cli.Cli
    , screenSize : ScreenSize.ScreenSize
    , version : String
    , userState : UserState.UserState
    , userMenuVisible : Bool
    , searchBar : SearchBar
    }


stateLens : Monocle.Lens.Lens Model (Result DashboardError SubState.SubState)
stateLens =
    Monocle.Lens.Lens .state (\b a -> { a | state = b })


substateOptional : Monocle.Optional.Optional Model SubState.SubState
substateOptional =
    Monocle.Optional.Optional (.state >> Result.toMaybe) (\s m -> { m | state = Ok s })


init : Ports -> Flags -> ( Model, Cmd Msg )
init ports flags =
    let
        searchBar =
            if flags.highDensity then
                Invisible
            else
                Expanded
                    { query = flags.search
                    , selectionMade = False
                    , showAutocomplete = False
                    , selection = 0
                    }
    in
        ( { state = Err NotAsked
          , csrfToken = flags.csrfToken
          , turbulencePath = flags.turbulencePath
          , highDensity = flags.highDensity
          , hoveredPipeline = Nothing
          , pipelineRunningKeyframes = flags.pipelineRunningKeyframes
          , groups = []
          , hoveredCliIcon = Nothing
          , screenSize = ScreenSize.Desktop
          , version = ""
          , userState = UserState.UserStateUnknown
          , userMenuVisible = False
          , searchBar = searchBar
          }
        , Cmd.batch
            [ fetchData
            , Group.pinTeamNames Group.stickyHeaderConfig
            , ports.title <| "Dashboard" ++ " - "
            , Task.perform ScreenResized Window.size
            ]
        )


substateLens : Monocle.Lens.Lens Model (Maybe SubState.SubState)
substateLens =
    Monocle.Lens.Lens (.state >> Result.toMaybe)
        (\mss model -> Maybe.map (\ss -> { model | state = Ok ss }) mss |> Maybe.withDefault model)


noop : Model -> ( Model, Cmd msg )
noop model =
    ( model, Cmd.none )


substate : String -> Bool -> ( Time.Time, APIData.APIData ) -> Result DashboardError SubState.SubState
substate csrfToken highDensity ( now, apiData ) =
    apiData.pipelines
        |> List.head
        |> Maybe.map
            (always
                { details =
                    if highDensity then
                        Nothing
                    else
                        Just
                            { now = now
                            , dragState = Group.NotDragging
                            , dropState = Group.NotDropping
                            , showHelp = False
                            }
                , hideFooter = False
                , hideFooterCounter = 0
                , csrfToken = csrfToken
                }
            )
        |> Result.fromMaybe (NoPipelines)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        APIDataFetched remoteData ->
            (case remoteData of
                RemoteData.NotAsked ->
                    model |> stateLens.set (Err NotAsked)

                RemoteData.Loading ->
                    model |> stateLens.set (Err NotAsked)

                RemoteData.Failure _ ->
                    model |> stateLens.set (Err (Turbulence model.turbulencePath))

                RemoteData.Success ( now, apiData ) ->
                    let
                        newModel =
                            model
                                |> Monocle.Lens.modify stateLens
                                    (Result.map
                                        (.set (SubState.detailsOptional =|> Details.nowLens) now >> Ok)
                                        >> Result.withDefault (substate model.csrfToken model.highDensity ( now, apiData ))
                                    )
                    in
                        { newModel
                            | groups = Group.groups apiData
                            , version = apiData.version
                            , userState =
                                case apiData.user of
                                    Just u ->
                                        UserState.UserStateLoggedIn u

                                    Nothing ->
                                        UserState.UserStateLoggedOut
                        }
            )
                |> noop

        ClockTick now ->
            model
                |> Monocle.Optional.modify substateOptional (SubState.tick now)
                |> noop

        AutoRefresh _ ->
            ( model
            , fetchData
            )

        KeyPressed keycode ->
            handleKeyPressed (Char.fromCode keycode) model

        ShowFooter ->
            model
                |> Monocle.Optional.modify substateOptional SubState.showFooter
                |> noop

        TogglePipelinePaused pipeline ->
            ( model, togglePipelinePaused { pipeline = pipeline, csrfToken = model.csrfToken } )

        DragStart teamName index ->
            model
                |> Monocle.Optional.modify
                    (substateOptional => SubState.detailsOptional)
                    ((Details.dragStateLens |> .set) <| Group.Dragging teamName index)
                |> noop

        DragOver teamName index ->
            model
                |> Monocle.Optional.modify
                    (substateOptional => SubState.detailsOptional)
                    ((Details.dropStateLens |> .set) <| Group.Dropping index)
                |> noop

        TooltipHd pipelineName teamName ->
            ( model, tooltipHd ( pipelineName, teamName ) )

        Tooltip pipelineName teamName ->
            ( model, tooltip ( pipelineName, teamName ) )

        DragEnd ->
            let
                updatePipelines : ( Group.PipelineIndex, Group.PipelineIndex ) -> Group.Group -> ( Group.Group, Cmd Msg )
                updatePipelines ( dragIndex, dropIndex ) group =
                    let
                        newGroup =
                            Group.shiftPipelines dragIndex dropIndex group
                    in
                        ( newGroup, orderPipelines newGroup.teamName newGroup.pipelines model.csrfToken )

                dragDropOptional : Monocle.Optional.Optional Model ( Group.DragState, Group.DropState )
                dragDropOptional =
                    substateOptional
                        => SubState.detailsOptional
                        =|> Monocle.Lens.tuple (Details.dragStateLens) (Details.dropStateLens)

                dragDropIndexOptional : Monocle.Optional.Optional Model ( Group.PipelineIndex, Group.PipelineIndex )
                dragDropIndexOptional =
                    dragDropOptional
                        => Monocle.Optional.zip
                            Group.dragIndexOptional
                            Group.dropIndexOptional

                groupsLens : Monocle.Lens.Lens Model (List Group.Group)
                groupsLens =
                    Monocle.Lens.Lens .groups (\b a -> { a | groups = b })

                groupOptional : Monocle.Optional.Optional Model Group.Group
                groupOptional =
                    (substateOptional
                        => SubState.detailsOptional
                        =|> Details.dragStateLens
                        => Group.teamNameOptional
                    )
                        >>= (\teamName ->
                                groupsLens
                                    <|= Group.findGroupOptional teamName
                            )

                bigOptional : Monocle.Optional.Optional Model ( ( Group.PipelineIndex, Group.PipelineIndex ), Group.Group )
                bigOptional =
                    Monocle.Optional.tuple
                        dragDropIndexOptional
                        groupOptional
            in
                model
                    |> modifyWithEffect bigOptional
                        (\( t, g ) ->
                            let
                                ( newG, msg ) =
                                    updatePipelines t g
                            in
                                ( ( t, newG ), msg )
                        )
                    |> Tuple.mapFirst (dragDropOptional.set ( Group.NotDragging, Group.NotDropping ))

        PipelineButtonHover state ->
            ( { model | hoveredPipeline = state }, Cmd.none )

        CliHover state ->
            ( { model | hoveredCliIcon = state }, Cmd.none )

        FilterMsg query ->
            let
                newModel =
                    case model.searchBar of
                        Expanded r ->
                            { model | searchBar = Expanded { r | query = query } }

                        _ ->
                            model
            in
                ( newModel
                , Cmd.batch
                    [ Task.attempt (always Noop) (Dom.focus "search-input-field")
                    , Navigation.modifyUrl (NewTopBar.queryStringFromSearch query)
                    ]
                )

        LogIn ->
            ( model
            , LoginRedirect.requestLoginRedirect ""
            )

        LogOut ->
            ( { model | state = Err NotAsked }, Cmd.batch [ NewTopBar.logOut, fetchData ] )

        LoggedOut (Ok ()) ->
            let
                redirectUrl =
                    case model.searchBar of
                        Invisible ->
                            Routes.dashboardHdRoute

                        _ ->
                            Routes.dashboardRoute
            in
                ( { model
                    | userState = UserState.UserStateLoggedOut
                    , userMenuVisible = False
                  }
                , Navigation.newUrl redirectUrl
                )

        LoggedOut (Err err) ->
            flip always (Debug.log "failed to log out" err) <|
                ( model, Cmd.none )

        ToggleUserMenu ->
            ( { model | userMenuVisible = not model.userMenuVisible }, Cmd.none )

        FocusMsg ->
            let
                newModel =
                    case model.searchBar of
                        Expanded r ->
                            { model | searchBar = Expanded { r | showAutocomplete = True } }

                        _ ->
                            model
            in
                ( newModel, Cmd.none )

        BlurMsg ->
            let
                newModel =
                    case model.searchBar of
                        Expanded r ->
                            case model.screenSize of
                                ScreenSize.Mobile ->
                                    if String.isEmpty r.query then
                                        { model | searchBar = Collapsed }
                                    else
                                        { model | searchBar = Expanded { r | showAutocomplete = False, selectionMade = False, selection = 0 } }

                                ScreenSize.Desktop ->
                                    { model | searchBar = Expanded { r | showAutocomplete = False, selectionMade = False, selection = 0 } }

                                ScreenSize.BigDesktop ->
                                    { model | searchBar = Expanded { r | showAutocomplete = False, selectionMade = False, selection = 0 } }

                        _ ->
                            model
            in
                ( newModel, Cmd.none )

        SelectMsg index ->
            let
                newModel =
                    case model.searchBar of
                        Expanded r ->
                            { model | searchBar = Expanded { r | selectionMade = True, selection = index + 1 } }

                        _ ->
                            model
            in
                ( newModel, Cmd.none )

        KeyDowns keycode ->
            case model.searchBar of
                Expanded r ->
                    if not r.showAutocomplete then
                        ( { model | searchBar = Expanded { r | selectionMade = False, selection = 0 } }, Cmd.none )
                    else
                        case keycode of
                            -- enter key
                            13 ->
                                if not r.selectionMade then
                                    ( model, Cmd.none )
                                else
                                    let
                                        options =
                                            Array.fromList (NewTopBar.autocompleteOptions { query = r.query, groups = model.groups })

                                        index =
                                            (r.selection - 1) % Array.length options

                                        selectedItem =
                                            case Array.get index options of
                                                Nothing ->
                                                    r.query

                                                Just item ->
                                                    item
                                    in
                                        ( { model | searchBar = Expanded { r | selectionMade = False, selection = 0, query = selectedItem } }
                                        , Cmd.none
                                        )

                            -- up arrow
                            38 ->
                                ( { model | searchBar = Expanded { r | selectionMade = True, selection = r.selection - 1 } }, Cmd.none )

                            -- down arrow
                            40 ->
                                ( { model | searchBar = Expanded { r | selectionMade = True, selection = r.selection + 1 } }, Cmd.none )

                            -- escape key
                            27 ->
                                ( model, Task.attempt (always Noop) (Dom.blur "search-input-field") )

                            _ ->
                                ( { model | searchBar = Expanded { r | selectionMade = False, selection = 0 } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShowSearchInput ->
            NewTopBar.showSearchInput model

        ScreenResized size ->
            let
                newSize =
                    ScreenSize.fromWindowSize size
            in
                ( { model
                    | screenSize = newSize
                    , searchBar =
                        SearchBar.screenSizeChanged
                            { oldSize = model.screenSize
                            , newSize = newSize
                            }
                            model.searchBar
                  }
                , Cmd.none
                )


orderPipelines : String -> List Models.Pipeline -> Concourse.CSRFToken -> Cmd Msg
orderPipelines teamName pipelines csrfToken =
    Task.attempt (always Noop) <|
        Concourse.Pipeline.order
            teamName
            (List.map .name pipelines)
            csrfToken



-- TODO this seems obsessed with pipelines. shouldn't be the dashboard's business


togglePipelinePaused : { pipeline : Models.Pipeline, csrfToken : Concourse.CSRFToken } -> Cmd Msg
togglePipelinePaused { pipeline, csrfToken } =
    Task.attempt (always Noop) <|
        if pipeline.status == PipelineStatus.PipelineStatusPaused then
            Concourse.Pipeline.unpause pipeline.teamName pipeline.name csrfToken
        else
            Concourse.Pipeline.pause pipeline.teamName pipeline.name csrfToken


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second ClockTick
        , Time.every (5 * Time.second) AutoRefresh
        , Mouse.moves (\_ -> ShowFooter)
        , Mouse.clicks (\_ -> ShowFooter)
        , Keyboard.presses KeyPressed
        , Keyboard.downs KeyDowns
        , Window.resizes Msgs.ScreenResized
        ]


view : Model -> Html Msg
view model =
    Html.div [ class "page" ]
        [ NewTopBar.view model
        , dashboardView model
        ]


dashboardView : Model -> Html Msg
dashboardView model =
    let
        mainContent =
            case model.state of
                Err NotAsked ->
                    [ Html.text "" ]

                Err (Turbulence path) ->
                    [ turbulenceView path ]

                Err NoPipelines ->
                    [ Html.div [ class "dashboard-no-content", css [ Css.height (Css.pct 100) ] ] [ (Html.map (always Noop) << Html.fromUnstyled) NoPipeline.view ] ]

                Ok substate ->
                    [ Html.div
                        [ class "dashboard-content" ]
                        (pipelinesView
                            { groups = model.groups
                            , substate = substate
                            , query = NewTopBar.query model
                            , hoveredPipeline = model.hoveredPipeline
                            , pipelineRunningKeyframes = model.pipelineRunningKeyframes
                            , userState = model.userState
                            }
                        )
                    ]
                        ++ footerView
                            { substate = substate
                            , hoveredCliIcon = model.hoveredCliIcon
                            , screenSize = model.screenSize
                            , version = model.version
                            }
    in
        Html.div
            [ classList [ ( .pageBodyClass Group.stickyHeaderConfig, True ), ( "dashboard-hd", model.highDensity ) ] ]
            mainContent


noResultsView : String -> Html Msg
noResultsView query =
    let
        boldedQuery =
            Html.span [ class "monospace-bold" ] [ Html.text query ]
    in
        Html.div
            [ class <| .pageBodyClass Group.stickyHeaderConfig ]
            [ Html.div [ class "dashboard-content " ]
                [ Html.div
                    [ class <| .sectionClass Group.stickyHeaderConfig ]
                    [ Html.div [ class "no-results" ]
                        [ Html.text "No results for "
                        , boldedQuery
                        , Html.text " matched your search."
                        ]
                    ]
                ]
            ]


helpView : Details.Details -> Html Msg
helpView details =
    Html.div
        [ classList
            [ ( "keyboard-help", True )
            , ( "hidden", not details.showHelp )
            ]
        ]
        [ Html.div [ class "help-title" ] [ Html.text "keyboard shortcuts" ]
        , Html.div [ class "help-line" ] [ Html.div [ class "keys" ] [ Html.span [ class "key" ] [ Html.text "/" ] ], Html.text "search" ]
        , Html.div [ class "help-line" ] [ Html.div [ class "keys" ] [ Html.span [ class "key" ] [ Html.text "?" ] ], Html.text "hide/show help" ]
        ]


toggleView : Bool -> Html Msg
toggleView highDensity =
    let
        route =
            if highDensity then
                Routes.dashboardRoute
            else
                Routes.dashboardHdRoute
    in
        Html.a
            [ style Styles.highDensityToggle
            , href route
            , attribute "aria-label" "Toggle high-density view"
            ]
            [ Html.div [ style <| Styles.highDensityIcon highDensity ] []
            , Html.text "high-density"
            ]


footerView :
    { substate : SubState.SubState
    , hoveredCliIcon : Maybe Cli.Cli
    , screenSize : ScreenSize.ScreenSize
    , version : String
    }
    -> List (Html Msg)
footerView { substate, hoveredCliIcon, screenSize, version } =
    let
        showHelp =
            substate.details |> Maybe.map .showHelp |> Maybe.withDefault False
    in
        if showHelp then
            [ keyboardHelpView ]
        else if not substate.hideFooter then
            [ infoView
                { substate = substate
                , hoveredCliIcon = hoveredCliIcon
                , screenSize = screenSize
                , version = version
                }
            ]
        else
            []


legendItem : PipelineStatus -> Html Msg
legendItem status =
    Html.div [ style Styles.legendItem ]
        [ Html.div
            [ style <| Styles.pipelineStatusIcon status ]
            []
        , Html.div [ style [ ( "width", "10px" ) ] ] []
        , Html.text <| PipelineStatus.show status
        ]


infoView :
    { substate : SubState.SubState
    , hoveredCliIcon : Maybe Cli.Cli
    , screenSize : ScreenSize.ScreenSize
    , version : String
    }
    -> Html Msg
infoView { substate, hoveredCliIcon, screenSize, version } =
    let
        legendSeparator : ScreenSize.ScreenSize -> List (Html Msg)
        legendSeparator screenSize =
            case screenSize of
                ScreenSize.Mobile ->
                    []

                ScreenSize.Desktop ->
                    [ Html.div
                        [ style Styles.legendSeparator ]
                        [ Html.text "|" ]
                    ]

                ScreenSize.BigDesktop ->
                    [ Html.div
                        [ style Styles.legendSeparator ]
                        [ Html.text "|" ]
                    ]

        cliIcon : Cli.Cli -> Maybe Cli.Cli -> Html Msg
        cliIcon cli hoveredCliIcon =
            let
                ( cliName, ariaText, icon ) =
                    case cli of
                        Cli.OSX ->
                            ( "osx", "OS X", "apple" )

                        Cli.Windows ->
                            ( "windows", "Windows", "windows" )

                        Cli.Linux ->
                            ( "linux", "Linux", "linux" )
            in
                Html.a
                    [ href (Cli.downloadUrl "amd64" cliName)
                    , attribute "aria-label" <| "Download " ++ ariaText ++ " CLI"
                    , style <| Styles.infoCliIcon (hoveredCliIcon == Just cli)
                    , id <| "cli-" ++ cliName
                    , onMouseEnter <| CliHover <| Just cli
                    , onMouseLeave <| CliHover Nothing
                    ]
                    [ Html.i [ class <| "fa fa-" ++ icon ] [] ]
    in
        Html.div
            [ id "dashboard-info"
            , style <| Styles.infoBar screenSize
            ]
            [ Html.div
                [ id "legend"
                , style Styles.legend
                ]
              <|
                List.map legendItem
                    [ PipelineStatusPending False
                    , PipelineStatusPaused
                    ]
                    ++ [ Html.div [ style Styles.legendItem ]
                            [ Html.div
                                [ style
                                    [ ( "background-image", "url(public/images/ic_running_legend.svg)" )
                                    , ( "height", "20px" )
                                    , ( "width", "20px" )
                                    , ( "background-repeat", "no-repeat" )
                                    , ( "background-position", "50% 50%" )
                                    ]
                                ]
                                []
                            , Html.div [ style [ ( "width", "10px" ) ] ] []
                            , Html.text "running"
                            ]
                       ]
                    ++ List.map legendItem
                        [ PipelineStatusFailed PipelineStatus.Running
                        , PipelineStatusErrored PipelineStatus.Running
                        , PipelineStatusAborted PipelineStatus.Running
                        , PipelineStatusSucceeded PipelineStatus.Running
                        ]
                    ++ legendSeparator screenSize
                    ++ [ toggleView (substate.details == Nothing) ]
            , Html.div [ id "concourse-info", style Styles.info ]
                [ Html.div [ style Styles.infoItem ]
                    [ Html.text <| "version: v" ++ version ]
                , Html.div [ style Styles.infoItem ]
                    [ Html.span
                        [ style [ ( "margin-right", "10px" ) ] ]
                        [ Html.text "cli: " ]
                    , cliIcon Cli.OSX hoveredCliIcon
                    , cliIcon Cli.Windows hoveredCliIcon
                    , cliIcon Cli.Linux hoveredCliIcon
                    ]
                ]
            ]


keyboardHelpView : Html Msg
keyboardHelpView =
    Html.div
        [ classList
            [ ( "keyboard-help", True )
            ]
        ]
        [ Html.div [ class "help-title" ] [ Html.text "keyboard shortcuts" ]
        , Html.div [ class "help-line" ] [ Html.div [ class "keys" ] [ Html.span [ class "key" ] [ Html.text "/" ] ], Html.text "search" ]
        , Html.div [ class "help-line" ] [ Html.div [ class "keys" ] [ Html.span [ class "key" ] [ Html.text "?" ] ], Html.text "hide/show help" ]
        ]


turbulenceView : String -> Html Msg
turbulenceView path =
    Html.div
        [ class "error-message" ]
        [ Html.div [ class "message" ]
            [ Html.img [ src path, class "seatbelt" ] []
            , Html.p [] [ Html.text "experiencing turbulence" ]
            , Html.p [ class "explanation" ] []
            ]
        ]


pipelinesView :
    { groups : List Group.Group
    , substate : SubState.SubState
    , hoveredPipeline : Maybe Models.Pipeline
    , pipelineRunningKeyframes : String
    , query : String
    , userState : UserState.UserState
    }
    -> List (Html Msg)
pipelinesView { groups, substate, hoveredPipeline, pipelineRunningKeyframes, query, userState } =
    let
        filteredGroups =
            groups |> filter query |> List.sortWith Group.ordering

        groupsToDisplay =
            if List.all (String.startsWith "team:") (filterTerms query) then
                filteredGroups
            else
                filteredGroups |> List.filter (.pipelines >> List.isEmpty >> not)

        groupViews =
            case substate.details of
                Just details ->
                    groupsToDisplay
                        |> List.map
                            (Group.view
                                { dragState = details.dragState
                                , dropState = details.dropState
                                , now = details.now
                                , hoveredPipeline = hoveredPipeline
                                , pipelineRunningKeyframes = pipelineRunningKeyframes
                                }
                            )

                Nothing ->
                    groupsToDisplay
                        |> List.map (Group.hdView pipelineRunningKeyframes)
    in
        if List.isEmpty groupViews then
            [ noResultsView (toString query) ]
        else
            List.map Html.fromUnstyled groupViews


handleKeyPressed : Char -> Model -> ( Model, Cmd Msg )
handleKeyPressed key model =
    case key of
        '/' ->
            ( model, Task.attempt (always Noop) (Dom.focus "search-input-field") )

        '?' ->
            model
                |> Monocle.Optional.modify (substateOptional => SubState.detailsOptional) Details.toggleHelp
                |> noop

        _ ->
            update ShowFooter model


fetchData : Cmd Msg
fetchData =
    APIData.remoteData
        |> Task.map2 (,) Time.now
        |> RemoteData.asCmd
        |> Cmd.map APIDataFetched


remoteUser : APIData.APIData -> Task.Task Http.Error ( APIData.APIData, Maybe Concourse.User )
remoteUser d =
    Concourse.User.fetchUser
        |> Task.map ((,) d << Just)
        |> Task.onError (always <| Task.succeed <| ( d, Nothing ))


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform ClockTick Time.now


filterTerms : String -> List String
filterTerms =
    replace All (regex "team:\\s*") (\_ -> "team:")
        >> replace All (regex "status:\\s*") (\_ -> "status:")
        >> String.words
        >> List.filter (not << String.isEmpty)


filter : String -> List Group.Group -> List Group.Group
filter =
    filterTerms >> flip (List.foldl filterGroupsByTerm)


filterPipelinesByTerm : String -> Group.Group -> Group.Group
filterPipelinesByTerm term ({ pipelines } as group) =
    let
        searchStatus =
            String.startsWith "status:" term

        statusSearchTerm =
            if searchStatus then
                String.dropLeft 7 term
            else
                term

        filterByStatus =
            fuzzySearch (.status >> Concourse.PipelineStatus.show) statusSearchTerm pipelines
    in
        { group
            | pipelines =
                if searchStatus then
                    filterByStatus
                else
                    fuzzySearch .name term pipelines
        }


filterGroupsByTerm : String -> List Group.Group -> List Group.Group
filterGroupsByTerm term groups =
    let
        searchTeams =
            String.startsWith "team:" term

        teamSearchTerm =
            if searchTeams then
                String.dropLeft 5 term
            else
                term
    in
        if searchTeams then
            fuzzySearch .teamName teamSearchTerm groups
        else
            groups |> List.map (filterPipelinesByTerm term)


fuzzySearch : (a -> String) -> String -> List a -> List a
fuzzySearch map needle records =
    let
        negateSearch =
            String.startsWith "-" needle
    in
        if negateSearch then
            List.filter (not << Simple.Fuzzy.match needle << map) records
        else
            List.filter (Simple.Fuzzy.match needle << map) records
