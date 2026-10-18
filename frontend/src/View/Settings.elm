module View.Settings exposing (viewInstanceConfigDialog, viewUserRepoDialog)

import Accessors exposing (A_Lens, get, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import ConfigForm exposing (PathStep(..))
import Dict
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, checked, class, disabled, id, placeholder, readonly, required, rows, selected, spellcheck, step, title, type_, value)
import Html.Events as Events
import Html.Extra as Html
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (ConfigField, ConfigForm, ConfigKind(..), ConfigNode(..), ConfigValues, Model)
import Model.Lenses exposing (settingsConfig, settingsEditAsToml, settingsRepoBranch, settingsRepoUrl, userRepoInfo)
import Settings
import String.Extra as String
import View.Icons exposing (icon)


viewUserRepoDialog : Model -> Html (Flow Model ())
viewUserRepoDialog model =
    let
        url =
            get settingsRepoUrl model

        branch =
            get settingsRepoBranch model
    in
    viewDialog Settings.userRepoDialogId
        "User repository"
        model
        [ Html.p [ class "settings-hint" ]
            [ Html.text "Pointy reads and writes the repository over SSH with this deploy key. On GitHub, add it under Settings → Deploy keys and tick Allow write access. On GitLab, add it under Settings → Repository → Deploy keys and tick Grant write permissions to this key." ]
        , Html.viewMaybe viewPublicKey (try (userRepoInfo << ApiData.success) model |> Maybe.andThen .publicKey)
        , viewField "SSH clone URL" "git@github.com:org/repo.git" settingsRepoUrl model
        , viewField "Branch" "main" settingsRepoBranch model
        ]
        (if List.any String.isBlank [ url, branch ] then
            Nothing

         else
            Just (Settings.saveUserRepo url branch)
        )


viewPublicKey : String -> Html (Flow Model ())
viewPublicKey key =
    Html.div [ class "settings-key" ]
        [ Html.input [ class "form-input", readonly True, value key, attribute "aria-label" "Deploy key" ] []
        , Html.button
            [ class "icon-btn"
            , type_ "button"
            , title "Copy deploy key"
            , Events.onClick (Settings.copyPublicKey key)
            ]
            [ icon True "content_copy" ]
        ]


viewField : String -> String -> A_Lens pr Model String -> Model -> Html (Flow Model ())
viewField label hint field model =
    Html.label [ class "settings-field" ]
        [ Html.text label
        , Html.input
            [ class "form-input"
            , placeholder hint
            , spellcheck False
            , value (get field model)
            , Events.onInput (Flow.setAll field)
            ]
            []
        ]


viewInstanceConfigDialog : Model -> Html (Flow Model ())
viewInstanceConfigDialog model =
    let
        state =
            Model.getSettingsState model
    in
    viewDialog Settings.instanceConfigDialogId
        "Instance configuration"
        model
        (Html.div [ class "settings-mode" ]
            [ Html.p [ class "settings-hint" ]
                [ Html.text
                    (if state.editAsToml then
                        "The backend's config.toml. Saving checks it, applies it and reloads the page."

                     else
                        "Saving rewrites config.toml without its comments, applies it and reloads the page."
                    )
                ]
            , viewModeToggle state.editAsToml
            ]
            :: (if state.editAsToml then
                    viewLoaded (List.singleton << viewConfigEditor) state.config

                else
                    viewLoaded viewConfigForm state.form
               )
        )
        (if state.editAsToml then
            ApiData.toMaybe state.config |> Maybe.map Settings.saveInstanceConfig

         else
            ApiData.toMaybe state.form |> Maybe.map Settings.saveConfigForm
        )


viewModeToggle : Bool -> Html (Flow Model ())
viewModeToggle editAsToml =
    Html.button
        [ class "btn file-view-mode-toggle"
        , type_ "button"
        , Events.onClick (Flow.setAll settingsEditAsToml (not editAsToml))
        ]
        (if editAsToml then
            [ icon True "tune", Html.text "Edit as form" ]

         else
            [ icon True "code", Html.text "Edit as TOML" ]
        )


viewLoaded : (a -> List (Html (Flow Model ()))) -> ApiData a -> List (Html (Flow Model ()))
viewLoaded viewData =
    ApiData.foldVisible []
        (always [ Html.p [ class "settings-hint" ] [ Html.text "Loading..." ] ])
        viewData
        (Http.errorMessage >> viewError >> List.singleton)


viewConfigEditor : String -> Html (Flow Model ())
viewConfigEditor contents =
    Html.textarea
        [ class "form-input form-input-textarea settings-config"
        , spellcheck False
        , value contents
        , attribute "aria-label" "Configuration"
        , Events.onInput (Success >> Flow.setAll settingsConfig)
        ]
        []


viewConfigForm : ConfigForm -> List (Html (Flow Model ()))
viewConfigForm form =
    viewConfigFields (ConfigForm.providerNames form.values) [] [] form.fields form.values


viewConfigFields : List String -> List PathStep -> List String -> List ConfigField -> ConfigValues -> List (Html (Flow Model ()))
viewConfigFields providers path names fields values =
    List.filterMap
        (\field ->
            Dict.get field.key values
                |> Maybe.map (viewConfigField providers (path ++ [ Key field.key ]) (names ++ [ field.key ]) field)
        )
        fields


viewConfigField : List String -> List PathStep -> List String -> ConfigField -> ConfigNode -> Html (Flow Model ())
viewConfigField providers path names field node =
    case ( field.kind, node ) of
        ( TableKind fields, ConfigTable table ) ->
            Html.fieldset [ class "config-section" ]
                (Html.legend []
                    [ if ConfigForm.isOptional field then
                        Html.label [ class "config-toggle" ]
                            [ Html.input
                                [ type_ "checkbox"
                                , checked (Maybe.isJust table)
                                , Events.stopPropagationOn "click" (Decode.map (\enabled -> ( Settings.editConfig path (ConfigForm.toggleTable fields enabled), True )) Events.targetChecked)
                                ]
                                []
                            , Html.text (sectionName names)
                            ]

                      else
                        Html.text (sectionName names)
                    ]
                    :: viewDoc field.doc
                    :: Maybe.unwrap [] (viewConfigFields providers path names fields) table
                )

        ( TablesKind fields, ConfigTables entries ) ->
            Html.fieldset [ class "config-section" ]
                (Html.legend [] [ Html.text ("[" ++ sectionName names ++ "]") ]
                    :: viewDoc field.doc
                    :: List.indexedMap (viewConfigEntry providers path names fields) entries
                    ++ [ Html.button
                            [ class "btn config-add"
                            , type_ "button"
                            , Events.onClick (Settings.editConfig path (ConfigForm.addEntry fields))
                            ]
                            [ icon True "add", Html.text "Add" ]
                       ]
                )

        ( kind, ConfigInput input ) ->
            Html.label [ class "settings-field" ]
                [ Html.span [ class "config-key" ] [ Html.text field.key ]
                , viewConfigInput providers field kind input (\text -> Settings.editConfig path (always (ConfigInput text)))
                , viewDoc (inputDoc kind field.doc)
                ]

        _ ->
            Html.nothing


viewConfigEntry : List String -> List PathStep -> List String -> List ConfigField -> Int -> ConfigValues -> Html (Flow Model ())
viewConfigEntry providers path names fields index entry =
    Html.div [ class "config-entry" ]
        (Html.button
            [ class "icon-btn config-remove"
            , type_ "button"
            , title "Remove"
            , Events.onClick (Settings.editConfig path (ConfigForm.removeEntry index))
            ]
            [ icon True "delete" ]
            :: viewConfigFields providers (path ++ [ Index index ]) names fields entry
        )


viewConfigInput : List String -> ConfigField -> ConfigKind -> String -> (String -> Flow Model ()) -> Html (Flow Model ())
viewConfigInput providers field kind input set =
    let
        unset =
            if ConfigForm.isOptional field then
                [ ( "", "Not set" ) ]

            else if String.isEmpty input then
                [ ( "", "Choose" ) ]

            else
                []
    in
    case kind of
        BoolKind ->
            viewSelect set input (unset ++ [ ( "true", "Yes" ), ( "false", "No" ) ])

        EnumKind values ->
            viewSelect set input (unset ++ List.map (\choice -> ( choice, choice )) values)

        ProviderKind ->
            viewSelect set
                input
                (( "", "None" )
                    :: List.map (\name -> ( name, name )) providers
                    ++ (if String.isEmpty input || List.member input providers then
                            []

                        else
                            [ ( input, input ++ " (not in providers)" ) ]
                       )
                )

        IntegerKind ->
            viewNumberInput "1" field input set

        NumberKind ->
            viewNumberInput "any" field input set

        TextKind ->
            Html.input [ class "form-input", spellcheck False, required field.required, value input, Events.onInput set ] []

        _ ->
            Html.textarea
                [ class "form-input form-input-textarea config-textarea"
                , spellcheck False
                , rows (max 2 (List.length (String.lines input)))
                , value input
                , Events.onInput set
                ]
                []


viewNumberInput : String -> ConfigField -> String -> (String -> Flow Model ()) -> Html (Flow Model ())
viewNumberInput increment field input set =
    Html.input [ class "form-input", type_ "number", step increment, required field.required, value input, Events.onInput set ] []


viewSelect : (String -> Flow Model ()) -> String -> List ( String, String ) -> Html (Flow Model ())
viewSelect set current choices =
    Html.select [ class "form-input", Events.onInput set ]
        (List.map (\( choice, label ) -> Html.option [ value choice, selected (choice == current) ] [ Html.text label ]) choices)


inputDoc : ConfigKind -> Maybe String -> Maybe String
inputDoc kind doc =
    case kind of
        ListKind ->
            Just (Maybe.unwrap "" (\text -> text ++ " ") doc ++ "One per line.")

        MapKind ->
            Just (Maybe.unwrap "" (\text -> text ++ " ") doc ++ "One key = value per line.")

        _ ->
            doc


viewDoc : Maybe String -> Html msg
viewDoc =
    Html.viewMaybe (\doc -> Html.span [ class "settings-hint" ] [ Html.text doc ])


sectionName : List String -> String
sectionName names =
    "[" ++ String.join "." names ++ "]"


viewError : String -> Html msg
viewError error =
    Html.pre [ class "settings-error" ] [ Html.text error ]


viewDialog : String -> String -> Model -> List (Html (Flow Model ())) -> Maybe (Flow Model ()) -> Html (Flow Model ())
viewDialog dialogId heading model body onSave =
    let
        state =
            Model.getSettingsState model
    in
    Html.node "dialog"
        [ id dialogId
        , class "dialog settings-dialog"
        , Events.onClick (Actions.closeDialog dialogId)
        ]
        [ Html.div
            [ class "dialog-content settings-content"
            , Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
            ]
            (Html.div [ class "settings-header" ]
                [ Html.span [ class "dialog-title" ] [ Html.text heading ]
                , Html.button
                    [ class "icon-btn"
                    , type_ "button"
                    , title "Close"
                    , Events.onClick (Actions.closeDialog dialogId)
                    ]
                    [ icon True "close" ]
                ]
                :: body
                ++ [ Html.viewMaybe viewError state.error
                   , Html.div [ class "dialog-actions" ]
                        [ Html.button
                            [ class "btn"
                            , type_ "button"
                            , disabled (Maybe.isNothing onSave || state.saving)
                            , Events.onClick (Maybe.withDefault Flow.none onSave)
                            ]
                            [ Html.text
                                (if state.saving then
                                    "Applying..."

                                 else
                                    "Save and apply"
                                )
                            ]
                        ]
                   ]
            )
        ]
