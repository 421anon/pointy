module View.Settings exposing (viewInstanceConfigDialog, viewUserRepoDialog)

import Accessors exposing (A_Lens, get, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, disabled, id, placeholder, readonly, spellcheck, title, type_, value)
import Html.Events as Events
import Html.Extra as Html
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model)
import Model.Lenses exposing (settingsConfig, settingsRepoBranch, settingsRepoUrl, userRepoInfo)
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
        config =
            get settingsConfig model
    in
    viewDialog Settings.instanceConfigDialogId
        "Instance configuration"
        model
        [ Html.p [ class "settings-hint" ]
            [ Html.text "The backend's config.toml. Saving checks it, applies it and reloads the page." ]
        , ApiData.foldVisible Html.nothing
            (always (Html.p [ class "settings-hint" ] [ Html.text "Loading..." ]))
            viewConfigEditor
            (Http.errorMessage >> viewError)
            config
        ]
        (ApiData.toMaybe config |> Maybe.map Settings.saveInstanceConfig)


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
