module Settings exposing
    ( copyPublicKey
    , editConfig
    , instanceConfigDialogId
    , openInstanceConfig
    , openUserRepo
    , saveConfigForm
    , saveInstanceConfig
    , saveUserRepo
    , setEditAsToml
    , userRepoDialogId
    )

import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..))
import Browser.Navigation as Nav
import ConfigForm exposing (PathStep)
import Extra.Http as Http
import Flow exposing (Flow)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Core exposing (ConfigForm, ConfigNode, Model, UserRepoInfo, initSettingsState)
import Model.Lenses exposing (settings, settingsConfig, settingsEditAsToml, settingsError, settingsForm, settingsRepoBranch, settingsRepoUrl, settingsSaving, userRepoInfo)
import Result.Extra as Result


userRepoDialogId : String
userRepoDialogId =
    "user-repo-dialog"


instanceConfigDialogId : String
instanceConfigDialogId =
    "instance-config-dialog"


openUserRepo : Flow Model ()
openUserRepo =
    Flow.setAll settings initSettingsState
        |> Flow.seq (Actions.openDialog userRepoDialogId)
        |> Flow.seq Api.fetchUserRepoInfo
        |> Flow.andThen (Result.unpack showError editUserRepo)


editUserRepo : UserRepoInfo -> Flow Model ()
editUserRepo info =
    Flow.setAll userRepoInfo (Success info)
        |> Flow.seq (Flow.setAll settingsRepoUrl info.url)
        |> Flow.seq (Flow.setAll settingsRepoBranch info.branch)


openInstanceConfig : Flow Model ()
openInstanceConfig =
    Flow.setAll settings { initSettingsState | form = Loading Nothing }
        |> Flow.seq (Actions.openDialog instanceConfigDialogId)
        |> Flow.seq Api.fetchConfigForm
        |> Flow.andThen
            (\result ->
                Flow.setAll settingsForm (ApiData.fromResult result)
                    |> Flow.seq (setEditAsToml (Result.isErr result))
            )


setEditAsToml : Bool -> Flow Model ()
setEditAsToml enabled =
    Flow.setAll settingsEditAsToml enabled
        |> Flow.seq (Flow.when enabled fetchInstanceConfig)


fetchInstanceConfig : Flow Model ()
fetchInstanceConfig =
    Flow.forAll settingsConfig
        (\config ->
            case config of
                NotAsked ->
                    reloadInstanceConfig

                Error _ ->
                    reloadInstanceConfig

                _ ->
                    Flow.pure ()
        )


reloadInstanceConfig : Flow Model ()
reloadInstanceConfig =
    Flow.setAll settingsConfig (Loading Nothing)
        |> Flow.seq Api.fetchInstanceConfig
        |> Flow.andThen (ApiData.fromResult >> Flow.setAll settingsConfig)


editConfig : List PathStep -> (ConfigNode -> ConfigNode) -> Flow Model ()
editConfig path change =
    Flow.over (settingsForm << ApiData.success) (\form -> { form | values = ConfigForm.update path change form.values })


copyPublicKey : String -> Flow Model ()
copyPublicKey =
    Actions.callJs "copyToClipboard" Encode.string (Decode.succeed ())
        >> Flow.seq (Actions.addToast True "Deploy key copied to clipboard")


saveUserRepo : String -> String -> Flow Model ()
saveUserRepo url =
    Api.updateUserRepo url >> applyAndReload


saveInstanceConfig : String -> Flow Model ()
saveInstanceConfig =
    Api.updateInstanceConfig >> applyAndReload


saveConfigForm : ConfigForm -> Flow Model ()
saveConfigForm =
    ConfigForm.encode >> Api.updateConfigForm >> applyAndReload


applyAndReload : Flow Model (Result Http.Error ()) -> Flow Model ()
applyAndReload save =
    Flow.setAll settingsError Nothing
        |> Flow.seq save
        |> Flow.andThen (Result.unpack showError (always (Flow.lift Nav.reload)))
        |> Flow.locking settingsSaving


showError : Http.Error -> Flow Model ()
showError =
    Http.errorMessage >> Just >> Flow.setAll settingsError
