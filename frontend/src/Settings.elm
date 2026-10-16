module Settings exposing
    ( copyPublicKey
    , instanceConfigDialogId
    , openInstanceConfig
    , openUserRepo
    , saveInstanceConfig
    , saveUserRepo
    , userRepoDialogId
    )

import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..))
import Browser.Navigation as Nav
import Extra.Http as Http
import Flow exposing (Flow)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Core exposing (Model, UserRepoInfo, initSettingsState)
import Model.Lenses exposing (settings, settingsConfig, settingsError, settingsRepoBranch, settingsRepoUrl, settingsSaving, userRepoInfo)
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
    Flow.setAll settings { initSettingsState | config = Loading Nothing }
        |> Flow.seq (Actions.openDialog instanceConfigDialogId)
        |> Flow.seq Api.fetchInstanceConfig
        |> Flow.andThen (ApiData.fromResult >> Flow.setAll settingsConfig)


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


applyAndReload : Flow Model (Result Http.Error ()) -> Flow Model ()
applyAndReload save =
    Flow.setAll settingsError Nothing
        |> Flow.seq save
        |> Flow.andThen (Result.unpack showError (always (Flow.lift Nav.reload)))
        |> Flow.locking settingsSaving


showError : Http.Error -> Flow Model ()
showError =
    Http.errorMessage >> Just >> Flow.setAll settingsError
