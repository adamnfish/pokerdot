module KeyboardShortcuts exposing (keyboardShortcuts)

import Keyboard exposing (Key(..))
import Model exposing (EditBlindsSettings(..), Model, OverlayUI(..), UI(..), editBlindsSettingsFromSmallBlindAndTimerStatus)



-- TODO: scroll to top when shortcuts open an overlay


keyboardShortcuts : Model -> List Key -> Model
keyboardShortcuts model keys =
    (toggleHelp keys model >> toggleEditBlinds keys model >> togglePeek keys model >> cancel keys model) model


closeOverlay : Model -> Model
closeOverlay =
    \m -> { m | overlayUi = NoOverlay }


toggleHelp : List Key -> Model -> (Model -> Model)
toggleHelp keys model =
    if List.member (Character "?") keys then
        if model.overlayUi == NoOverlay then
            \m -> { m | overlayUi = HelpOverlay }

        else if model.overlayUi == HelpOverlay then
            closeOverlay

        else
            identity

    else
        identity


toggleEditBlinds : List Key -> Model -> (Model -> Model)
toggleEditBlinds keys model =
    if List.member (Character "E") keys then
        case model.overlayUi of
            NoOverlay ->
                case model.ui of
                    Model.GameScreen actSelection self game welcome ->
                        if self.isAdmin then
                            \m ->
                                { m
                                    | overlayUi =
                                        EditBlindOverlay
                                            (editBlindsSettingsFromSmallBlindAndTimerStatus game.smallBlind game.timer)
                                }

                        else
                            identity

                    Model.RoundResultScreen potResults playerWinnings self game welcome editBlindsSettings ->
                        if self.isAdmin then
                            \m ->
                                { m
                                    | overlayUi =
                                        EditBlindOverlay
                                            (editBlindsSettingsFromSmallBlindAndTimerStatus game.smallBlind game.timer)
                                }

                        else
                            identity

                    _ ->
                        identity

            EditBlindOverlay _ ->
                closeOverlay

            _ ->
                identity

    else
        identity


togglePeek : List Key -> Model -> (Model -> Model)
togglePeek keys model =
    if List.member (Character "P") keys then
        case model.ui of
            GameScreen _ self game welcome ->
                \m -> { m | peeking = not m.peeking }

            RoundResultScreen potResults playerWinnings self game welcome editBlindsSettings ->
                \m -> { m | peeking = not m.peeking }

            _ ->
                identity

    else
        identity


cancel : List Key -> Model -> (Model -> Model)
cancel keys model =
    if List.member Escape keys || List.member (Character "X") keys then
        case model.overlayUi of
            NoOverlay ->
                identity

            _ ->
                closeOverlay

    else
        identity
