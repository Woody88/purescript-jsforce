module MainWeb where
import Prelude
import Salesforce.Connection.Web
import Salesforce.Connection.Web.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Salesforce.Connection (ClientId(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (click)
import Web.HTML.HTMLButtonElement as Button
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window as Window

main :: Effect Unit 
main = do
    document  <- Window.document =<< window
    buttonEl  <- (getElementById "sf-login" $ toNonElementParentNode document)
    msfLoginBtn <- pure $ Button.fromElement =<< buttonEl
    
    clickListener <- (\win -> eventListener (\_ -> loginOauth config win (\_ -> pure unit))) =<< window

    case msfLoginBtn of 
        Just sfLoginBtn -> 
            addEventListener click clickListener false $ Button.toEventTarget sfLoginBtn 
        _ -> 
            pure unit 
    
    where 
        config = 
            { serverUrl: "http://login.salesforce.com/services/oauth2/authorize"
            , response_type: "token"
            , clientId: ClientId ".."
            , redirect_uri: ".."
            , scope: "full"
            }
