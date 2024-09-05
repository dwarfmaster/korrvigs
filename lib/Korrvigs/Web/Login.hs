module Korrvigs.Web.Login (getLoginR, postLoginR, logWrap, logout) where

import Control.Lens
import Data.Password.Scrypt
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Yesod hiding (check)

newtype LoginData = LoginData {_password :: Text}

makeLenses ''LoginData

getHashedPassword :: Handler (PasswordHash Scrypt)
getHashedPassword = getsYesod web_pwd

getPasswordSalt :: Handler Text
getPasswordSalt = getsYesod web_salt

check :: Text -> Handler Bool
check pass = do
  salt <- getPasswordSalt
  let salted = mkPassword $ pass <> salt
  target <- getHashedPassword
  pure $ checkPassword salted target == PasswordCheckSuccess

loginForm :: Html -> MForm Handler (FormResult LoginData, Widget)
loginForm =
  renderDivs $ LoginData <$> areq passwordField "Password" Nothing

checkLogged :: Handler Bool
checkLogged =
  lookupSession "logged" >>= \case
    Just linfo -> case iso8601ParseM $ T.unpack linfo of
      Just time -> do
        current <- liftIO getCurrentTime
        let days = diffUTCTime current time / nominalDay
        if days > 1.0
          then deleteSession "logged" >> pure False
          else pure True
      Nothing -> deleteSession "logged" >> pure False
    Nothing -> pure False

loginWidget :: Handler Widget
loginWidget = do
  ((res, widget), enctype) <- runFormPost loginForm
  let prompt =
        Rcs.formsStyle
          >> [whamlet|
         <form #login-form .required method="post" action=@{LoginR} enctype=#{enctype}>
           ^{widget}
           <button type="submit">Login
       |]
  case res of
    FormSuccess pwd ->
      check (pwd ^. password) >>= \logged ->
        if logged
          then do
            time <- liftIO getCurrentTime
            setSession "logged" $ T.pack $ iso8601Show time
            redirectUltDest HomeR
          else pure $ [whamlet|<p>Login failed !|] >> prompt
    FormFailure errs ->
      pure
        [whamlet|
          <p>Failure:
            <ul>
              $forall err <- errs
                <li> #{err}
        |]
    FormMissing -> pure prompt

getLoginR :: Handler Html
getLoginR = loginWidget >>= defaultLayout

postLoginR :: Handler Html
postLoginR = loginWidget >>= defaultLayout

logWrap :: Handler a -> Handler a
logWrap handler =
  checkLogged >>= \logged ->
    if logged
      then handler
      else do
        route <- getCurrentRoute
        maybe (pure ()) setUltDest route
        redirect LoginR

logout :: Handler ()
logout = deleteSession "logged" >> redirect LoginR
