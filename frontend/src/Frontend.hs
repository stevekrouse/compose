{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Control.Lens
import qualified Data.Map  as Map
import Reflex
import Reflex.Dom
import Data.Text (pack, unpack, Text)
import Data.Functor (($>))
import Control.Applicative  ((<*>), (<$>))
import Control.Monad.Fix (MonadFix)
import Data.Maybe (isNothing, catMaybes, isJust)
import Data.Semigroup (First (..))
import Data.Foldable (find)
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Common.Route


data MyEvent = 
  Login Registrant | 
  Logout | 
  NewURL String

-- someone trying to register
-- may or may not become a User depending on validations
-- TODO: create User type with more fields, and convert validated registrats over
data Registrant = Registrant
  { _username :: Text
  , _email :: Text
  , _password :: Text
  } deriving (Show)

-- validates registrants on its own merits
-- as well as on the results of checking if the username or email are already taken
validate :: Registrant -> Bool -> Bool -> [ String ]
validate (Registrant username email password) usernameAlreadyTaken_ emailAlreadyTaken_ =
    catMaybes [
      if usernameAlreadyTaken_ then Just "username has already been taken" else Nothing,
      if emailAlreadyTaken_ then Just "email has already been taken" else Nothing,
      if length (unpack password) < 8 then Just "password is too short (minimum is 8 characters)" else Nothing,
      if null $ unpack username then Just "username can't be blank" else Nothing,
      if null $ unpack email then Just "email can't be blank" else Nothing,
      if null $ unpack password then Just "password can't be blank" else Nothing
    ]

-- UI to register
-- inputs functions to test whether username or email already taken
-- TODO: model these functions as as Text-> Event Bool (where the Event represents the Server response)
register 
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , EventWriter t (First MyEvent) m
     ) => (Text -> Dynamic t Bool) 
       -> (Text -> Dynamic t Bool) 
       -> m ()
register usernameAlreadyTaken emailAlreadyTaken = divClass "auth-page" $ divClass "container page" $ divClass "row" $ divClass "col-md-6 offset-md-3 col-xs-12" $ mdo
    elClass "h1" "text-xs-center" $ text "Sign up"
    elClass "p" "text-xs-center" $
      aClass loginURL "" $ text "Already have an account?"

    elClass "ul" "error-messages" $
      simpleList errors (el "li" . dynText . fmap pack)

    newUserSubmitted <- el "form" $ do
      usernameI <- elClass "fieldset" "form-group" $
        inputElement $ def
          & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
            [ ("class","form-control form-control-lg")
            , ("placeholder","Your name")
            ]
      emailI <- elClass "fieldset" "form-group" $
        inputElement $ def
          & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
            [ ("class","form-control form-control-lg")
            , ("placeholder","Email")
            ]
      passI <- elClass "fieldset" "form-group" $
        inputElement $ def
          & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
            [ ("class","form-control form-control-lg")
            , ("placeholder","Password")
            , ("type","password")
            ]

      (submitElem, _) <- elAttr' "button" ("class" =: "btn btn-lg btn-primary pull-xs-right" <> "type" =: "button") $ text "Sign Up"
      let submitE = domEvent Click submitElem
      let user = Registrant
            <$> usernameI ^. to _inputElement_value
            <*> emailI ^. to _inputElement_value
            <*> passI ^. to _inputElement_value

      pure $ current user <@ submitE

    let (someErrors, goodUser) =
          fanEither
          . pushAlways (\registrant -> do
                        v <- sample . current
                             $ validate registrant
                             <$> (usernameAlreadyTaken . _username $ registrant)
                             <*> (emailAlreadyTaken . _email $ registrant)
                        pure (if null v then Right registrant else Left v))
          $ newUserSubmitted
    errors <- holdDyn [] someErrors

    tellEvent $ First . Login <$> goodUser
    tellEvent $ First (NewURL baseURL) <$ goodUser


homePage :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Maybe Registrant -> m ()
homePage loggedInUser = elClass "div" "home-page" $ mdo
  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"


aClass :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First MyEvent) m) => String -> String -> m () -> m ()
aClass route klass contents = do
  (el, _) <- elAttr' "a" (Map.fromList [("class", pack klass), ("href", "#")])
    contents
  tellEvent $ First (NewURL route) <$ domEvent Click el


navbar :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First MyEvent) m) => String -> Maybe Registrant -> m ()
navbar url loggedInUser = do
  elClass "nav" "navbar navbar-light" $
    elClass "div" "container" $ do
      aClass baseURL "navbar-brand" $ text "conduit"
      elClass "ul" "nav navbar-nav pull-xs-right" $ do
        navItem baseURL $ text "Home"
        menu loggedInUser
  where
    menu (Just Registrant {_username = username}) = do
      navItem "denote-conduit.com/editor" $ do
        elClass "i" "ion-compose" blank
        text " "
        text "New Post"
      navItem "denote-conduit.com/settings" $ do
        elClass "i" "ion-gear-a" blank
        text " "
        text "Settings"
      navItem ("denote-conduit.com/profile/" ++ unpack username) $ text username
    menu Nothing                                 = do
      navItem loginURL    $ text "Sign in"
      navItem registerURL $ text "Sign up"

    navItem :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First MyEvent) m) => String -> m () -> m ()
    navItem route contents = elClass "li" "nav-item" $ do
      aClass route ("nav-link" ++ if url == route then " active" else "") contents


settings :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First MyEvent) m) => Maybe Registrant -> m ()
settings Nothing = pure ()
settings (Just (Registrant username email password)) = do
  elClass "div" "settings-page" $ do
    elClass "div" "container page" $
      elClass "div" "row" $
        elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
          elClass "h1" "text-xs-center" $ text "Your Settings"
          el "form" $ do
            el "fieldset" $ do
              urlI <- elClass "fieldset" "form-group" $
                inputElement $ def
                  & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                    [("class","form-control")
                    ,("placeholder","URL of profile picture")
                    ]
              usernameI <- elClass "fieldset" "form-group" $
                inputElement $ def
                  & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                    [ ("class","form-control")
                    , ("placeholder","Your name")
                    , ("value", username)
                    ]
              bioI <- elClass "fieldset" "form-group" $
                textAreaElement $ def
                  & textAreaElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                    [("class","form-control")
                    ,("placeholder","Short bio about you")
                    ,("rows","8")
                    ]
              emailI <- elClass "fieldset" "form-group" $
                inputElement $ def
                  & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                    [ ("class", "form-control")
                    , ("placeholder", "Email")
                    , ("type", "input")
                    , ("value", email)
                    ]
              passwordI <- elClass "fieldset" "form-group" $
                inputElement $ def
                  & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                    [("class","form-control")
                    ,("placeholder","Password")
                    ,("type","password")
                    ]
              updateE <- buttonClass "btn btn-lg btn-primary pull-xs-right" $ text "Update Settings"
              -- TODO

              el "hr" blank

              logoutClick <- buttonClass "btn btn-outline-danger" $ text "Logout"
              tellEvent $ First (NewURL baseURL) <$ logoutClick
              tellEvent $ First Logout <$ logoutClick

login :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First MyEvent) m) => (Text -> Text -> Dynamic t (Maybe Registrant)) -> m ()
login loginUser = elClass "div" "auth-page" $ do
  elClass "div" "container-page" $ do
    elClass "div" "row" $ do
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ mdo
        elClass "h1" "text-xs-center" $ text "Sign in"

        elClass "p" "text-xs-center" $
          aClass registerURL "" $ text "Need an account?"

        -- errorDyn <- holdDyn Nothing $ leftmost [Nothing <$ submitE, Just <$> errorE]

        elClass "ul" "error-messages" $ blank
          -- void $ dyn $ ffor errorDyn $ traverse_ $ \_ ->
          --   el "li" (text "Login Failed")

        el "form" $ mdo
          emailI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Email")
                ]
          passI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Password")
                , ("type","password")
                ]
          submitE <- buttonClass "btn btn-lg btn-primary pull-xs-right" $ text "Sign in"

          let emailAndPassword = (,) <$> emailI ^. to _inputElement_value <*> passI ^. to _inputElement_value
          let emailAndPasswordE = current emailAndPassword <@ submitE
          let loggedInUser = ffilter isJust $ pushAlways (\(email, password) -> sample . current $ loginUser email password) emailAndPasswordE
          
          tellEvent $ fforMaybe loggedInUser (First . Login <$>)
          tellEvent $ First (NewURL baseURL) <$ loggedInUser


buttonClass :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First MyEvent) m) => Text -> m a -> m (Event t ())
buttonClass klass m = do
  (el, _) <- elAttr' "button" ("class" =: klass <> "type" =: "button") m
  pure $ () <$ domEvent Click el


-- maps the URL bar to a page
-- passes the eventWriter updates as URL updates
-- passes the output Event Maybe Registrant as authentication events
-- TOODO: how do we funnel further events up to the global level?
router :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First MyEvent) m) => String -> Maybe Registrant -> (Text -> Dynamic t Bool) -> (Text -> Dynamic t Bool) -> (Text -> Text -> Dynamic t (Maybe Registrant)) -> m ()
router url loggedInUser usernameAlreadyTaken emailAlreadyTaken loginUser
  | url == baseURL                               = homePage loggedInUser
  | url == registerURL && isNothing loggedInUser = register usernameAlreadyTaken emailAlreadyTaken
  | url == loginURL && isNothing loggedInUser    = login loginUser
  | url == settingsURL && isJust loggedInUser    = settings loggedInUser
  | otherwise                                    = homePage loggedInUser-- TODO: 404 page


-- this simulates an entire browser tab with a URL bar 
browser :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, EventWriter t (First Registrant) m) => (Text -> Dynamic t Bool) -> (Text -> Dynamic t Bool) -> (Text -> Text -> Dynamic t (Maybe Registrant)) -> m ()
browser usernameAlreadyTaken emailAlreadyTaken loginUser = mdo
  let allEvents = getFirst <$> _allEvents
  (_, _allEvents) <- runEventWriterT $ mdo
    divClass "browser" $ mdo
      urlElem <- inputElement $ def
        & inputElementConfig_setValue .~ (pack <$> urlBarUpdates) 
        & inputElementConfig_initialValue .~ pack initialURL -- this starts the URL value
        & inputElementConfig_elementConfig.elementConfig_initialAttributes 
          .~ Map.fromList 
            [ ("class","url-bar")
            , ("value", pack initialURL) -- and this overwritest the URL value
            ] 
      let urlB = unpack <$> _inputElement_value urlElem -- TODO: only push new URL values on Enter-key-presses
      dyn_ $ navbar <$> urlB <*> loggedInUser
      dyn_ $ router <$> urlB <*> loggedInUser <*> constDyn usernameAlreadyTaken <*> constDyn emailAlreadyTaken <*> constDyn loginUser
      
      let urlBarUpdates = fforMaybe allEvents (\e -> case e of { NewURL s -> Just s; _ -> Nothing })
      loggedInUser <- holdDyn Nothing $ fforMaybe allEvents (\e -> case e of { Login r -> Just (Just r); Logout -> Just Nothing; _ -> Nothing})
      pure ()

  let newUserE = fforMaybe allEvents (\e -> case e of { Login r -> Just (First r); _ -> Nothing })
  tellEvent newUserE
  pure ()

baseURL :: String
baseURL = "denote-conduit.com/"

registerURL :: String
registerURL = baseURL ++ "register"

loginURL :: String
loginURL = baseURL ++ "login"

settingsURL :: String
settingsURL = baseURL ++ "settings"

initialURL :: String
initialURL = registerURL


-- this runs multiple browser tabs in the same window
-- it proccesses global state, and feeds it back into the browsers
-- TODO: store state from previous runs, and migrate between them
app :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
app = divClass "universe" $ mdo
  (_, newUser) <- runEventWriterT $ browser usernameAlreadyTaken emailAlreadyTaken loginUser
  (_, newUser1) <- runEventWriterT $ browser usernameAlreadyTaken emailAlreadyTaken loginUser
  users <- foldDyn addUser [Registrant "a" "b" "c"] $ (getFirst <$>) $ newUser <> newUser1 -- TODO UUID and switch to map? & leftmost is not quite right


  -- TODO: model these properly: Event a -> Event b
  -- where we add in an artifical delay to simulate network load time
  let usernameAlreadyTaken username_ = isJust . find (== username_) . map _username <$> users
      emailAlreadyTaken email_ = isJust . find (== email_) . map _email <$> users
      loginUser email_ password_ = find (\r -> _email r == email_ && _password r == password_) <$> users
      addUser user users = if any (\r -> _email user == _email r || _username user == _username r) users then users else user : users

  dynText $ fmap (pack . show) users

  pure ()


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Compose Conduit RealWorld"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      app
  }
