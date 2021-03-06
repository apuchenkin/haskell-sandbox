module Foundation where

import Import.NoFoundation
import Database.Persist.Sql     (ConnectionPool, runSqlPool)
import Text.Hamlet              (hamletFile)
import Text.Jasmine             (minifym)
import Yesod.Auth.Http.Basic
import Yesod.Default.Util       (addStaticContentExternal)
import Yesod.Core.Types         (Logger)
import Data.ByteString.Base64   (decodeLenient)
import Data.Word8               (_space, _colon)
import qualified Data.ByteString as B
import qualified Network.Wai.Internal  as W (requestHeaders)
import qualified Yesod.Core.Unsafe as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appChan        :: TChan Text
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Sessions is disabled
    makeSessionBackend _ = return Nothing

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    -- authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized ChatR _ = return Authorized

    -- Default to Authorized for now.
    isAuthorized _ _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing  -> return AuthenticationRequired
            Just _   -> return Authorized

--    instance Yesod App where
--      isAuthorized SecureR _   =
--        maybeAuthId >>= return . maybe AuthenticationRequired (const Authorized)
--      isAuthorized _ _         = Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

--    errorHandler NotAuthenticated = selectRep $
--          provideRep $ do
--            addHeader "WWW-Authenticate" $ T.concat
--                  [ "RedirectJSON realm=\"Realm\", param=\"localhost\"" ]
--            -- send error response here
--            ...
--    errorHandler e = defaultErrorHandler e

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

--    -- Where to send a user after successful login
--    loginDest _ = HomeR
--    -- Where to send a user after logout
--    logoutDest _ = HomeR
--    -- Override the above two destinations when a Referer: header is present
--    redirectToReferer _ = True

--    getAuthId = return . Just . credsIdent
    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
--    authPlugins _ = [authDummy]
--    authHttpManager = getHttpManager

    maybeAuthId = do
        request <- waiRequest
        let authorization = lookup "Authorization" (W.requestHeaders request)
        case authorization of
            Just header -> do
                let (_, v) = B.breakByte _space header
                let (username, password) = B.breakByte _colon $ decodeLenient v
                case B.uncons password of
                    Just (_, p) -> checkCreds (decodeUtf8 username) (decodeUtf8 p)
                    _ -> return Nothing
            _ -> return Nothing
        where
            checkCreds ident password = runDB $ do
            user <- selectKeysList [UserIdent ==. ident, UserPassword ==. Just password] [LimitTo 1]
            return $ listToMaybe user

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
