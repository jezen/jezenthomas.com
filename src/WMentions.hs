{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module WMentions where

import Data.Maybe
import GHC.Generics(Generic)
import System.Environment.Blank (getEnv)
import System.Posix.Files (fileExist)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>), splitPath, joinPath, dropExtension)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as H
import Data.String (fromString)
import Network.Wreq qualified as W
import Data.Functor (($>))
import Data.Aeson
import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as JK
import Control.Lens ((^.))
import Data.Vector (Vector, fromList)
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as VAlg
import Data.Text (Text)
import Data.Text qualified as T
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Ord qualified as Ord
import Control.Applicative ((<|>))
import Hakyll hiding (host)

type WMPost = String

data StoredMentions = SM {
    likes :: Map WMPost (Vector Value),
    replies :: Map WMPost (Vector Value),
    reposts :: Map WMPost (Vector Value)
} deriving (Show, Generic)

instance ToJSON StoredMentions where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON StoredMentions

emptySM :: StoredMentions
emptySM = SM mempty mempty mempty

newtype GetWM = GetWM { unGetWM :: StoredMentions } deriving (Show)

instance FromJSON GetWM where
    parseJSON = withObject "webmention" $ \v -> do
        cs <- v .: "children"
        case cs of
            Array vec -> pure . GetWM $ SM (foldToMap likes) (foldToMap replies) (foldToMap reposts) where

                foldToMap objs = Map.fromList $ foldl go mempty (V.groupBy gb objs) where
                    go wmmap ls
                        | V.null ls = wmmap
                        | otherwise = case V.head ls of
                            Object o -> case JK.lookup "wm-target" o of
                                Just (String t) -> (cleanTarget $ T.unpack t, ls) : wmmap
                                _ -> wmmap
                            _ -> wmmap

                gb (Object kmap1) (Object kmap2) = fromMaybe False $ (==)
                    <$> JK.lookup "wm-target" kmap1
                    <*> JK.lookup "wm-target" kmap2
                gb _ _ = False

                (likes, rest) = V.partition (isType "like-of") vec
                (replies, reposts) = V.partition (isType "in-reply-to") rest

                isType type_ (Object kmap) = maybe False (type_ ==) (JK.lookup "wm-property" kmap)
                isType _ _ = False

            _ -> fail "failed to parse chidren as an array"


cleanTarget :: String -> String
cleanTarget t = ensureSlash . dropExtension . joinPath $ case splitPath t of
    ("https://" : host : []) -> ["/"]
    ("https://" : host : "posts/" : path) -> path
    ("https://" : host : path) -> path
    ( host : "posts/" : path) -> path
    ( "posts/" : path) -> path
    ["/"] -> ["/"]
    path -> "himum" : path
  where
    ensureSlash [] = ['/']
    ensureSlash [c] | c /= '/' = c:['/'] | otherwise = [c]
    ensureSlash (x:xs) = x : ensureSlash xs


renderRepost (Object kmap) = do
  Object author <- JK.lookup "author" kmap
  String authorPhotoUrl <- JK.lookup "photo" author
  String authorUrl <- JK.lookup "url" author
  pure . renderHtml $ H.img ! H.src (fromString $ T.unpack $ authorPhotoUrl) ! H.alt "like image"

renderReply (Object kmap) = do
  String (T.unpack -> url) <- JK.lookup "url" kmap
  String (T.unpack -> published) <- JK.lookup "published" kmap
  Object author <- JK.lookup "author" kmap
  Object content <- JK.lookup "content" kmap
  String (T.unpack -> chtml) <- JK.lookup "html" content <|> JK.lookup "text" content
  String (T.unpack -> authorPhotoUrl) <- JK.lookup "photo" author
  String (T.unpack -> authorName) <- JK.lookup "name" author
  String (T.unpack -> authorUrl) <- JK.lookup "url" author
  pure . renderHtml $
      H.div ! H.class_ "mention" $ do
          H.a ! H.class_ "mention__authorImageLink" ! H.href (fromString url) $
              H.img ! H.class_ "mention_authorLink" ! H.src (fromString authorPhotoUrl) ! H.alt (fromString authorName)
          H.div ! H.class_ "mention__authorLink" $ do
              H.strong (H.a ! H.href (fromString authorUrl) $ (fromString authorName))
              H.div ! H.class_ "mention__content" $ fromString chtml
              H.small $ H.a ! H.href (fromString url) $ fromString published

renderLike (Object kmap) = do
  String (T.unpack -> url) <- JK.lookup "url" kmap
  Object author <- JK.lookup "author" kmap
  String authorPhotoUrl <- JK.lookup "photo" author
  String (T.unpack -> authorPhotoUrl) <- JK.lookup "photo" author
  String (T.unpack -> authorName) <- JK.lookup "name" author
  pure . renderHtml $ H.a ! H.class_ "like" ! H.href (fromString url) $
      H.img ! H.class_ "like__image" ! H.src (fromString authorPhotoUrl) ! H.alt (fromString authorName)

getFromFileOrWebmentionIO :: IO StoredMentions
getFromFileOrWebmentionIO = do
    webementionIoToken <- getEnv "WMTOKEN"
    newMentions <- case webementionIoToken of
      Nothing -> do
        putStrLn $ "Warning: Webmentions: no webmention.io token found"
        putStrLn $ "Warning: Webmentions: please set WMTOKEN environment variable"
        pure emptySM
      Just token -> do

        response <- W.get $ "https://webmention.io/api/mentions.jf2?domain=jezenthomas.com&token=" <> token

        case response ^. W.responseStatus . W.statusCode of
          200 -> case fmap unGetWM . eitherDecode $ response ^. W.responseBody of
              Right sm@(SM _ _ _) -> pure sm
              Left err -> do
                putStrLn $ "Error: Webmentions: couldn't decode webmention.io response: Aeson error: " <> show err
                pure $ SM mempty mempty mempty

          bad -> do
            putStrLn $ "Error: Webmentions: couldn't fetch webmentions. response status code: " <> show bad
            pure emptySM

    fileExist "webmentions.json" >>= \case
      True -> do
        SM oldLikes oldReplies oldReposts <- fromMaybe emptySM . decodeStrict <$> B.readFile "webmentions.json"

        let SM newLikes newReplies newReposts = newMentions
            result = SM (merge newLikes oldLikes) (merge newReplies oldReplies)
              (merge oldReposts oldReposts)

            merge = Map.unionWith checkIds

            checkIds :: Vector Value -> Vector Value -> Vector Value
            checkIds vec1 vec2 = VAlg.nubBy comparison (vec1 <> vec2)

            comparison v1@(Object o1) v2@(Object o2) =
              fromMaybe (Ord.compare (encode v1) (encode v2)) $
                  Ord.compare <$> JK.lookup "wm-id" o1 <*> JK.lookup "wm-id" o2

            comparison o1 o2 = (Ord.compare (encode o1) (encode o2))

        BL.writeFile "webmentions.json" (encodePretty result) $> result

      False -> BL.writeFile "webmentions.json" (encodePretty newMentions) $> newMentions
