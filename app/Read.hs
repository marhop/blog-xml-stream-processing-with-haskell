{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Read where

import Conduit
import Data.Maybe
import Data.Text
import Data.XML.Types
import Text.XML.Stream.Parse

data Book = Book
  { isbn :: Maybe Text,
    title :: Text,
    author :: Text,
    date :: Maybe Text,
    keywords :: [Text]
  }
  deriving (Show)

main :: IO ()
main = runConduitRes $ parseFile def "data.xml" .| readXml .| output

output :: (MonadIO m) => ConduitT Book o m ()
output = mapM_C (liftIO . print)

readXml :: (MonadThrow m) => ConduitT Event Book m ()
readXml = force "failed reading XML" $ tagNoAttr "library" (manyYield book)

book :: (MonadThrow m) => ConduitT Event o m (Maybe Book)
book = tag' "book" (attr "isbn") $ \isbn -> do
  title <- fromMaybe "" <$> tagNoAttr (withName $ dc "title") content
  author <- fromMaybe "" <$> tagNoAttr (withName $ dc "creator") content
  date <- tagNoAttr (withName $ dc "date") content
  _ <- ignoreTree (withName $ dc "description") ignoreAttrs
  keywords <- many $ tagNoAttr (withName $ dc "subject") content
  pure $ Book {..}

withName :: Name -> NameMatcher Name
withName = matching . (==)

dc :: Text -> Name
dc n = Name n (Just "http://purl.org/dc/elements/1.1/") (Just "dc")
