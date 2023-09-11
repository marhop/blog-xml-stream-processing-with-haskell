{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Write where

import Conduit
import Data.Text
import Data.XML.Types
import Text.XML.Stream.Render

data Book = Book
  { isbn :: Maybe Text,
    title :: Text,
    author :: Text,
    date :: Maybe Text,
    keywords :: [Text]
  }
  deriving (Show)

main :: IO ()
main = runConduit $ mkBooks .| writeXml .| output

mkBooks :: (Monad m) => ConduitT i Book m ()
mkBooks =
  yieldMany
    [ Book
        { isbn = Just "9781593272838",
          title = "Learn You a Haskell for Great Good!",
          author = "Miran Lipovača",
          date = Nothing,
          keywords = []
        },
      Book
        { isbn = Nothing,
          title = "Pride and Prejudice",
          author = "Jane Austen",
          date = Just "1813",
          keywords = ["marriage", "wealth", "class"]
        }
    ]

output :: (MonadIO m, PrimMonad m) => ConduitT Event o m ()
output = renderBytes def .| stdoutC

writeXml :: (Monad m) => ConduitT Book Event m ()
writeXml = tag "library" mempty $ awaitForever book

book :: (Monad m) => Book -> ConduitT Book Event m ()
book Book {..} = tag "book" (optionalAttr "isbn" isbn) $ do
  tag (dc "title") mempty (content title)
  tag (dc "creator") mempty (content author)
  maybe (pure ()) (tag (dc "date") mempty . content) date
  yieldMany keywords .| awaitForever (tag (dc "subject") mempty . content)

dc :: Text -> Name
dc n = Name n (Just "http://purl.org/dc/elements/1.1/") (Just "dc")
