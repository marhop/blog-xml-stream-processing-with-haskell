{-# LANGUAGE OverloadedStrings #-}

module ReadDom where

import Data.Maybe
import Data.Text
import Text.XML
import Text.XML.Cursor
import Prelude hiding (readFile)

data Book = Book
  { isbn :: Maybe Text,
    title :: Text,
    author :: Text,
    date :: Maybe Text,
    keywords :: [Text]
  }
  deriving (Show)

main :: IO ()
main = do
  xml <- fromDocument <$> readFile def "data.xml"
  mapM_ print $ xml $/ element "book" &| mkBook

mkBook :: Cursor -> Book
mkBook xml =
  Book
    { isbn = listToMaybe $ attribute "isbn" xml,
      title = mconcat $ xml $/ element (dc "title") &/ content,
      author = mconcat $ xml $/ element (dc "creator") &/ content,
      date = listToMaybe $ xml $/ element (dc "date") &/ content,
      keywords = xml $/ element (dc "subject") &/ content
    }

dc :: Text -> Name
dc n = Name n (Just "http://purl.org/dc/elements/1.1/") (Just "dc")
