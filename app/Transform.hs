{-# LANGUAGE OverloadedStrings #-}

module Transform where

import Conduit
import Control.Monad
import Data.Text
import Data.XML.Types
import Text.XML.Stream.Parse (parseFile)
import Text.XML.Stream.Render (content, def, renderBytes, tag)

main :: IO ()
main = runConduitRes $ parseFile def "data.xml" .| transform .| output

-- | Append a keyword to the (possibly empty) list of subject elements.
transform :: (Monad m) => ConduitT Event Event m ()
transform = do
  -- The subject elements are always at the end of the element content.
  takeWhileC (/= EventEndElement "book")
  atEndOfStream <- nullC
  unless atEndOfStream $ do
    tag (dc "subject") mempty (content "...")
    takeC 1
    transform

output :: (MonadIO m, PrimMonad m) => ConduitT Event o m ()
output = renderBytes def .| stdoutC

dc :: Text -> Name
dc n = Name n (Just "http://purl.org/dc/elements/1.1/") (Just "dc")
