{-# LANGUAGE OverloadedStrings #-}

module ReadNested where

import Conduit
import Control.Monad
import Data.Text
import Data.XML.Types
import Text.XML.Stream.Parse

main :: IO ()
main = runConduitRes $ parseFile def "nested.xml" .| readXml .| output

output :: (MonadIO m) => ConduitT Text o m ()
output = mapM_C (liftIO . print)

readXml :: (MonadThrow m) => ConduitT Event Text m ()
readXml = force "failed reading XML" node

node :: (MonadThrow m) => ConduitT Event Text m (Maybe ())
node = tag' "node" (requireAttr "label") $ \label -> do
  mbNode <- node
  case mbNode of
    Just () -> void $ many node
    Nothing -> yield label
