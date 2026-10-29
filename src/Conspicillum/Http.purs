-- | Text over HTTP, for the few calls the page makes besides the rig's socket.
module Conspicillum.Http (get, post, delete, Reply, exchange) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, notNull, null, toNullable)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

foreign import requestImpl
  :: String -> String -> Nullable String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit

foreign import exchangeImpl
  :: String -> String -> Nullable String -> (Reply -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit

-- | A reply of any status. Only a request that got no reply at all fails.
type Reply = { status :: Int, body :: String }

-- | For a call whose non-2xx replies mean something, such as a 409.
exchange :: String -> String -> Maybe String -> Aff (Either String Reply)
exchange method url body = makeAff \done -> do
  exchangeImpl method url (toNullable body) (done <<< Right <<< Right) (done <<< Right <<< Left)
  pure nonCanceler

request :: String -> String -> Nullable String -> Aff (Either String String)
request method url body = makeAff \done -> do
  requestImpl method url body (done <<< Right <<< Right) (done <<< Right <<< Left)
  pure nonCanceler

get :: String -> Aff (Either String String)
get url = request "GET" url null

post :: String -> String -> Aff (Either String String)
post url body = request "POST" url (notNull body)

delete :: String -> Aff (Either String String)
delete url = request "DELETE" url null
