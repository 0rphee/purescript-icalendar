--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.Type
-- Copyright : (c) 2006-2009, Galois, Inc. 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability : provisional
-- Portability: portable
--
--
-- Representing MIME types and values.
--
--------------------------------------------------------------------
module Codec.MIME.Type where

import Prim hiding (Type)
import Prelude

import Data.Foldable (elem)
import Data.Foldable as F
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as T
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

data MIMEParam = MIMEParam
  { paramName :: String
  , paramValue :: String
  }

derive instance Ord MIMEParam
derive instance Eq MIMEParam

data Type = Type
  { mimeType :: MIMEType
  , mimeParams :: List MIMEParam
  }

derive instance Ord Type
derive instance Eq Type

break :: String -> String -> (String /\ String)
break pat s = res.before /\ res.after
  where
  index = case T.indexOf (T.Pattern pat) s of
    Just v -> v
    Nothing -> 0
  res = T.splitAt index s

-- | The @null@ MIME record type value; currently a @text/plain@.
nullType :: Type
nullType = Type
  { mimeType: Text "plain"
  , mimeParams: Nil
  }

showType :: Type -> String
showType (Type t) = showMIMEType (t.mimeType) <> showMIMEParams (t.mimeParams)

showMIMEParams :: List (MIMEParam) -> String
showMIMEParams ps = F.fold $ map showP ps
  where
  showP (MIMEParam a) = "; " <> a.paramName <> "=\"" <> a.paramValue <> "\""

data MIMEType
  = Application SubType
  | Audio SubType
  | Image SubType
  | Message SubType
  | Model SubType
  | Multipart Multipart
  | Text TextType
  | Video SubType
  | Other { otherType :: String, otherSubType :: SubType }

derive instance Ord MIMEType
derive instance Eq MIMEType

showMIMEType :: MIMEType -> String
showMIMEType t =
  case t of
    Application s -> "application/" <> s
    Audio s -> "audio/" <> s
    Image s -> "image/" <> s
    Message s -> "message/" <> s
    Model s -> "model/" <> s
    Multipart s -> "multipart/" <> showMultipart s
    Text s -> "text/" <> s
    Video s -> "video/" <> s
    Other a -> a.otherType <> "/" <> a.otherSubType

-- | a (type, subtype) MIME pair.
data MIMEPair = MIMEPair String SubType

derive instance Eq MIMEPair

showMIMEPair :: MIMEPair -> String
showMIMEPair (MIMEPair a b) = a <> "/" <> b

-- | default subtype representation.
type SubType = String

-- | subtype for text content; currently just a string.
type TextType = SubType

subTypeString :: Type -> String
subTypeString (Type t) = T.drop 1 $ snd $ break ("/") (showMIMEType (t.mimeType))

majTypeString :: Type -> String
majTypeString (Type t) = fst $ break ("/") (showMIMEType (t.mimeType))

data Multipart
  = Alternative
  | Byteranges
  | Digest
  | Encrypted
  | FormData
  | Mixed
  | Parallel
  | Related
  | Signed
  | Extension String -- ^ e.g., 'x-foo' (i.e., includes the 'x-' bit)
  | OtherMulti String -- unrecognized\/uninterpreted.

-- (e.g., appledouble, voice-message, etc.)

derive instance Ord Multipart
derive instance Eq Multipart

isXmlBased :: Type -> Boolean
isXmlBased (Type t) =
  case t.mimeType of
    Multipart _ -> false
    _ -> case T.stripSuffix (T.Pattern "") (subTypeString (Type t)) of
      Just _ -> true
      Nothing -> false

isXmlType :: Type -> Boolean
isXmlType tt@(Type t) = isXmlBased tt ||
  case t.mimeType of
    Application s -> elem s xml_media_types
    Text s -> elem s xml_media_types
    _ -> false
  where
  -- Note: xml-dtd isn't considered an XML type here.
  xml_media_types :: List String
  xml_media_types =
    List.fromFoldable [ "xml", "xml-external-parsed-entity" ]

showMultipart :: Multipart -> String
showMultipart m =
  case m of
    Alternative -> "alternative"
    Byteranges -> "byteranges"
    Digest -> "digest"
    Encrypted -> "encrypted"
    FormData -> "form-data"
    Mixed -> "mixed"
    Parallel -> "parallel"
    Related -> "related"
    Signed -> "signed"
    Extension e -> e
    OtherMulti e -> e

type Content = String

data MIMEValue = MIMEValue
  { mime_val_type :: Type
  , mime_val_disp :: Maybe Disposition
  , mime_val_content :: MIMEContent
  , mime_val_headers :: List MIMEParam
  , mime_val_inc_type :: Boolean
  }

derive instance Eq MIMEValue

nullMIMEValue :: MIMEValue
nullMIMEValue = MIMEValue
  { mime_val_type: nullType
  , mime_val_disp: Nothing
  , mime_val_content: Multi Nil
  , mime_val_headers: Nil
  , mime_val_inc_type: true
  }

data MIMEContent
  = Single Content
  | Multi (List MIMEValue)

derive instance Eq MIMEContent

data Disposition = Disposition
  { dispType :: DispType
  , dispParams :: List DispParam
  }

derive instance Eq Disposition

data DispType
  = DispInline
  | DispAttachment
  | DispFormData
  | DispOther String

derive instance Eq DispType

data DispParam
  = Name String
  | Filename String
  | CreationDate String
  | ModDate String
  | ReadDate String
  | Size String
  | OtherParam String String

derive instance Eq DispParam
