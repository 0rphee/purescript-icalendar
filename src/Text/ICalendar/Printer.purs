-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards   #-}
-- {-# LANGUAGE TupleSections     #-}
-- {-# LANGUAGE CPP #-}
module Text.ICalendar.Printer
  ( EncodingFunctions(..)
  -- , printICalendar
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Data.Default
import Data.Monoid
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Text.ICalendar.Types

import Control.Monad.RWS (get, put, tell, RWS, asks, modify, runRWS)
import Data.Array as List
import Data.CaseInsensitive as CI
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (for_, sequence_, traverse_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as S
import Data.String as BS
import Data.Time as Time
import Data.Unit (Unit(..))
import Text.ICalendar.Types as Ver
import URI.URI as URI
import Codec.MIME.Type

data UPrintf = UPrintf -- TODO 

class PrintfType t where
  spr :: String -> List UPrintf -> t

printf :: forall t. PrintfType t => String -> t
printf fmts = spr fmts Nil

data Builder = Builder -- TODO

data ByteString = BS -- TODO

-- showMIMEType :: a
-- showMIMEType = undefined

-- | Functions for encoding into bytestring builders.
data EncodingFunctions = EncodingFunctions
  { efChar2Bu :: Char -> Builder
  , efChar2Len :: Char -> Int -- ^ How many octets the character is encoded.
  }

utf8Len :: Char -> Int
utf8Len c = h
  where
  o = fromEnum c
  h
    | o < 0x80 = 1
    | o < 0x800 = 2
    | o < 0x10000 = 3
    | o < 0x200000 = 4
    | o < 0x4000000 = 5
    | otherwise = 6

newtype AltRep = AltRep URI.URI
newtype CN = CN Text
newtype Dir = Dir URI.URI
newtype Member = Member (Set URI.URI)
newtype DelTo = DelTo (Set URI.URI)
newtype DelFrom = DelFrom (Set URI.URI)
newtype RSVP = RSVP Boolean
newtype SentBy = SentBy CalAddress

data Quoting = NeedQuotes | Optional | NoQuotes

derive instance Eq Quoting
derive instance Ord Quoting

-- | UTF8.
-- instance Default EncodingFunctions where
--   def = EncodingFunctions Bu.charUtf8
--     utf8Len

type ContentPrinter = RWS EncodingFunctions Builder Int

-- | Print a VCalendar object to a ByteString.
-- printICalendar :: EncodingFunctions -> VCalendar -> ByteString
-- printICalendar r v = (\(_ /\ _ /\ x) -> Bu.toLazyByteString x) $
--   runRWS (printVCalendar v) r 0

-- {{{ Component printers

printVCalendar :: VCalendar -> ContentPrinter ()
printVCalendar (VCalendar a) = do
  line "BEGIN:VCALENDAR"
  ln $ do
    prop "VERSION" $ a.vcVersion.versionOther -- Should be first for
    printValue a.vcVersion -- compatibility.
  ln $ do
    prop "PRODID" $ a.vcProdId.prodIdOther
    text $ a.vcProdId.prodIdValue
  ln $ do
    prop "CALSCALE" $ a.vcScalescaleOther
    text <<< _.original $ a.vcScalescaleValue
  for_ a.vcMethod $ \meth -> do
    prop "METHOD" $ meth.methodOther
    ln <<< text <<< _.original $ meth.methodValue
  traverse_ printProperty a.vcOther
  traverse_ printVTimeZone a.vcTimeZones
  traverse_ printVEvent a.vcEvents
  traverse_ printVTodo a.vcTodos
  traverse_ printVJournal a.vcJournals
  traverse_ printVFreeBusy a.vcFreeBusys
  traverse_ printVOther a.vcOtherComps
  line "END:VCALENDAR"

printVTimeZone :: VTimeZone -> ContentPrinter ()
printVTimeZone ( {-VTimeZone-} a) = do
  line "BEGIN:VTIMEZONE"
  ln $ do
    prop "TZID" $ a.tzidOther.vtzId
    text $ a.vtzId.tzidValue
  printProperty a.vtzLastMod
  for_ a.vtzUrl $ \url -> do
    prop "TZURL" $ a.url.tzUrlOther
    ln.printShow $ a.url.tzUrlValue
  traverse_ (printTZProp "STANDARD") a.vtzStandardC
  traverse_ (printTZProp "DAYLIGHT") a.vtzDaylightC
  traverse_ printProperty a.vtzOther
  line "END:VTIMEZONE"

printTZProp :: ByteString -> TZProp -> ContentPrinter ()
printTZProp name (TZProp a) = do
  line $ "BEGIN:" <> name
  printProperty a.tzpDTStart
  ln $ do
    prop "TZOFFSETTO" $ a.tzpTZOffsetTo.utcOffsetOther
    printUTCOffset $ a.tzpTZOffsetTo.utcOffsetValue
  ln $ do
    prop "TZOFFSETFROM" $ a.tzpTZOffsetTo.utcOffsetOther
    printUTCOffset $ a.tzpTZOffsetFrom.utcOffsetValue
  printProperty a.tzpRRule
  printProperty a.tzpComment
  printProperty a.tzpRDate
  for_ a.tzpTZName $ \(TZName a) -> ln $ do
    prop "TZNAME" $ toParam a.tzNameLanguage <> toParam a.tzNameOther
    text a.tzNameValue
  traverse_ printProperty a.tzpOther
  line $ "END:" <> name

printVEvent :: VEvent -> ContentPrinter ()
printVEvent ( {-VEvent -} a) = do
  line "BEGIN:VEVENT"
  printProperty a.veDTStamp
  printProperty a.veUID
  printProperty a.veDTStart
  printProperty a.veClass
  printProperty a.veCreated
  printProperty a.veDescription
  printProperty a.veGeo
  printProperty a.veLastMod
  printProperty a.veLocation
  printProperty a.veOrganizer
  printProperty a.vePriority
  printProperty a.veSeq
  printProperty a.veStatus
  printProperty a.veSummary
  printProperty a.veTransp
  printProperty a.veUrl
  printProperty a.veRecurId
  printProperty a.veRRule
  printProperty a.veDTEndDuration
  printProperty a.veAttach
  printProperty a.veAttendee
  printProperty a.veCategories
  printProperty a.veComment
  printProperty a.veContact
  printProperty a.veExDate
  printProperty a.veRStatus
  printProperty a.veRelated
  printProperty a.veResources
  printProperty a.veRDate
  for_ a.veAlarms printVAlarm
  printProperty a.veOther
  line "END:VEVENT"

printVTodo :: VTodo -> ContentPrinter ()
printVTodo ( {-VTodo-} a) = do
  line "BEGIN:VTODO"
  printProperty a.vtDTStamp
  printProperty a.vtUID
  printProperty a.vtClass
  printProperty a.vtCompleted
  printProperty a.vtCreated
  printProperty a.vtDescription
  printProperty a.vtDTStart
  printProperty a.vtGeo
  printProperty a.vtLastMod
  printProperty a.vtLocation
  printProperty a.vtOrganizer
  printProperty a.vtPercent
  printProperty a.vtPriority
  printProperty a.vtSeq
  printProperty a.vtRecurId
  printProperty a.vtStatus
  printProperty a.vtSummary
  printProperty a.vtUrl
  printProperty a.vtRRule
  printProperty a.vtDueDuration
  printProperty a.vtAttach
  printProperty a.vtAttendee
  printProperty a.vtCategories
  printProperty a.vtComment
  printProperty a.vtContact
  printProperty a.vtExDate
  printProperty a.vtRStatus
  printProperty a.vtRelated
  printProperty a.vtResources
  printProperty a.vtRDate
  for_ a.vtAlarms printVAlarm
  printProperty a.vtOther
  line "END:VTODO"

printVJournal :: VJournal -> ContentPrinter ()
printVJournal ( {-VJournal-} a) = do
  line "BEGIN:VJOURNAL"
  printProperty a.vjDTStamp
  printProperty a.vjUID
  printProperty a.vjClass
  printProperty a.vjCreated
  printProperty a.vjDescription
  printProperty a.vjDTStart
  printProperty a.vjLastMod
  printProperty a.vjOrganizer
  printProperty a.vjSeq
  printProperty a.vjRecurId
  printProperty a.vjStatus
  printProperty a.vjSummary
  printProperty a.vjUrl
  printProperty a.vjRRule
  printProperty a.vjAttach
  printProperty a.vjAttendee
  printProperty a.vjCategories
  printProperty a.vjComment
  printProperty a.vjContact
  printProperty a.vjExDate
  printProperty a.vjRStatus
  printProperty a.vjRelated
  printProperty a.vjRDate
  printProperty a.vjOther
  line "END:VJOURNAL"

printVFreeBusy :: VFreeBusy -> ContentPrinter ()
printVFreeBusy ( {-VFreeBusy-} a) = do
  line "BEGIN:VFREEBUSY"
  printProperty a.vfbDTStamp
  printProperty a.vfbUID
  printProperty a.vfbContact
  printProperty a.vfbDTStart
  printProperty a.vfbDTEnd
  printProperty a.vfbOrganizer
  printProperty a.vfbUrl
  printProperty a.vfbAttendee
  printProperty a.vfbComment
  printProperty a.vfbFreeBusy
  printProperty a.vfbRStatus
  printProperty a.vfbOther
  line "END:VFREEBUSY"

printVOther :: VOther -> ContentPrinter ()
printVOther (VOther a) = do
  ln.out $ "BEGIN:V" <> CI.original a.voName
  traverse_ printProperty a.voProps
  ln.out $ "END:V" <> CI.original a.voName

printVAlarm :: VAlarm -> ContentPrinter ()
printVAlarm va = do
  line "BEGIN:VALARM"
  prop "ACTION" $ va.vaActionOther
  case va of
    (VAlarmAudio a) -> do
      ln $ bytestring "AUDIO"
      printProperty a.vaTrigger
      repAndDur
      printProperty a.vaAudioAttach
      printProperty a.vaOther
    (VAlarmDisplay a) -> do
      ln $ bytestring "DISPLAY"
      printProperty a.vaTrigger
      printProperty a.vaDescription
      repAndDur
      printProperty a.vaOther
    (VAlarmEmail a) -> do
      ln $ bytestring "EMAIL"
      printProperty a.vaTrigger
      printProperty a.vaDescription
      printProperty a.vaSummary
      printProperty a.vaAttendee
      repAndDur
      printProperty a.vaMailAttach
      printProperty a.vaOther
    (VAlarmX a) -> do
      ln.out $ CI.original a.vaAction
      printProperty a.vaTrigger
      printProperty a.vaOther
  line "END:VALARM"
  where
  repAndDur = unless (va.vaRepeat == def) $ do
    printProperty $ va.vaRepeat
    unless (va.vaRepeat.repeatValue == 0) $
      for_ (va.vaDuration) printProperty

-- }}}
-- {{{ Property printers.

class IsProperty a where
  printProperty :: a -> ContentPrinter ()

instance IsProperty a => IsProperty (Set a) where
  printProperty = traverse_ printProperty

instance IsProperty a => IsProperty (Maybe a) where
  printProperty (Just x) = printProperty x
  printProperty _ = pure unit

instance (IsProperty a, IsProperty b) => IsProperty (Either a b) where
  printProperty (Left x) = printProperty x
  printProperty (Right x) = printProperty x

instance IsProperty FreeBusy where
  printProperty (FreeBusy a) = ln $ do
    prop "FREEBUSY" $ toParam a.freeBusyOther <> toParam a.freeBusyType
    printN printValue $ List.fromFoldable a.freeBusyPeriods

instance IsProperty PercentComplete where
  printProperty (PercentComplete a) = ln $ do
    prop "PERCENT-COMPLETE" a.percentCompleteOther
    printShow a.percentCompleteValue

instance IsProperty Completed where
  printProperty (Completed a) = ln $ do
    prop "COMPLETED" a.completedOther
      printValue
      a.completedValue

instance IsProperty DurationProp where
  printProperty (DurationProp a) = ln $ do
    prop "DURATION" a.durationOther
      printValue
      a.durationValue

instance IsProperty Repeat where
  printProperty (Repeat a) = ln $ do
    prop "REPEAT" a.repeatOther
      printShow
      a.repeatValue

instance IsProperty DTEnd where
  printProperty dtend = ln $ prop "DTEND" dtend *> printValue dtend

instance IsProperty Due where
  printProperty due = ln $ prop "DUE" due *> printValue due

instance IsProperty DTStamp where
  printProperty x = ln $ prop "DTSTAMP" x *> printValue x

instance IsProperty UID where
  printProperty (UID a) = ln $ prop "UID" a.uidOther *> text a.uidValue

instance IsProperty DTStart where
  printProperty x = ln $ prop "DTSTART" x *> printValue x

instance IsProperty Class where
  printProperty c@(Class a)
    | c == def = pure unit
    | otherwise = ln $ do
        prop "CLASS" a.classOther
        printValue a.classValue

instance IsProperty Created where
  printProperty (Created a) = ln $ do
    prop "CREATED" a.createdOther
    printUTCTime a.createdValue

instance IsProperty Description where
  printProperty (Description a) = ln $ do
    prop "DESCRIPTION" $ toParam (AltRep <$> a.descriptionAltRep)
      <> toParam a.descriptionLanguage
      <>
        toParam a.descriptionOther
    text a.descriptionValue

instance IsProperty Geo where
  printProperty (Geo a) = ln $ do
    prop "GEO".geoOther
    out <<< {-  undefined $ -}  printf "%.6f;%.6f" a.geoLat a.geoLong

instance IsProperty LastModified where
  printProperty (LastModified a) = ln $ do
    prop "LAST-MODIFIED" a.lastModifiedOther
    printUTCTime a.lastModifiedValue

instance IsProperty Location where
  printProperty (Location a) = ln $ do
    prop "LOCATION" $ toParam (AltRep <$> a.locationAltRep)
      <> toParam a.locationLanguage
      <> toParam a.locationOther
    text a.locationValue

instance IsProperty Organizer where
  printProperty (Organizer a) = ln $ do
    prop "ORGANIZER" $ toParam (CN <$> a.organizerCN)
      <> toParam (Dir <$> a.organizerDir)
      <> toParam (SentBy <$> a.organizerSentBy)
      <> toParam a.organizerLanguage
      <> toParam a.organizerOther
    printShow a.organizerValue

instance IsProperty Priority where
  printProperty x
    | x == def = pure unit
    | otherwise = ln $ do
        prop "PRIORITY" $ x.priorityOther
        printShow $ x.priorityValue

instance IsProperty Sequence where
  printProperty x
    | x == def = pure unit
    | otherwise = ln $ do
        prop "SEQUENCE" $ x.sequenceOther
        printShow $ x.sequenceValue

instance IsProperty EventStatus where
  printProperty s = ln $ do
    prop "STATUS" $ s.eventStatusOther
    printValue s

instance IsProperty TodoStatus where
  printProperty s = ln $ do
    prop "STATUS" $ s.todoStatusOther
    printValue s

instance IsProperty JournalStatus where
  printProperty s = ln $ do
    prop "STATUS" $ s.journalStatusOther
    printValue s

instance IsProperty Summary where
  printProperty (Summary a) = ln $ do
    prop "SUMMARY" $ toParam (AltRep <$> a.summaryAltRep)
      <> toParam a.summaryLanguage
      <> toParam a.summaryOther
    text a.summaryValue

instance IsProperty TimeTransparency where
  printProperty x
    | x == def = pure unit
    | otherwise = ln $ do
        prop "TRANSP" $ x.timeTransparencyOther
        printValue x

instance IsProperty URL where
  printProperty (URL a) = ln $ prop "URL" a.urlOther *> printShow a.urlValue

instance IsProperty RecurrenceId where
  printProperty r = ln $ prop "RECURRENCE-ID" r *> printValue r

instance IsProperty RRule where
  printProperty (RRule a) = ln $ do
    prop "RRULE" a.rRuleOther
      printValue
      a.rRuleValue

instance IsProperty Attachment where
  printProperty a = ln $ prop "ATTACH" a *> printValue a

instance IsProperty Attendee where
  printProperty att@(Attendee a) = ln $ do
    prop "ATTENDEE" att
    printValue a.attendeeValue

instance IsProperty Categories where
  printProperty (Categories a) = ln $ do
    prop "CATEGORIES" $ toParam a.categoriesOther <>
      toParam a.categoriesLanguage
    texts $ List.fromFoldable a.categoriesValues

instance IsProperty Comment where
  printProperty (Comment a) = ln $ do
    prop "COMMENT" $ toParam (AltRep <$> a.commentAltRep)
      <> toParam a.commentLanguage
      <>
        toParam a.commentOther
    text a.commentValue

instance IsProperty Contact where
  printProperty (Contact a) = ln $ do
    prop "CONTACT" $ toParam (AltRep <$> a.contactAltRep)
      <> toParam a.contactLanguage
      <>
        toParam a.contactOther
    text a.contactValue

instance IsProperty ExDate where
  printProperty exd = ln $ do
    prop "EXDATE" exd
    case exd of
      (ExDates a) -> printN printValue $ List.fromFoldable a.exDates
      (ExDateTimes a) -> printN printValue $ List.fromFoldable a.exDateTimes

instance IsProperty RequestStatus where
  printProperty (RequestStatus a) = ln $ do
    prop "REQUEST-STATUS" $ toParam a.requestStatusLanguage <>
      toParam a.requestStatusOther
    ( \z -> case z of
        (x : xs) -> do
          printShow x
          sequence_ $ map (\y -> putc '.' *> printShow y) xs
        [] -> pure unit
    ) a.requestStatusCode
    putc ';'
    text a.requestStatusDesc
    for_ a.requestStatusExt $ \x -> putc ';' *> text x

instance IsProperty RelatedTo where
  printProperty (RelatedTo a) = ln $ do
    prop "RELATED-TO" $ toParam a.relatedToOther <> toParam a.relatedToType
    text a.relatedToValue

instance IsProperty Resources where
  printProperty (Resources a) = ln $ do
    prop "RESOURCES" $ toParam (AltRep <$> a.resourcesAltRep)
      <> toParam a.aresourcesLanguage
      <> toParam a.resourcesOther
    texts $ List.fromFoldable a.resourcesValue

instance IsProperty RDate where
  printProperty r = ln $ prop "RDATE" r *> printValue r

instance IsProperty OtherProperty where
  printProperty (OtherProperty a) = ln $ do
    out (CI.original a.otherName)
    traverse_ param $ toParam a.otherParams
    out ":"
    bytestring a.otherValue

instance IsProperty Trigger where
  printProperty tr@(TriggerDuration a) = ln $ do
    prop "TRIGGER" tr
    printValue a.triggerDuration
  printProperty tr@(TriggerDateTime a) = ln $ do
    prop "TRIGGER" tr
    printUTCTime a.triggerDateTime

-- | Print a generic property.
prop
  :: ToParam a
  => ByteString
  -> a
  -> ContentPrinter ()
prop b x = do
  put (BS.length b)
  -- tell (Bu.lazyByteString b) TODO
  traverse_ param $ toParam x
  out ":"

-- }}}
-- {{{ Parameter "printers".

class ToParam a where
  toParam :: a -> List (T2 Text (List (T2 Quoting Text)))

instance ToParam a => ToParam (Maybe a) where
  toParam Nothing = []
  toParam (Just x) = toParam x

instance ToParam a => ToParam (Set a) where
  toParam s = case S.findMax s of
    Nothing -> []
    Just (Tuple x _) -> toParam x

instance ToParam ExDate where
  toParam (ExDates a) = [ ("VALUE" /\ [ (NoQuotes /\ "DATE") ]) ] <>
    toParam a.exDateOther
  toParam (ExDateTimes a) = toParam a.exDateOther <>
    toParam (fst <$> S.findMax a.exDateTimes)

instance ToParam AltRep where
  toParam (AltRep x) = [ ("ALTREP" /\ [ (NeedQuotes /\ {-T.pack-}  show x) ]) ]

instance ToParam SentBy where
  toParam (SentBy x) = [ ("SENT-BY" /\ [ (NeedQuotes /\ {-T.pack-}  show x) ]) ]

instance ToParam Dir where
  toParam (Dir x) = [ ("DIR" /\ [ (NeedQuotes /\ {-T.pack-}  show x) ]) ]

instance ToParam DateTime where
  toParam (ZonedDateTime a) = [ ("TZID" /\ [ (NoQuotes /\ a.dateTimeZone) ]) ]
  toParam _ = []

instance ToParam DTEnd where
  toParam (DTEndDateTime a) = toParam a.dtEndOther <>
    toParam a.dtEndDateTimeValue
  toParam (DTEndDate a) = [ ("VALUE" /\ [ (NoQuotes /\ "DATE") ]) ] <>
    toParam a.dtEndOther

instance ToParam Due where
  toParam (DueDateTime a) = toParam a.dueOther <> toParam a.dueDateTimeValue
  toParam (DueDate a) = [ ("VALUE" /\ [ (NoQuotes /\ "DATE") ]) ] <>
    toParam a.dueOther

instance ToParam CN where
  toParam (CN x) = [ ("CN" /\ [ (Optional /\ x) ]) ]

instance ToParam DTStart where
  toParam (DTStartDateTime a) = toParam a.dtStartDateTimeValue <>
    toParam a.dtStartOther
  toParam (DTStartDate a) = [ ("VALUE" /\ [ (NoQuotes /\ "DATE") ]) ] <>
    toParam a.dtStartOther

instance ToParam RDate where
  toParam (RDateDates a) = [ ("VALUE" /\ [ (NoQuotes /\ "DATE") ]) ] <>
    toParam a.rDateOther
  toParam (RDatePeriods a) = [ ("VALUE" /\ [ (NoQuotes /\ "PERIOD") ]) ]
    <> toParam a.rDateOther
    <>
      toParam (fst <$> S.findMax a.rDatePeriods)
  toParam (RDateDateTimes a) = toParam a.rDateDateTimes <> toParam a.rDateOther

instance ToParam Period where
  toParam (PeriodDates x _) = toParam x
  toParam (PeriodDuration x _) = toParam x

instance ToParam DTStamp where
  toParam (DTStamp a) = toParam a.dtStampOther

instance ToParam OtherParams where
  toParam (OtherParams l) = fromOP <$> List.fromFoldable l
    where
    fromOP (OtherParam x y) = (CI.original x /\ (\v -> Optional /\ v) <$> y)

instance ToParam Language where
  toParam (Language x) = [ ("LANGUAGE" /\ [ (Optional /\ CI.original x) ]) ]

instance ToParam TZName where
  toParam (TZName a) = toParam a.tzNameLanguage <> toParam a.tzNameOther

instance ToParam x => ToParam (List x) where
  toParam = List.concat <<< map toParam

instance ToParam (Text /\ (List (Quoting /\ Text))) where
  toParam = (\x -> x : Nil)

instance ToParam RecurrenceId where
  toParam (RecurrenceIdDate a) = [ ("VALUE" /\ [ (NoQuotes /\ "DATE") ]) ]
    <> toParam a.recurrenceIdRange
    <>
      toParam a.recurrenceIdOther
  toParam (RecurrenceIdDateTime a) = toParam a.recurrenceIdDateTime
    <> toParam a.recurrenceIdRange
    <>
      toParam a.recurrenceIdOther

instance ToParam Range where
  toParam ThisAndFuture = [ ("RANGE" /\ [ (NoQuotes /\ "THISANDFUTURE") ]) ]
  toParam _ = [] -- ThisAndPrior MUST NOT be generated.

instance ToParam FBType where
  toParam x | x == def = []
  toParam Free = [ ("FBTYPE" /\ [ (NoQuotes /\ "FREE") ]) ]
  toParam Busy = [ ("FBTYPE" /\ [ (NoQuotes /\ "BUSY") ]) ]
  toParam BusyUnavailable = [ ("FBTYPE" /\ [ (NoQuotes /\ "BUSY-UNAVAILABLE") ]) ]
  toParam BusyTentative = [ ("FBTYPE" /\ [ (NoQuotes /\ "BUSY-TENTATIVE") ]) ]
  toParam (FBTypeX x) = [ ("FBTYPE" /\ [ (Optional /\ CI.original x) ]) ]

instance ToParam MIMEType where
  toParam m = [ ("FMTTYPE" /\ [ (NoQuotes /\ {-T.fromStrict $-}  showMIMEType m) ]) ]

instance ToParam Attachment where
  toParam (UriAttachment a) = toParam a.attachFmtType <>
    toParam a.attachOther
  toParam (BinaryAttachment a) = toParam a.attachFmtType
    <> toParam.attachOther
    <>
      [ ("VALUE" /\ [ (NoQuotes /\ "BINARY") ])
      , ("ENCODING" /\ [ (NoQuotes /\ "BASE64") ])
      ]

instance ToParam CUType where
  toParam x | x == def = []
  toParam Individual = [ ("CUTYPE" /\ [ (NoQuotes /\ "INDIVIDUAL") ]) ]
  toParam Group = [ ("CUTYPE" /\ [ (NoQuotes /\ "GROUP") ]) ]
  toParam Resource = [ ("CUTYPE" /\ [ (NoQuotes /\ "RESOURCE") ]) ]
  toParam Room = [ ("CUTYPE" /\ [ (NoQuotes /\ "ROOM") ]) ]
  toParam Unknown = [ ("CUTYPE" /\ [ (NoQuotes /\ "UNKNOWN") ]) ]
  toParam (CUTypeX x) = [ ("CUTYPE" /\ [ (Optional /\ CI.original x) ]) ]

instance ToParam Member where
  toParam (Member x) | S.isEmpty x = []
  toParam (Member x) =
    [ ( "MEMBER"
          /\ (\v -> NeedQuotes /\ v) <<< {-T.pack <<< -}  show <$> S.fromFoldable x
      )
    ]

instance ToParam Role where
  toParam x | x == def = []
  toParam Chair = [ ("ROLE" /\ [ (NoQuotes /\ "CHAIR") ]) ]
  toParam ReqParticipant = [ ("ROLE" /\ [ (NoQuotes /\ "REQ-PARTICIPANT") ]) ]
  toParam OptParticipant = [ ("ROLE" /\ [ (NoQuotes /\ "OPT-PARTICIPANT") ]) ]
  toParam NonParticipant = [ ("ROLE" /\ [ (NoQuotes /\ "NON-PARTICIPANT") ]) ]
  toParam (RoleX x) = [ ("ROLE" /\ [ (Optional /\ CI.original x) ]) ]

instance ToParam PartStat where
  toParam x | x == def = []
  toParam PartStatNeedsAction = [ ("PARTSTAT" /\ [ (NoQuotes /\ "NEEDS-ACTION") ]) ]
  toParam Accepted = [ ("PARTSTAT" /\ [ (NoQuotes /\ "ACCEPTED") ]) ]
  toParam Declined = [ ("PARTSTAT" /\ [ (NoQuotes /\ "DECLINED") ]) ]
  toParam Tentative = [ ("PARTSTAT" /\ [ (NoQuotes /\ "TENTATIVE") ]) ]
  toParam Delegated = [ ("PARTSTAT" /\ [ (NoQuotes /\ "DELEGATED") ]) ]
  toParam PartStatCompleted = [ ("PARTSTAT" /\ [ (NoQuotes /\ "COMPLETED") ]) ]
  toParam InProcess = [ ("PARTSTAT" /\ [ (NoQuotes /\ "IN-PROCESS") ]) ]
  toParam (PartStatX x) = [ ("PARTSTAT" /\ [ (Optional /\ CI.original x) ]) ]

instance ToParam RelationshipType where
  toParam x | x == def = []
  toParam Parent = [ ("RELTYPE" /\ [ (NoQuotes /\ "PARENT") ]) ]
  toParam Child = [ ("RELTYPE" /\ [ (NoQuotes /\ "CHILD") ]) ]
  toParam Sibling = [ ("RELTYPE" /\ [ (NoQuotes /\ "SIBLING") ]) ]
  toParam (RelationshipTypeX x) = [ ("RELTYPE" /\ [ (Optional /\ CI.original x) ]) ]

instance ToParam RSVP where
  toParam (RSVP false) = []
  toParam (RSVP true) = [ ("RSVP" /\ [ (NoQuotes /\ "TRUE") ]) ]

instance ToParam DelTo where
  toParam (DelTo x)
    | S.isEmpty = []
    | otherwise =
        [ ( "DELEGATED-TO"
              /\ (\v -> NeedQuotes /\ v) <<< {-T.pack <<< -}  show
              <$> S.fromFoldable x
          )
        ]

instance ToParam DelFrom where
  toParam (DelFrom x)
    | S.isEmpty x = []
    | otherwise =
        [ ( "DELEGATED-FROM"
              /\ (\v -> NeedQuotes /\ v) <<< {-T.pack <<< -}  show
              <$> S.fromFoldable x
          )
        ]

instance ToParam Attendee where
  toParam (Attendee a) = toParam a.attendeeCUType
    <> toParam (Member a.attendeeMember)
    <> toParam a.attendeeRole
    <> toParam a.attendeePartStat
    <> toParam (RSVP a.attendeeRSVP)
    <> toParam (DelTo a.attendeeDelTo)
    <> toParam (DelFrom a.attendeeDelFrom)
    <> toParam (SentBy <$> a.attendeeSentBy)
    <> toParam (CN <$> a.attendeeCN)
    <> toParam (Dir <$> a.attendeeDir)
    <> toParam a.attendeeLanguage
    <>
      toParam a.attendeeOther

instance ToParam AlarmTriggerRelationship where
  toParam x | x == def = []
  toParam Start = [ ("RELATED" /\ [ (NoQuotes /\ "START") ]) ]
  toParam End = [ ("RELATED" /\ [ (NoQuotes /\ "END") ]) ]

instance ToParam Trigger where
  toParam (TriggerDuration a) = toParam a.triggerOther <>
    toParam a.triggerRelated
  toParam (TriggerDateTime a) = toParam a.triggerOther <>
    [ ("VALUE" /\ [ (NoQuotes /\ "DATE-TIME") ]) ]

-- }}}
-- {{{ Value printers

printUTCOffset :: Int -> ContentPrinter ()
printUTCOffset n = do
  case signum n of
    -1 -> putc '-'
    _ -> putc '+'
  out <<< {-T.pack $ -}  printf "%02d" t
  out <<< {-T.pack $ -}  printf "%02d" m
  when (s > 0) <<< out <<< {-T.pack $ -}  printf "%02d" s
  where
  (m' /\ s) = abs n `divMod` 60
  (t /\ m) = m' `divMod` 60

printNWeekday :: Either (Int /\ Weekday) Weekday -> ContentPrinter ()
printNWeekday (Left (n /\ w)) = printShow n *> printValue w
printNWeekday (Right x) = printValue x

printShow :: Show a => a -> ContentPrinter ()
printShow = out <<< {-T.pack <<< -}  show

printShowN :: Show a => (List a) -> ContentPrinter ()
printShowN = printN printShow

printN :: (a -> ContentPrinter ()) -> (List a) -> ContentPrinter ()
printN m (x : xs) = m x *> sequence_ $ map (\x' -> putc ',' *> m x') xs
printN _ _ = pure Unit

printShowUpper :: Show a => a -> ContentPrinter ()
printShowUpper = out <<< {-T.pack <<< -} map toUpper <<< show

printUTCTime :: Time.UTCTime -> ContentPrinter ()
printUTCTime = out <<< {-T.pack <<< -}  formatTime "%C%y%m%dT%H%M%SZ"

class IsValue a where
  printValue :: a -> ContentPrinter ()

instance IsValue ICalVersion where
  printValue (MaxICalVersion a) = out <<< {-T.pack $ -}  Ver.showVersion versionMax
  printValue (MinMaxICalVersion a) = do
    out <<< {-T.pack $ -}  Ver.showVersion versionMin
    putc ';'
    out <<< {-T.pack $ -}  Ver.showVersion versionMax

instance IsValue Recur where
  printValue (Recur a) = do
    out "FREQ="
    printShowUpper recurFreq
    for_ recurUntilCount $ \x ->
      case x of
        Left y -> out ";UNTIL=" *> printValue y
        Right y -> out ";COUNT=" *> printShow y
    when (recurInterval /= 1) $
      out ";INTERVAL=" *> printShow recurInterval
    unless (null recurBySecond) $
      out ";BYSECOND=" *> printShowN recurBySecond
    unless (null recurByMinute) $
      out ";BYMINUTE=" *> printShowN recurByMinute
    unless (null recurByHour) $
      out ";BYHOUR=" *> printShowN recurByHour
    unless (null recurByDay) $
      out ";BYDAY=" *> printN printNWeekday recurByDay
    unless (null recurByMonthDay) $
      out ";BYMONTHDAY=" *> printShowN recurByMonthDay
    unless (null recurByYearDay) $
      out ";BYYEARDAY=" *> printShowN recurByYearDay
    unless (null recurByWeekNo) $
      out ";BYWEEKNO=" *> printShowN recurByWeekNo
    unless (null recurByMonth) $
      out ";BYMONTH=" *> printShowN recurByMonth
    unless (null recurBySetPos) $
      out ";BYSETPOS=" *> printShowN recurBySetPos
    unless (recurWkSt == Monday) $
      out ";WKST=" *> printValue recurWkSt

instance IsValue TimeTransparency where
  printValue Opaque {} = out "OPAQUE"
  printValue Transparent {} = out "TRANSPARENT"

instance IsValue DTEnd where
  printValue (DTEndDateTime a) = printValue dtEndDateTimeValue
  printValue (DTEndDate a) = printValue dtEndDateValue

instance IsValue Due where
  printValue (DueDateTime a) = printValue dueDateTimeValue
  printValue (DueDate a) = printValue dueDateValue

instance IsValue EventStatus where
  printValue TentativeEvent {} = out "TENTATIVE"
  printValue ConfirmedEvent {} = out "CONFIRMED"
  printValue CancelledEvent {} = out "CANCELLED"

instance IsValue TodoStatus where
  printValue TodoNeedsAction {} = out "NEEDS-ACTION"
  printValue CompletedTodo {} = out "COMPLETED"
  printValue InProcessTodo {} = out "IN-PROCESS"
  printValue CancelledTodo {} = out "CANCELLED"

instance IsValue JournalStatus where
  printValue DraftJournal {} = out "DRAFT"
  printValue FinalJournal {} = out "FINAL"
  printValue CancelledJournal {} = out "CANCELLED"

instance IsValue ClassValue where
  printValue (ClassValueX x) = out $ CI.original x
  printValue x = printShowUpper x

instance IsValue Weekday where
  printValue Sunday = out "SU"
  printValue Monday = out "MO"
  printValue Tuesday = out "TU"
  printValue Wednesday = out "WE"
  printValue Thursday = out "TH"
  printValue Friday = out "FR"
  printValue Saturday = out "SA"

instance IsValue Date where
  printValue (Date a) = out <<< {-T.pack $ -}  formatTime "%C%y%m%d" dateValue

instance IsValue DateTime where
  printValue (FloatingDateTime a) =
    out <<< {-T.pack $ -}  formatTime "%C%y%m%dT%H%M%S" dateTimeFloating
  printValue (UTCDateTime a) = printUTCTime dateTimeUTC
  printValue (ZonedDateTime a) =
    out <<< {-T.pack $ -}  formatTime "%C%y%m%dT%H%M%S" dateTimeFloating

instance IsValue (Either Date DateTime) where
  printValue (Left x) = printValue x
  printValue (Right x) = printValue x

instance IsValue DTStamp where
  printValue (DTStamp a) = printUTCTime dtStampValue

instance IsValue DTStart where
  printValue (DTStartDateTime a) = printValue dtStartDateTimeValue
  printValue (DTStartDate a) = printValue dtStartDateValue

instance IsValue URI.URI where
  printValue = printShow

instance IsValue Duration where
  printValue (DurationDate a) = do
    when (durSign == Negative) $ putc '-'
    putc 'P'
    printShow durDay *> putc 'D'
    putc 'T'
    printShow durHour *> putc 'H'
    printShow durMinute *> putc 'M'
    printShow durSecond *> putc 'S'
  printValue (DurationTime a) = do
    when (durSign == Negative) $ putc '-'
    out "PT"
    printShow durHour *> putc 'H'
    printShow durMinute *> putc 'M'
    printShow durSecond *> putc 'S'
  printValue (DurationWeek a) = do
    when (durSign == Negative) $ putc '-'
    out "P"
    printShow durWeek *> putc 'W'

instance IsValue RecurrenceId where
  printValue (RecurrenceIdDate a) = printValue recurrenceIdDate
  printValue (RecurrenceIdDateTime a) = printValue recurrenceIdDateTime

instance IsValue Period where
  printValue (PeriodDates f t) = printValue f *> putc '/' *> printValue t
  printValue (PeriodDuration f d) = printValue f *> putc '/' *> printValue d

instance IsValue UTCPeriod where
  printValue (UTCPeriodDates f t) = printUTCTime f *> putc '/' *> printUTCTime t
  printValue (UTCPeriodDuration f d) = printUTCTime f *> putc '/' *> printValue d

instance IsValue RDate where
  printValue (RDateDates a) = printN printValue $ S.fromFoldable rDateDates
  printValue (RDateDateTimes a) = printN printValue $ S.fromFoldable rDateDateTimes
  printValue (RDatePeriods a) = printN printValue $ S.fromFoldable rDatePeriods

instance IsValue Attachment where
  printValue (UriAttachment a) = printShow attachUri
  printValue (BinaryAttachment a) = bytestring $ B64.encode attachContent

-- }}}
-- {{{ Lib

ln :: ContentPrinter () -> ContentPrinter ()
ln x = x *> newline

param :: (Text /\ (List (Quoting /\ Text))) -> ContentPrinter ()
param (n /\ xs) = putc ';' *> out n *> putc '=' *> paramVals xs

paramVals :: (List (Quoting /\ Text)) -> ContentPrinter ()
paramVals (x : xs) = paramVal x *> sequence_ $ map (\x' -> putc ',' *> paramVal x') xs
paramVals _ = pure Unit

paramVal :: (Quoting /\ Text) -> ContentPrinter ()
paramVal (NeedQuotes /\ t) = putc '"' *> out t *> putc '"'
paramVal (NoQuotes /\ t) = out t
paramVal (_ /\ t) = paramVal (NeedQuotes /\ t)

texts :: (List Text) -> ContentPrinter ()
texts (x : xs) = text x *> sequence_ $ map (\x -> putc ',' *> text x') xs
texts _ = pure Unit

text :: Text -> ContentPrinter ()
text t = case T.uncons t of
  Just (';' /\ r) -> out "\\;" *> text r
  Just ('\n' /\ r) -> out "\\n" *> text r
  Just (',' /\ r) -> out "\\," *> text r
  Just ('\\' /\ r) -> out "\\\\" *> text r
  Just (c /\ r) -> putc c *> text r
  Nothing -> pure Unit

bytestring :: ByteString -> ContentPrinter ()
bytestring = BS.foldl' (\m c -> m *> putc8 c) (pure Unit)

out :: Text -> ContentPrinter ()
out t = case T.uncons t of
  Just (c /\ r) -> putc c *> out r
  Nothing -> pure Unit

putc :: Char -> ContentPrinter ()
putc c = do
  x <- get
  (b /\ clen) <- asks (efChar2Bu &&& efChar2Len)
  let cl = clen c
  when (x + cl > 75) foldLine
  tell $ b c
  modify ((+) cl)

putc8 :: Char -> ContentPrinter ()
putc8 c = do
  x <- get
  when (x >= 75) foldLine
  tell $ Bu.char8 c
  modify ((+) 1)

foldLine :: ContentPrinter ()
foldLine = tell (Bu.byteString "\r\n ") *> put 1

newline :: ContentPrinter ()
newline = tell (Bu.byteString "\r\n") *> put 0

-- | Output a whole line. Must be less than 75 bytes.
line :: ByteString -> ContentPrinter ()
line b = tell (Bu.lazyByteString b) *> newline

formatTime :: FormatTime t => String -> t -> String
formatTime = Time.formatTime defaultTimeLocale

-- }}}
