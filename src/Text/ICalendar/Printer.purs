module Text.ICalendar.Printer
  ( EncodingFunctions(..)
  , Builder
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

import Codec.MIME.Type (MIMEType, showMIMEType)
import Control.Monad.RWS (get, put, tell, RWS, asks, modify, runRWS)
import Data.Binary.Base64 as B64
import Data.CaseInsensitive (CI(..))
import Data.CaseInsensitive as CI
import Data.DateTime as DT
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (for_, sequence_, traverse_)
import Data.Foldable as Foldable
import Data.Formatter.DateTime as DF
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord (abs, signum)
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as S
import Data.String (toUpper)
import Data.String as BS
import Data.String as T
import Data.String.CodePoints (codePointFromChar, singleton)
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits as CodeUnits
import Data.Time as Time
import Data.Unit (Unit(..))
import Text.ICalendar.Types as Types
import Text.ICalendar.Types as Ver
import URI.URI as URI

data UPrintf = UPrintf -- TODO 

class PrintfType t where -- TODO
  spr :: String -> List UPrintf -> t

printf :: forall t. PrintfType t => String -> t -- TODO
printf fmts = spr fmts Nil

type Builder = String -- TODO

type ByteString = String -- TODO

lsing :: forall a. a -> List a
lsing = List.singleton

-- | Functions for encoding into bytestring builders.
data EncodingFunctions = EncodingFunctions
  { efChar2Bu :: Char -> Builder
  , efChar2Len :: Char -> Int -- ^ How many octets the character is encoded.
  }

divMod :: Int -> Int -> Int /\ Int
divMod a b = l /\ r
  where
  l = div a b
  r = mod a b

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

newtype AltRep = AltRep UriM
newtype CN = CN Text
newtype Dir = Dir UriM
newtype Member = Member (Set UriM)
newtype DelTo = DelTo (Set UriM)
newtype DelFrom = DelFrom (Set UriM)
newtype RSVP = RSVP Boolean
newtype SentBy = SentBy CalAddress

data Quoting = NeedQuotes | Optional | NoQuotes

derive instance Eq Quoting
derive instance Ord Quoting

-- TODO
-- | UTF8.
-- instance Default EncodingFunctions where
--   def = EncodingFunctions Bu.charUtf8
--     utf8Len

type ContentPrinter a = RWS EncodingFunctions Builder Int a

-- TODO
-- | Print a VCalendar object to a ByteString.
-- printICalendar :: EncodingFunctions -> VCalendar -> ByteString
-- printICalendar r v = (\(_ /\ _ /\ x) -> Bu.toLazyByteString x) $
--   runRWS (printVCalendar v) r 0

-- {{{ Component printers

printVCalendar :: VCalendar -> ContentPrinter Unit
printVCalendar (VCalendar a) = do
  line "BEGIN:VCALENDAR"
  ln $ do
    prop "VERSION" $ versionOther a.vcVersion -- Should be first for
    printValue a.vcVersion -- compatibility.
  ln $ do
    prop "PRODID" $ prodIdOther a.vcProdId
    text $ prodIdValue a.vcProdId
  ln $ do
    prop "CALSCALE" $ scaleOther a.vcScale
    text <<< CI.original $ scaleValue a.vcScale
  for_ a.vcMethod $ \(Method meth) -> do
    prop "METHOD" $ meth.methodOther
    ln <<< text <<< CI.original $ meth.methodValue
  traverse_ printProperty a.vcOther
  traverse_ printVTimeZone a.vcTimeZones
  traverse_ printVEvent a.vcEvents
  traverse_ printVTodo a.vcTodos
  traverse_ printVJournal a.vcJournals
  traverse_ printVFreeBusy a.vcFreeBusys
  traverse_ printVOther a.vcOtherComps
  line "END:VCALENDAR"

printVTimeZone :: VTimeZone -> ContentPrinter Unit
printVTimeZone ( {-VTimeZone-} a) = do
  line "BEGIN:VTIMEZONE"
  ln $ do
    prop "TZID" $ a.vtzId.tzidOther
    text $ a.vtzId.tzidValue
  printProperty a.vtzLastMod
  for_ a.vtzUrl $ \url -> do
    prop "TZURL" $ url.tzUrlOther
    ln <<< printShow $ url.tzUrlValue
  traverse_ (printTZProp "STANDARD") a.vtzStandardC
  traverse_ (printTZProp "DAYLIGHT") a.vtzDaylightC
  traverse_ printProperty a.vtzOther
  line "END:VTIMEZONE"

printTZProp :: ByteString -> TZProp -> ContentPrinter Unit
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

printVEvent :: VEvent -> ContentPrinter Unit
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

printVTodo :: VTodo -> ContentPrinter Unit
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

printVJournal :: VJournal -> ContentPrinter Unit
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

printVFreeBusy :: VFreeBusy -> ContentPrinter Unit
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

printVOther :: VOther -> ContentPrinter Unit
printVOther (VOther a) = do
  ln $ out $ "BEGIN:V" <> CI.original a.voName
  traverse_ printProperty a.voProps
  ln $ out $ "END:V" <> CI.original a.voName

printVAlarm :: VAlarm -> ContentPrinter Unit
printVAlarm va = do
  line "BEGIN:VALARM"
  case va of
    (VAlarmAudio a) -> do
      prop "ACTION" $ a.vaActionOther
      ln $ bytestring "AUDIO"
      printProperty a.vaTrigger
      repAndDur a.vaRepeat a.vaDuration
      printProperty a.vaAudioAttach
      printProperty a.vaOther
    (VAlarmDisplay a) -> do
      prop "ACTION" $ a.vaActionOther
      ln $ bytestring "DISPLAY"
      printProperty a.vaTrigger
      printProperty a.vaDescription
      repAndDur a.vaRepeat a.vaDuration
      printProperty a.vaOther
    (VAlarmEmail a) -> do
      prop "ACTION" $ a.vaActionOther
      ln $ bytestring "EMAIL"
      printProperty a.vaTrigger
      printProperty a.vaDescription
      printProperty a.vaSummary
      printProperty a.vaAttendee
      repAndDur a.vaRepeat a.vaDuration
      printProperty a.vaMailAttach
      printProperty a.vaOther
    (VAlarmX a) -> do
      prop "ACTION" $ a.vaActionOther
      ln <<< out $ CI.original a.vaAction
      printProperty a.vaTrigger
      printProperty a.vaOther
  line "END:VALARM"
  where
  repAndDur :: Repeat -> Maybe DurationProp -> ContentPrinter Unit
  repAndDur vaRepeat@(Repeat r) vaDuration = unless (vaRepeat == def) $ do
    printProperty $ vaRepeat
    unless (r.repeatValue == 0) $
      for_ (vaDuration) printProperty

-- }}}
-- {{{ Property printers.

class IsProperty a where
  printProperty :: a -> ContentPrinter Unit

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
    printValue a.completedValue

instance IsProperty DurationProp where
  printProperty (DurationProp a) = ln $ do
    prop "DURATION" a.durationOther
    printValue a.durationValue

instance IsProperty Repeat where
  printProperty (Repeat a) = ln $ do
    prop "REPEAT" a.repeatOther
    printShow a.repeatValue

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
    prop "GEO" a.geoOther
    -- TODO
    -- out <<< {-  undefined $ -}  printf "%.6f;%.6f" a.geoLat a.geoLong
    pure unit

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
  printProperty p@(Priority x)
    | p == def = pure unit
    | otherwise = ln $ do
        prop "PRIORITY" $ x.priorityOther
        printShow $ x.priorityValue

instance IsProperty Sequence where
  printProperty s@(Sequence x)
    | s == def = pure unit
    | otherwise = ln $ do
        prop "SEQUENCE" $ x.sequenceOther
        printShow $ x.sequenceValue

instance IsProperty EventStatus where
  printProperty s = case s of
    TentativeEvent a -> f a
    ConfirmedEvent a -> f a
    CancelledEvent a -> f a
    where
    f b = ln $ do
      prop "STATUS" $ b.eventStatusOther
      printValue s

instance IsProperty TodoStatus where
  printProperty s = ln $ do
    prop "STATUS" $ todoStatusOther s
    printValue s

instance IsProperty JournalStatus where
  printProperty s = case s of
    DraftJournal a -> f a
    FinalJournal a -> f a
    CancelledJournal a -> f a
    where
    f b =
      ln $ do
        prop "STATUS" $ b.journalStatusOther
        printValue s

instance IsProperty Summary where
  printProperty (Summary a) = ln $ do
    prop "SUMMARY" $ toParam (AltRep <$> a.summaryAltRep)
      <> toParam a.summaryLanguage
      <> toParam a.summaryOther
    text a.summaryValue

instance IsProperty TimeTransparency where
  printProperty t
    | t == def = pure unit
    | otherwise =
        ln $ do
          prop "TRANSP" $ x.timeTransparencyOther
          printValue t
        where
        x = case t of
          Opaque a -> a
          Transparent a -> a

instance IsProperty URL where
  printProperty (URL a) = ln $ prop "URL" a.urlOther *> printShow a.urlValue

instance IsProperty RecurrenceId where
  printProperty r = ln $ prop "RECURRENCE-ID" r *> printValue r

instance IsProperty RRule where
  printProperty (RRule a) = ln $ do
    prop "RRULE" a.rRuleOther
    printValue a.rRuleValue

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
        Nil -> pure unit
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
      <> toParam a.resourcesLanguage
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
  :: forall a
   . ToParam a
  => ByteString
  -> a
  -> ContentPrinter Unit
prop b x = do
  put (BS.length b)
  -- TODO  
  -- tell (Bu.lazyByteString b) 
  traverse_ param $ toParam x
  out ":"

-- }}}
-- {{{ Parameter "printers".

class ToParam a where
  toParam :: a -> List (Text /\ (List (Quoting /\ Text)))

instance ToParam a => ToParam (Maybe a) where
  toParam Nothing = Nil
  toParam (Just x) = toParam x

instance ToParam a => ToParam (Set a) where
  toParam s = case S.findMax s of
    Nothing -> Nil
    Just x -> toParam x

instance ToParam ExDate where
  toParam (ExDates a) = List.fromFoldable [ ("VALUE" /\ lsing ((NoQuotes /\ "DATE"))) ] <>
    toParam a.exDateOther
  toParam (ExDateTimes a) = toParam a.exDateOther <>
    toParam (S.findMax a.exDateTimes)

instance ToParam AltRep where
  toParam (AltRep x) = List.fromFoldable [ ("ALTREP" /\ List.fromFoldable [ (NeedQuotes /\ {-T.pack-}  show x) ]) ]

instance ToParam SentBy where
  toParam (SentBy x) = b
    where
    a = (List.fromFoldable [ (NeedQuotes /\ {-T.pack-}  show x) ])
    b = List.fromFoldable [ ("SENT-BY" /\ a) ]

instance ToParam Dir where
  toParam (Dir x) = lsing (("DIR" /\ lsing ((NeedQuotes /\ {-T.pack-}  show x))))

instance ToParam ICalDateTime where
  toParam (ZonedDateTime a) = lsing (("TZID" /\ lsing ((NoQuotes /\ a.dateTimeZone))))
  toParam _ = Nil

instance ToParam DTEnd where
  toParam (DTEndDateTime a) = toParam a.dtEndOther <>
    toParam a.dtEndDateTimeValue
  toParam (DTEndDate a) = lsing (("VALUE" /\ lsing ((NoQuotes /\ "DATE")))) <>
    toParam a.dtEndOther

instance ToParam Due where
  toParam (DueDateTime a) = toParam a.dueOther <> toParam a.dueDateTimeValue
  toParam (DueDate a) = lsing (("VALUE" /\ lsing ((NoQuotes /\ "DATE")))) <>
    toParam a.dueOther

instance ToParam CN where
  toParam (CN x) = lsing (("CN" /\ lsing ((Optional /\ x))))

instance ToParam DTStart where
  toParam (DTStartDateTime a) = toParam a.dtStartDateTimeValue <>
    toParam a.dtStartOther
  toParam (DTStartDate a) = lsing (("VALUE" /\ lsing ((NoQuotes /\ "DATE")))) <>
    toParam a.dtStartOther

instance ToParam RDate where
  toParam (RDateDates a) = lsing (("VALUE" /\ lsing ((NoQuotes /\ "DATE")))) <>
    toParam a.rDateOther
  toParam (RDatePeriods a) = lsing (("VALUE" /\ lsing ((NoQuotes /\ "PERIOD"))))
    <> toParam a.rDateOther
    <>
      toParam (S.findMax a.rDatePeriods)
  toParam (RDateDateTimes a) = toParam a.rDateDateTimes <> toParam a.rDateOther

instance ToParam Period where
  toParam (PeriodDates x _) = toParam x
  toParam (PeriodDuration x _) = toParam x

instance ToParam DTStamp where
  toParam (DTStamp a) = toParam a.dtStampOther

instance ToParam OtherParams where
  toParam (OtherParams l) = Nil -- map fromOP val
    where
    fromOP :: OtherParam -> List _
    fromOP (OtherParam (CI x) y) = lsing (x.original /\ (map (\v -> Optional /\ v) y))
    val = List.fromFoldable l

instance ToParam Language where
  toParam (Language x) = lsing (("LANGUAGE" /\ lsing ((Optional /\ CI.original x))))

instance ToParam TZName where
  toParam (TZName a) = toParam a.tzNameLanguage <> toParam a.tzNameOther

instance ToParam x => ToParam (List x) where
  toParam = List.concat <<< map toParam

instance ToParam (Text /\ (List (Quoting /\ Text))) where
  toParam = (\x -> x : Nil)

instance ToParam RecurrenceId where
  toParam (RecurrenceIdDate a) = lsing (("VALUE" /\ lsing ((NoQuotes /\ "DATE"))))
    <> toParam a.recurrenceIdRange
    <>
      toParam a.recurrenceIdOther
  toParam (RecurrenceIdDateTime a) = toParam a.recurrenceIdDateTime
    <> toParam a.recurrenceIdRange
    <>
      toParam a.recurrenceIdOther

instance ToParam Range where
  toParam ThisAndFuture = lsing (("RANGE" /\ lsing ((NoQuotes /\ "THISANDFUTURE"))))
  toParam _ = Nil -- ThisAndPrior MUST NOT be generated.

instance ToParam FBType where
  toParam x | x == def = Nil
  toParam Free = lsing (("FBTYPE" /\ lsing ((NoQuotes /\ "FREE"))))
  toParam Busy = lsing (("FBTYPE" /\ lsing ((NoQuotes /\ "BUSY"))))
  toParam BusyUnavailable = lsing (("FBTYPE" /\ lsing ((NoQuotes /\ "BUSY-UNAVAILABLE"))))
  toParam BusyTentative = lsing (("FBTYPE" /\ lsing ((NoQuotes /\ "BUSY-TENTATIVE"))))
  toParam (FBTypeX x) = lsing (("FBTYPE" /\ lsing ((Optional /\ CI.original x))))

instance ToParam MIMEType where
  toParam m = lsing (("FMTTYPE" /\ lsing ((NoQuotes /\ {-T.fromStrict $-}  showMIMEType m))))

instance ToParam Attachment where
  toParam (UriAttachment a) = toParam a.attachFmtType <>
    toParam a.attachOther
  toParam (BinaryAttachment a) = toParam a.attachFmtType
    <> toParam a.attachOther
    <>
      List.fromFoldable
        [ ("VALUE" /\ lsing ((NoQuotes /\ "BINARY")))
        , ("ENCODING" /\ lsing ((NoQuotes /\ "BASE64")))
        ]

instance ToParam CUType where
  toParam x | x == def = Nil
  toParam Individual = lsing (("CUTYPE" /\ lsing ((NoQuotes /\ "INDIVIDUAL"))))
  toParam Group = lsing (("CUTYPE" /\ lsing ((NoQuotes /\ "GROUP"))))
  toParam Resource = lsing (("CUTYPE" /\ lsing ((NoQuotes /\ "RESOURCE"))))
  toParam Room = lsing (("CUTYPE" /\ lsing ((NoQuotes /\ "ROOM"))))
  toParam Unknown = lsing (("CUTYPE" /\ lsing ((NoQuotes /\ "UNKNOWN"))))
  toParam (CUTypeX x) = lsing (("CUTYPE" /\ lsing ((Optional /\ CI.original x))))

instance ToParam Member where
  toParam (Member x) | S.isEmpty x = Nil
  toParam (Member x) =
    lsing
      ( ( "MEMBER"
            /\ ((\v -> NeedQuotes /\ v) <<< {-T.pack <<< -}  show <$> List.fromFoldable x)
        )
      )

instance ToParam Role where
  toParam x | x == def = Nil
  toParam Chair = lsing (("ROLE" /\ lsing ((NoQuotes /\ "CHAIR"))))
  toParam ReqParticipant = lsing (("ROLE" /\ lsing ((NoQuotes /\ "REQ-PARTICIPANT"))))
  toParam OptParticipant = lsing (("ROLE" /\ lsing ((NoQuotes /\ "OPT-PARTICIPANT"))))
  toParam NonParticipant = lsing (("ROLE" /\ lsing ((NoQuotes /\ "NON-PARTICIPANT"))))
  toParam (RoleX x) = lsing (("ROLE" /\ lsing ((Optional /\ CI.original x))))

instance ToParam PartStat where
  toParam x | x == def = Nil
  toParam PartStatNeedsAction = lsing (("PARTSTAT" /\ lsing ((NoQuotes /\ "NEEDS-ACTION"))))
  toParam Accepted = lsing (("PARTSTAT" /\ lsing ((NoQuotes /\ "ACCEPTED"))))
  toParam Declined = lsing (("PARTSTAT" /\ lsing ((NoQuotes /\ "DECLINED"))))
  toParam Tentative = lsing (("PARTSTAT" /\ lsing ((NoQuotes /\ "TENTATIVE"))))
  toParam Delegated = lsing (("PARTSTAT" /\ lsing ((NoQuotes /\ "DELEGATED"))))
  toParam PartStatCompleted = lsing (("PARTSTAT" /\ lsing ((NoQuotes /\ "COMPLETED"))))
  toParam InProcess = lsing (("PARTSTAT" /\ lsing ((NoQuotes /\ "IN-PROCESS"))))
  toParam (PartStatX x) = lsing (("PARTSTAT" /\ lsing ((Optional /\ CI.original x))))

instance ToParam RelationshipType where
  toParam x | x == def = Nil
  toParam Parent = lsing (("RELTYPE" /\ lsing ((NoQuotes /\ "PARENT"))))
  toParam Child = lsing (("RELTYPE" /\ lsing ((NoQuotes /\ "CHILD"))))
  toParam Sibling = lsing (("RELTYPE" /\ lsing ((NoQuotes /\ "SIBLING"))))
  toParam (RelationshipTypeX x) = lsing (("RELTYPE" /\ lsing ((Optional /\ CI.original x))))

instance ToParam RSVP where
  toParam (RSVP false) = Nil
  toParam (RSVP true) = lsing (("RSVP" /\ lsing ((NoQuotes /\ "TRUE"))))

instance ToParam DelTo where
  toParam (DelTo x)
    | S.isEmpty x = Nil
    | otherwise =
        lsing
          ( ( "DELEGATED-TO"
                /\ ((\v -> NeedQuotes /\ v) <<< show <$> List.fromFoldable x)
            )
          )

instance ToParam DelFrom where
  toParam (DelFrom x)
    | S.isEmpty x = Nil
    | otherwise =
        lsing
          ( ( "DELEGATED-FROM"
                /\
                  ( (\v -> NeedQuotes /\ v) <<< show
                      <$> List.fromFoldable x
                  )
            )
          )

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
  toParam x | x == def = Nil
  toParam Start = lsing (("RELATED" /\ lsing ((NoQuotes /\ "START"))))
  toParam End = lsing (("RELATED" /\ lsing ((NoQuotes /\ "END"))))

instance ToParam Trigger where
  toParam (TriggerDuration a) = toParam a.triggerOther <>
    toParam a.triggerRelated
  toParam (TriggerDateTime a) = toParam a.triggerOther <>
    lsing (("VALUE" /\ lsing ((NoQuotes /\ "DATE-TIME"))))

-- }}}
-- {{{ Value printers

printUTCOffset :: Int -> ContentPrinter Unit
printUTCOffset n = do
  case signum n of
    -1 -> putc '-'
    _ -> putc '+'
  -- TODO
  -- out $ printf "%02d" t
  -- out $ printf "%02d" m
  -- when (s > 0) <<< out $ printf "%02d" s
  where
  (m' /\ s) = abs n `divMod` 60
  (t /\ m) = m' `divMod` 60

printNWeekday :: Either (Int /\ DT.Weekday) DT.Weekday -> ContentPrinter Unit
printNWeekday (Left (n /\ w)) = printShow n *> printValue w
printNWeekday (Right x) = printValue x

printShow :: forall a. Show a => a -> ContentPrinter Unit
printShow = out <<< {-T.pack <<< -}  show

printShowN :: forall a. Show a => (List a) -> ContentPrinter Unit
printShowN = printN printShow

printN :: forall a. (a -> ContentPrinter Unit) -> List a -> ContentPrinter Unit
printN m (x : xs) = m x *> (sequence_ $ map (\x' -> putc ',' *> m x') xs)
printN _ _ = pure unit

printShowUpper :: forall a. Show a => a -> ContentPrinter Unit
printShowUpper = out <<< {-T.pack <<< map -} toUpper <<< show

printUTCTime :: Types.UTCTime -> ContentPrinter Unit
printUTCTime t = out $ (formatDateTime t <> "Z")

class IsValue a where
  printValue :: a -> ContentPrinter Unit

instance IsValue ICalVersion where
  printValue (MaxICalVersion a) = out $ Ver.showVersion a.versionMax
  printValue (MinMaxICalVersion a) = do
    out $ Ver.showVersion a.versionMin
    putc ';'
    out $ Ver.showVersion a.versionMax

instance IsValue Recur where
  printValue (Recur a) = do
    out "FREQ="
    printShowUpper a.recurFreq
    for_ a.recurUntilCount $ \x ->
      case x of
        Left y -> out ";UNTIL=" *> printValue y
        Right y -> out ";COUNT=" *> printShow y
    when (a.recurInterval /= 1) $
      out ";INTERVAL=" *> printShow a.recurInterval
    unless (Foldable.null a.recurBySecond) $
      out ";BYSECOND=" *> printShowN a.recurBySecond
    unless (Foldable.null a.recurByMinute) $
      out ";BYMINUTE=" *> printShowN a.recurByMinute
    unless (Foldable.null a.recurByHour) $
      out ";BYHOUR=" *> printShowN a.recurByHour
    unless (Foldable.null a.recurByDay) $
      out ";BYDAY=" *> printN printNWeekday a.recurByDay
    unless (Foldable.null a.recurByMonthDay) $
      out ";BYMONTHDAY=" *> printShowN a.recurByMonthDay
    unless (Foldable.null a.recurByYearDay) $
      out ";BYYEARDAY=" *> printShowN a.recurByYearDay
    unless (Foldable.null a.recurByWeekNo) $
      out ";BYWEEKNO=" *> printShowN a.recurByWeekNo
    unless (Foldable.null a.recurByMonth) $
      out ";BYMONTH=" *> printShowN a.recurByMonth
    unless (Foldable.null a.recurBySetPos) $
      out ";BYSETPOS=" *> printShowN a.recurBySetPos
    unless (a.recurWkSt == DT.Monday) $
      out ";WKST=" *> printValue a.recurWkSt

instance IsValue TimeTransparency where
  printValue (Opaque a) = out "OPAQUE"
  printValue (Transparent a) = out "TRANSPARENT"

instance IsValue DTEnd where
  printValue (DTEndDateTime a) = printValue a.dtEndDateTimeValue
  printValue (DTEndDate a) = printValue a.dtEndDateValue

instance IsValue Due where
  printValue (DueDateTime a) = printValue a.dueDateTimeValue
  printValue (DueDate a) = printValue a.dueDateValue

instance IsValue EventStatus where
  printValue (TentativeEvent _) = out "TENTATIVE"
  printValue (ConfirmedEvent _) = out "CONFIRMED"
  printValue (CancelledEvent _) = out "CANCELLED"

instance IsValue TodoStatus where
  printValue (TodoNeedsAction _) = out "NEEDS-ACTION"
  printValue (CompletedTodo _) = out "COMPLETED"
  printValue (InProcessTodo _) = out "IN-PROCESS"
  printValue (CancelledTodo _) = out "CANCELLED"

instance IsValue JournalStatus where
  printValue (DraftJournal _) = out "DRAFT"
  printValue (FinalJournal _) = out "FINAL"
  printValue (CancelledJournal _) = out "CANCELLED"

instance IsValue ClassValue where
  printValue (ClassValueX x) = out $ CI.original x
  printValue x = printShowUpper x

instance IsValue DT.Weekday where
  printValue DT.Sunday = out "SU"
  printValue DT.Monday = out "MO"
  printValue DT.Tuesday = out "TU"
  printValue DT.Wednesday = out "WE"
  printValue DT.Thursday = out "TH"
  printValue DT.Friday = out "FR"
  printValue DT.Saturday = out "SA"

instance IsValue Date where
  printValue (date) = out $ formatDate date

instance IsValue ICalDateTime where
  printValue (FloatingDateTime a) =
    out $ formatDateTime a.dateTimeFloating
  printValue (UTCDateTime a) = printUTCTime a.dateTimeUTC
  printValue (ZonedDateTime a) =
    out $ formatDateTime a.dateTimeFloating

instance IsValue (Either Date ICalDateTime) where
  printValue (Left x) = printValue x
  printValue (Right x) = printValue x

instance IsValue DTStamp where
  printValue (DTStamp a) = printUTCTime a.dtStampValue

instance IsValue DTStart where
  printValue (DTStartDateTime a) = printValue a.dtStartDateTimeValue
  printValue (DTStartDate a) = printValue a.dtStartDateValue

instance IsValue UriM where
  printValue = printShow

instance IsValue Duration where
  printValue (DurationDate a) = do
    when (a.durSign == Negative) $ putc '-'
    putc 'P'
    printShow a.durDay *> putc 'D'
    putc 'T'
    printShow a.durHour *> putc 'H'
    printShow a.durMinute *> putc 'M'
    printShow a.durSecond *> putc 'S'
  printValue (DurationTime a) = do
    when (a.durSign == Negative) $ putc '-'
    out "PT"
    printShow a.durHour *> putc 'H'
    printShow a.durMinute *> putc 'M'
    printShow a.durSecond *> putc 'S'
  printValue (DurationWeek a) = do
    when (a.durSign == Negative) $ putc '-'
    out "P"
    printShow a.durWeek *> putc 'W'

instance IsValue RecurrenceId where
  printValue (RecurrenceIdDate a) = printValue a.recurrenceIdDate
  printValue (RecurrenceIdDateTime a) = printValue a.recurrenceIdDateTime

instance IsValue Period where
  printValue (PeriodDates f t) = printValue f *> putc '/' *> printValue t
  printValue (PeriodDuration f d) = printValue f *> putc '/' *> printValue d

instance IsValue UTCPeriod where
  printValue (UTCPeriodDates f t) = printUTCTime f *> putc '/' *> printUTCTime t
  printValue (UTCPeriodDuration f d) = printUTCTime f *> putc '/' *> printValue d

instance IsValue RDate where
  printValue (RDateDates a) = printN printValue $ List.fromFoldable a.rDateDates
  printValue (RDateDateTimes a) = printN printValue $ List.fromFoldable a.rDateDateTimes
  printValue (RDatePeriods a) = printN printValue $ List.fromFoldable a.rDatePeriods

instance IsValue Attachment where
  printValue (UriAttachment a) = printShow a.attachUri
  printValue (BinaryAttachment a) = bytestring $ B64.encode a.attachContent

ln :: ContentPrinter Unit -> ContentPrinter Unit
ln x = x *> newline

param :: (Text /\ (List (Quoting /\ Text))) -> ContentPrinter Unit
param (n /\ xs) = putc ';' *> out n *> putc '=' *> paramVals xs

paramVals :: (List (Quoting /\ Text)) -> ContentPrinter Unit
paramVals (x : xs) = paramVal x *> (sequence_ $ map (\x' -> putc ',' *> paramVal x') xs)
paramVals _ = pure unit

paramVal :: (Quoting /\ Text) -> ContentPrinter Unit
paramVal (NeedQuotes /\ t) = putc '"' *> out t *> putc '"'
paramVal (NoQuotes /\ t) = out t
paramVal (_ /\ t) = paramVal (NeedQuotes /\ t)

texts :: (List Text) -> ContentPrinter Unit
texts (x : xs) = text x *> (sequence_ $ map (\x' -> putc ',' *> text x') xs)
texts _ = pure unit

text :: Text -> ContentPrinter Unit
text t = case map (\{ head: h, tail: r } -> h /\ r) $ CodeUnits.uncons t of
  Just (';' /\ r) -> out "\\;" *> text r
  Just ('\n' /\ r) -> out "\\n" *> text r
  Just (',' /\ r) -> out "\\," *> text r
  Just ('\\' /\ r) -> out "\\\\" *> text r
  Just (c /\ r) -> putc c *> text r
  Nothing -> pure unit

bytestring :: ByteString -> ContentPrinter Unit
bytestring s = Foldable.foldl (\m c -> m *> putc8 c) (pure unit) (CodeUnits.toCharArray s)

out :: Text -> ContentPrinter Unit
out t = case CodeUnits.uncons t of
  Just ({ head: c, tail: r }) -> putc c *> out r
  Nothing -> pure unit

putc :: Char -> ContentPrinter Unit
putc c = do
  x <- get
  (b /\ clen) <- asks ((_.efChar2Bu &&& _.efChar2Len) <<< \(EncodingFunctions x) -> x)
  let cl = clen c
  when (x + cl > 75) foldLine
  tell $ b c
  _ <- modify ((+) cl)
  pure unit

putc8 :: Char -> ContentPrinter Unit
putc8 c = do
  x <- get
  when (x >= 75) foldLine
  tell $ CodeUnits.singleton c
  _ <- modify ((+) 1)
  pure unit

foldLine :: ContentPrinter Unit
foldLine = tell ( {-Bu.byteString-} "\r\n ") *> put 1

newline :: ContentPrinter Unit
newline = tell ( {-Bu.byteString-} "\r\n") *> put 0

-- | Output a whole line. Must be less than 75 bytes.
line :: ByteString -> ContentPrinter Unit
line b = tell ( {-Bu.lazyByteString-} b) *> newline

formatDate :: DT.Date -> String
formatDate d = DF.format (List.fromFoldable [ DF.YearFull, DF.MonthFull, DF.MonthTwoDigits, DF.DayOfMonthTwoDigits ]) (DT.DateTime d bottom)

formatTime :: DT.Time -> String
formatTime d = DF.format (List.fromFoldable [ DF.Hours24, DF.MinutesTwoDigits, DF.SecondsTwoDigits ]) (DT.DateTime bottom d)

formatDateTime :: DT.DateTime -> String
formatDateTime (DT.DateTime d t) = formatDate d <> "T" <> formatTime t

-- formatTime :: FormatTime t => String -> t -> String
-- formatTime = Time.formatTime defaultTimeLocale

