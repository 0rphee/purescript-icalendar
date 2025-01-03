{- # LANGUAGE CPP                # -}
{- # LANGUAGE DeriveDataTypeable # -}
{- # LANGUAGE OverloadedStrings  # -}
-- | ICalendar types, based on RFC5545.

module Text.ICalendar.Types (module Text.ICalendar.Types) where

-- module Text.ICalendar.Types

import Prelude

import Codec.MIME.Type (MIMEType)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive as CI
import Data.DateTime as DT
import Data.Default (class Default, def)
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (T2)
import URI (Fragment, HierPath, Host, Path, Port, Query, UserInfo)
import URI.HostPortPair (HostPortPair)
import URI.URI as URI

version :: Version
version =
  Version { versionBranch: List.fromFoldable [ 0, 1, 0 ], versionTags: List.Nil }

type Float = Number

type Text =
  String

-- | Couldn't find an alternative, this will have to do, all code ported from <https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/src/GHC.Internal.Data.Version.html#showVersion>

data Version = Version
  { versionBranch :: List Int
  , versionTags :: List String
  }

showVersion :: Version -> String
showVersion (Version { versionBranch, versionTags }) =
  ((List.intercalate "." (map show versionBranch)))
    <> (Foldable.fold (map (\x -> "-" <> x) versionTags))

type Integer =
  Int

newtype Day = ModifiedJulianDay
  { toModifiedJulianDay :: Integer
  }

derive instance Eq Day
derive instance Ord Day

type UTCTime = DT.DateTime

type LocalTime = DT.DateTime

-- | Language.

data Language = Language CI

derive instance Eq Language
derive instance Ord Language

-- TODO: RFC5646 types and parser.
-- deriving

type UriM = URI.URI UserInfo (HostPortPair Host Port) Path HierPath Query Fragment
type CalAddress = UriM

-- | One other parameter, either x-param or iana-param.

data OtherParam = OtherParam CI (List Text)

derive instance Eq OtherParam
derive instance Ord OtherParam

-- | Other parameters, either x-param or other iana-param.

data OtherParams = OtherParams (Set OtherParam)

derive instance Eq OtherParams
derive instance Ord OtherParams

defSet :: forall a. Set a
defSet = Set.empty

defMap :: forall a b. Map a b
defMap =
  Map.empty

instance Default OtherParams where
  def = OtherParams defSet

-- | VCalendar component. 3.4.

data VCalendar =
  VCalendar
    { vcProdId :: ProdId
    , vcVersion :: ICalVersion
    , vcScale :: Scale
    , vcMethod :: Maybe Method
    , vcOther :: Set OtherProperty
    , vcTimeZones :: Map Text VTimeZone

    -- ^ Map TZID-value VTimeZone
    , vcEvents :: Map (T2 Text (Maybe (Either Date ICalDateTime))) VEvent

    -- ^ Map (UID-value, Maybe RecurrenceID-value) VEvent
    , vcTodos :: Map (T2 Text (Maybe (Either Date ICalDateTime))) VTodo

    -- ^ Map (UID-value, Maybe RecurrenceID-value) VTodo
    , vcJournals :: Map (T2 Text (Maybe (Either Date ICalDateTime))) VJournal

    -- ^ Map (UID-value, Maybe RecurrenceID-value) VJournal
    , vcFreeBusys :: Map Text VFreeBusy

    -- ^ Map UID-value VFreeBusy
    , vcOtherComps :: Set VOther
    }

-- deriving
-- ( Show, Eq, Ord, Typeable )
-- instance Default VCalendar where

defVCalendar :: VCalendar
defVCalendar =
  VCalendar
    { vcProdId:
        ProdId
          { prodIdValue:
              "-//haskell.org/NONSGML iCalendar-"
                <> showVersion version
                <> "//EN"
          , prodIdOther: def
          }
    , vcVersion: MaxICalVersion { versionMax: Version { versionBranch: List.fromFoldable [ 2, 0 ], versionTags: List.Nil }, versionOther: def }
    , vcScale: def
    , vcMethod: Nothing
    , vcOther: defSet
    , vcTimeZones: defMap

    -- ^ Map TZID-value VTimeZone
    , vcEvents: defMap

    -- ^ Map (UID-value, Maybe RecurrenceID-value) VEvent
    , vcTodos: defMap

    -- ^ Map (UID-value, Maybe RecurrenceID-value) VTodo
    , vcJournals: defMap

    -- ^ Map (UID-value, Maybe RecurrenceID-value) VJournal
    , vcFreeBusys: defMap

    -- ^ Map UID-value VFreeBusy
    , vcOtherComps: defSet
    }

-- deriving
-- ( Show, Eq, Ord, Typeable )
-- | 'vcMethod' is ignored at the moment.
--
-- Picks the left in most cases.
--
-- On UID/RecurrenceId/TZID clash, picks the 'VEvent's, 'VTodo's and
-- 'VJournal's with the highest ('Sequence', 'DTStamp'), the 'VTimeZone's
-- with the highest 'LastModified', and 'VFreeBusy' with the highest 'DTStamp'.
--
-- If the Sequence, DTStamp or LastModified is the same, picks the left.
-- instance Sem.Semigroup VCalendar where

instance Semigroup VCalendar where
  append (VCalendar a) (VCalendar b) =
    VCalendar
      { vcProdId: _.vcProdId a
      , vcVersion: _.vcVersion a
      , vcScale: _.vcScale a
      , vcMethod: _.vcMethod a
      , vcOther: Set.union (_.vcOther a) (_.vcOther b)
      , vcTimeZones:
          merge tz
            (_.vcTimeZones a)
            (_.vcTimeZones b)
      , vcEvents: merge ev (_.vcEvents a) (_.vcEvents b)
      , vcTodos: merge td (_.vcTodos a) (_.vcTodos b)
      , vcJournals:
          merge jo
            (_.vcJournals a)
            (_.vcJournals b)
      , vcFreeBusys:
          merge fb
            (_.vcFreeBusys a)
            (_.vcFreeBusys b)
      , vcOtherComps: Set.union (_.vcOtherComps a) (_.vcOtherComps b)
      }
    where
    merge :: forall c k. (Ord k) => (c -> c -> c) -> Map k c -> Map k c -> Map k c
    merge f d1 d2 =
      Map.unionWith (\x y -> f x y) d1 d2

    tz c d =
      if _.vtzLastMod c >= _.vtzLastMod d then c else d

    ev c d =
      if Tuple (_.veSeq c) (_.veDTStamp c) >= Tuple (_.veSeq d) (_.veDTStamp d) then c else d

    td c d =
      if Tuple (_.vtSeq c) (_.vtDTStamp c) >= Tuple (_.vtSeq d) (_.vtDTStamp d) then c else d

    jo c d =
      if Tuple (_.vjSeq c) (_.vjDTStamp c) >= Tuple (_.vjSeq d) (_.vjDTStamp d) then c else d

    fb c d =
      if _.vfbDTStamp c >= _.vfbDTStamp d then c
      else
        d

instance Monoid VCalendar where
  mempty = defVCalendar

-- memptyVCalendar =
--   defVCalendar

-- mappendVCalendar =
--   combineVCalendars

-- | Product Identifier. 3.7.3.

data ProdId = ProdId
  { prodIdValue :: Text
  , prodIdOther :: OtherParams
  }

prodIdOther :: ProdId -> OtherParams
prodIdOther (ProdId a) = a.prodIdOther

prodIdValue :: ProdId -> Text
prodIdValue (ProdId a) = a.prodIdValue

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Version. 3.7.4.

data ICalVersion
  = MaxICalVersion
      { versionMax :: Version
      , versionOther :: OtherParams
      }
  | MinMaxICalVersion
      { versionMax :: Version
      , versionMin :: Version
      , versionOther :: OtherParams
      }

-- deriving
-- ( Show, Eq, Ord, dataable )

versionOther :: ICalVersion -> OtherParams
versionOther a = case a of
  MaxICalVersion b -> b.versionOther
  MinMaxICalVersion b -> b.versionOther

versionMax :: ICalVersion -> Version
versionMax a = case a of
  MaxICalVersion b -> b.versionMax
  MinMaxICalVersion b -> b.versionMax

-- | Calendar Scale. 3.7.1.

data Scale = Scale
  { scaleValue :: CI
  , scaleOther :: OtherParams
  }

scaleValue :: Scale -> CI
scaleValue (Scale a) = a.scaleValue

scaleOther :: Scale -> OtherParams
scaleOther (Scale a) = a.scaleOther

-- deriving
-- ( Show, Eq, Ord, dataable )
-- instance Default Scale where
-- 

instance Default Scale where
  def = Scale { scaleValue: CI.mk "GREGORIAN", scaleOther: def }

-- | Method. 3.7.2.

data Method = Method
  { methodValue :: CI -- TODO: iTIP, RFC5546
  , methodOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Event Component. 3.6.1.

type VEvent =
  -- VEvent
  { veDTStamp :: DTStamp
  , veUID :: UID
  , veClass :: Class -- ^ 'def' = 'Public'
  , veDTStart :: Maybe DTStart
  , veCreated :: Maybe Created
  , veDescription :: Maybe Description
  , veGeo :: Maybe Geo
  , veLastMod :: Maybe LastModified
  , veLocation :: Maybe Location
  , veOrganizer :: Maybe Organizer
  , vePriority :: Priority -- ^ 'def' = 0
  , veSeq :: Sequence -- ^ 'def' = 0
  , veStatus :: Maybe EventStatus
  , veSummary :: Maybe Summary
  , veTransp :: TimeTransparency -- ^ 'def' = 'Opaque'
  , veUrl :: Maybe URL
  , veRecurId :: Maybe RecurrenceId
  , veRRule :: Set RRule
  , veDTEndDuration :: Maybe (Either DTEnd DurationProp)
  , veAttach :: Set Attachment
  , veAttendee :: Set Attendee
  , veCategories :: Set Categories
  , veComment :: Set Comment
  , veContact :: Set Contact
  , veExDate :: Set ExDate
  , veRStatus :: Set RequestStatus
  , veRelated :: Set RelatedTo
  , veResources :: Set Resources
  , veRDate :: Set RDate
  , veAlarms :: Set VAlarm
  , veOther :: Set OtherProperty
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | To-Do Component. 3.6.2

type VTodo =
  -- VTodo
  { vtDTStamp :: DTStamp
  , vtUID :: UID
  , vtClass :: Class -- ^ 'def' = 'Public'
  , vtCompleted :: Maybe Completed
  , vtCreated :: Maybe Created
  , vtDescription :: Maybe Description
  , vtDTStart :: Maybe DTStart
  , vtGeo :: Maybe Geo
  , vtLastMod :: Maybe LastModified
  , vtLocation :: Maybe Location
  , vtOrganizer :: Maybe Organizer
  , vtPercent :: Maybe PercentComplete
  , vtPriority :: Priority -- ^ 'def' = 0
  , vtRecurId :: Maybe RecurrenceId
  , vtSeq :: Sequence -- ^ 'def' = 0
  , vtStatus :: Maybe TodoStatus
  , vtSummary :: Maybe Summary
  , vtUrl :: Maybe URL
  , vtRRule :: Set RRule
  , vtDueDuration :: Maybe (Either Due DurationProp)
  , vtAttach :: Set Attachment
  , vtAttendee :: Set Attendee
  , vtCategories :: Set Categories
  , vtComment :: Set Comment
  , vtContact :: Set Contact
  , vtExDate :: Set ExDate
  , vtRStatus :: Set RequestStatus
  , vtRelated :: Set RelatedTo
  , vtResources :: Set Resources
  , vtRDate :: Set RDate
  , vtAlarms :: Set VAlarm
  , vtOther :: Set OtherProperty
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Journal Component. 3.6.3

type VJournal =
  -- VJournal
  { vjDTStamp :: DTStamp
  , vjUID :: UID
  , vjClass :: Class -- ^ 'def' = 'Public'
  , vjCreated :: Maybe Created
  , vjDTStart :: Maybe DTStart
  , vjLastMod :: Maybe LastModified
  , vjOrganizer :: Maybe Organizer
  , vjRecurId :: Maybe RecurrenceId
  , vjSeq :: Sequence -- ^ 'def' = 0
  , vjStatus :: Maybe JournalStatus
  , vjSummary :: Maybe Summary
  , vjUrl :: Maybe URL
  , vjRRule :: Set RRule
  , vjAttach :: Set Attachment
  , vjAttendee :: Set Attendee
  , vjCategories :: Set Categories
  , vjComment :: Set Comment
  , vjContact :: Set Contact
  , vjDescription :: Set Description
  , vjExDate :: Set ExDate
  , vjRelated :: Set RelatedTo
  , vjRDate :: Set RDate
  , vjRStatus :: Set RequestStatus
  , vjOther :: Set OtherProperty
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Free/Busy Component. 3.6.4

type VFreeBusy =
  --VFreeBusy
  { vfbDTStamp :: DTStamp
  , vfbUID :: UID
  , vfbContact :: Maybe Contact
  , vfbDTStart :: Maybe DTStart
  , vfbDTEnd :: Maybe DTEnd
  , vfbOrganizer :: Maybe Organizer
  , vfbUrl :: Maybe URL
  , vfbAttendee :: Set Attendee
  , vfbComment :: Set Comment
  , vfbFreeBusy :: Set FreeBusy
  , vfbRStatus :: Set RequestStatus
  , vfbOther :: Set OtherProperty
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Time Zone Component. 3.6.5.

type VTimeZone =
  -- VTimeZone
  { vtzId :: TZID
  , vtzLastMod :: Maybe LastModified
  , vtzUrl :: Maybe TZUrl
  , vtzStandardC :: Set TZProp
  , vtzDaylightC :: Set TZProp
  , vtzOther :: Set OtherProperty
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Time zone property, also 3.6.5.

data TZProp =
  TZProp
    { tzpDTStart :: DTStart
    , tzpTZOffsetTo :: UTCOffset
    , tzpTZOffsetFrom :: UTCOffset
    , tzpRRule :: Set RRule -- SHOULD NOT have multiple RRules.
    , tzpComment :: Set Comment
    , tzpRDate :: Set RDate
    , tzpTZName :: Set TZName
    , tzpOther :: Set OtherProperty
    }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | VAlarm component. 3.6.6.

data VAlarm
  = VAlarmAudio
      { vaTrigger :: Trigger
      , vaRepeat :: Repeat -- ^ 'def' = 0
      , vaDuration :: Maybe DurationProp
      , vaAudioAttach :: Maybe Attachment
      , vaOther :: Set OtherProperty
      , vaActionOther :: OtherParams
      }
  | VAlarmDisplay
      { vaDescription :: Description
      , vaTrigger :: Trigger
      , vaRepeat :: Repeat
      , vaDuration :: Maybe DurationProp
      , vaOther :: Set OtherProperty
      , vaActionOther :: OtherParams
      }
  | VAlarmEmail
      { vaDescription :: Description
      , vaTrigger :: Trigger
      , vaSummary :: Summary
      , vaAttendee :: Set Attendee
      , vaRepeat :: Repeat
      , vaDuration :: Maybe DurationProp
      , vaMailAttach :: Set Attachment
      , vaOther :: Set OtherProperty
      , vaActionOther :: OtherParams
      }
  | VAlarmX
      { vaAction :: CI
      , vaTrigger :: Trigger
      , vaActionOther :: OtherParams
      , vaOther :: Set OtherProperty
      }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Any other component not recognized.

data VOther = VOther
  { voName :: CI
  , voProps :: Set OtherProperty
  }

derive instance Eq VOther
derive instance Ord VOther

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Attachment. 3.8.1.1.

data Attachment
  = UriAttachment
      { attachFmtType :: Maybe MIMEType
      , attachUri :: UriM
      , attachOther :: OtherParams
      }
  | BinaryAttachment
      { attachFmtType :: Maybe MIMEType
      , attachContent :: Uint8Array
      , attachOther :: OtherParams
      }

-- deriving ( Show, Eq, Ord, dataable )
-- | Categories. 3.8.1.2.

data Categories = Categories
  { categoriesValues :: Set Text
  , categoriesLanguage :: Maybe Language
  , categoriesOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Classification. 3.8.1.3.

data Class = Class
  { classValue :: ClassValue
  , classOther :: OtherParams
  }

derive instance Eq Class
derive instance Ord Class

instance Default Class where
  def :: Class
  def = Class { classValue: def, classOther: def }

-- | Classification value. 3.8.1.3.
-- Unrecognized ClassValueX MUST be treated as Private.

data ClassValue
  = Public
  | Private
  | Confidential
  | ClassValueX CI

derive instance Eq ClassValue
derive instance Ord ClassValue

instance Show ClassValue where
  show x = case x of
    Public -> "Public"
    Private -> "Private"
    Confidential -> "Confidential"
    ClassValueX ci -> "ClassValueX (" <> show ci <> ")"

instance Default ClassValue where
  def = Public

-- | Date-Time Completed. 3.8.2.1.

data Completed = Completed
  { completedValue :: ICalDateTime
  , completedOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Comment. 3.8.1.4.

data Comment = Comment
  { commentValue :: Text
  , commentAltRep :: Maybe UriM
  , commentLanguage :: Maybe Language
  , commentOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Description. 3.8.1.5.

data Description = Description
  { descriptionValue :: Text
  , descriptionAltRep :: Maybe UriM
  , descriptionLanguage :: Maybe Language
  , descriptionOther :: OtherParams
  }

derive instance Eq Description
derive instance Ord Description
-- | Geographic Position. 3.8.1.6.

data Geo = Geo
  { geoLat :: Float
  , geoLong :: Float
  , geoOther :: OtherParams
  }

derive instance Eq Geo
derive instance Ord Geo
-- | Location. 3.8.1.7.

data Location = Location
  { locationValue :: Text
  , locationAltRep :: Maybe UriM
  , locationLanguage :: Maybe Language
  , locationOther :: OtherParams
  }

derive instance Eq Location
derive instance Ord Location
-- | Percent complete. 3.8.1.8.

data PercentComplete = PercentComplete
  { percentCompleteValue :: Int
  , percentCompleteOther :: OtherParams
  }

derive instance Eq PercentComplete
derive instance Ord PercentComplete
-- | Priority. 3.8.1.9.

data Priority = Priority
  { priorityValue :: Int
  , priorityOther :: OtherParams
  }

derive instance Eq Priority
derive instance Ord Priority

instance Default Priority where
  def :: Priority
  def = Priority { priorityValue: 0, priorityOther: def }

-- | Resources. 3.8.1.10.

data Resources = Resources
  { resourcesValue :: Set Text
  , resourcesAltRep :: Maybe UriM
  , resourcesLanguage :: Maybe Language
  , resourcesOther :: OtherParams
  }

derive instance Eq Resources
derive instance Ord Resources
-- | Status, but only for Events. 3.8.1.11.

data EventStatus
  = TentativeEvent { eventStatusOther :: OtherParams }
  | ConfirmedEvent { eventStatusOther :: OtherParams }
  | CancelledEvent { eventStatusOther :: OtherParams } -- deriving ( Show, Eq, Ord, dataable )

-- | Status, but only for TODOs. 3.8.1.11.

data TodoStatus
  = TodoNeedsAction { todoStatusOther :: OtherParams }
  | CompletedTodo { todoStatusOther :: OtherParams }
  | InProcessTodo { todoStatusOther :: OtherParams }
  | CancelledTodo { todoStatusOther :: OtherParams } -- deriving ( Show, Eq, Ord, dataable )

todoStatusOther :: TodoStatus -> OtherParams
todoStatusOther x = case x of
  TodoNeedsAction a -> a.todoStatusOther
  CompletedTodo a -> a.todoStatusOther
  InProcessTodo a -> a.todoStatusOther
  CancelledTodo a -> a.todoStatusOther

-- | Status, but only for Journals. 3.8.1.11.

data JournalStatus
  = DraftJournal { journalStatusOther :: OtherParams }
  | FinalJournal { journalStatusOther :: OtherParams }
  | CancelledJournal { journalStatusOther :: OtherParams } -- deriving ( Show, Eq, Ord, dataable )

-- | Summary. 3.8.1.12.

data Summary = Summary
  { summaryValue :: Text
  , summaryAltRep :: Maybe UriM
  , summaryLanguage :: Maybe Language
  , summaryOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Date. 3.3.4

type Date = DT.Date

-- data Date = Date
--   { dateValue :: Day
--   }

-- deriving ( Show, Eq, Ord, dataable )

-- | Date-Time value. 3.3.5.
data ICalDateTime
  = FloatingDateTime
      { dateTimeFloating :: LocalTime
      }
  | UTCDateTime
      { dateTimeUTC :: UTCTime
      }
  | ZonedDateTime
      { dateTimeFloating :: LocalTime
      , dateTimeZone :: Text
      }

derive instance Eq ICalDateTime
derive instance Ord ICalDateTime

-- deriving ( Show, Eq, Ord, dataable )
-- | Date-Time End. 3.8.2.2.

data DTEnd
  = DTEndDateTime
      { dtEndDateTimeValue :: ICalDateTime
      , dtEndOther :: OtherParams
      }
  | DTEndDate
      { dtEndDateValue :: Date
      , dtEndOther :: OtherParams
      }

derive instance Eq DTEnd
derive instance Ord DTEnd

-- deriving ( Show, Eq, Ord, dataable )
-- | Date-Time Due. 3.8.2.3.

data Due
  = DueDateTime
      { dueDateTimeValue :: ICalDateTime
      , dueOther :: OtherParams
      }
  | DueDate
      { dueDateValue :: Date
      , dueOther :: OtherParams
      }

derive instance Eq Due
derive instance Ord Due

-- deriving ( Show, Eq, Ord, dataable )

-- | Date-Time Start. 3.8.2.4.

data DTStart
  = DTStartDateTime
      { dtStartDateTimeValue :: ICalDateTime
      , dtStartOther :: OtherParams
      }
  | DTStartDate
      { dtStartDateValue :: Date
      , dtStartOther :: OtherParams
      }

-- deriving ( Show, Eq, Ord, dataable )
-- | Duration value. 3.3.6.

data Duration
  -- TODO(?): Convert to DiffTime?
  = DurationDate
      { durSign :: Sign -- ^ 'def' = 'Positive'
      , durDay :: Int
      , durHour :: Int
      , durMinute :: Int
      , durSecond :: Int
      }
  | DurationTime
      { durSign :: Sign
      , durHour :: Int
      , durMinute :: Int
      , durSecond :: Int
      }
  | DurationWeek
      { durSign :: Sign
      , durWeek :: Int
      }

derive instance Eq Duration
derive instance Ord Duration

-- | Sign.
data Sign
  = Positive
  | Negative

derive instance Eq Sign
derive instance Ord Sign

instance Default Sign where
  def = Positive

-- | Duration property. 3.8.2.5.

data DurationProp = DurationProp
  { durationValue :: Duration
  , durationOther :: OtherParams
  }

derive instance Eq DurationProp
derive instance Ord DurationProp

data FreeBusy = FreeBusy
  { freeBusyType :: FBType
  , freeBusyPeriods :: Set UTCPeriod
  , freeBusyOther :: OtherParams
  }

derive instance Eq FreeBusy
derive instance Ord FreeBusy

-- deriving ( Show, Eq, Ord, dataable )
-- | Period of time. 3.3.9.
data Period
  = PeriodDates ICalDateTime ICalDateTime
  | PeriodDuration ICalDateTime Duration

derive instance Eq Period
derive instance Ord Period

-- | Period of time which must be UTC, as in FreeBusy. 3.3.9.
data UTCPeriod
  = UTCPeriodDates UTCTime UTCTime
  | UTCPeriodDuration UTCTime Duration

derive instance Eq UTCPeriod
derive instance Ord UTCPeriod

-- | Free/Busy Time data. 3.2.9.
--
-- Unrecognized FBTypeX MUST be treated as Busy.
data FBType
  = Free
  | Busy
  | BusyUnavailable
  | BusyTentative
  | FBTypeX CI

derive instance Eq FBType
derive instance Ord FBType

instance Default FBType where
  def = Busy

-- | Time Transparency. 3.8.2.7.

data TimeTransparency
  = Opaque { timeTransparencyOther :: OtherParams }
  | Transparent { timeTransparencyOther :: OtherParams } -- deriving ( Show, Eq, Ord, dataable )

derive instance Eq TimeTransparency
derive instance Ord TimeTransparency

instance Default TimeTransparency where
  def :: TimeTransparency
  def = Opaque { timeTransparencyOther: def }

-- | Time Zone Identifier. 3.8.3.1.

data TZID = TZID
  { tzidValue :: Text
  , tzidGlobal :: Boolean
  , tzidOther :: OtherParams
  }

tzidValue :: TZID -> Text
tzidValue (TZID a) = a.tzidValue

tzidGlobal :: TZID -> Boolean
tzidGlobal (TZID a) = a.tzidGlobal

tzidOther :: TZID -> OtherParams
tzidOther (TZID a) = a.tzidOther

-- deriving ( Show, Eq, Ord, dataable )
-- | Time Zone Name. 3.8.3.2.

data TZName = TZName
  { tzNameValue :: Text
  , tzNameLanguage :: Maybe Language
  , tzNameOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | UTC Offset. 3.3.14, 3.8.3.4, and 3.8.3.3. (unified-ish)

data UTCOffset = UTCOffset
  { utcOffsetValue :: Int -- ^ Number of seconds away from UTC
  , utcOffsetOther :: OtherParams
  }

utcOffsetValue :: UTCOffset -> Int
utcOffsetValue (UTCOffset a) = a.utcOffsetValue

utcOffsetOther :: UTCOffset -> OtherParams
utcOffsetOther (UTCOffset a) = a.utcOffsetOther

-- deriving ( Show, Eq, Ord, dataable )
-- | Time Zone URL. 3.8.3.5.

data TZUrl = TZUrl
  { tzUrlValue :: UriM
  , tzUrlOther :: OtherParams
  }

tzUrlValue :: TZUrl -> UriM
tzUrlValue (TZUrl a) = a.tzUrlValue

tzUrlOther :: TZUrl -> OtherParams
tzUrlOther (TZUrl a) = a.tzUrlOther

-- -- deriving ( Show, Eq, Ord, dataable )
-- | Attendee. 3.8.4.1.

data Attendee = Attendee
  { attendeeValue :: CalAddress
  , attendeeCUType :: CUType -- ^ 'def' = 'Individual'
  , attendeeMember :: Set CalAddress
  , attendeeRole :: Role -- ^ 'def' = 'ReqParticipant'
  , attendeePartStat :: PartStat -- ^ 'def' = 'PartStatNeedsAction'
  , attendeeRSVP :: Boolean
  , attendeeDelTo :: Set CalAddress
  , attendeeDelFrom :: Set CalAddress
  , attendeeSentBy :: Maybe CalAddress
  , attendeeCN :: Maybe Text
  , attendeeDir :: Maybe UriM
  , attendeeLanguage :: Maybe Language
  , attendeeOther :: OtherParams
  }

-- | Calendar User data. 3.2.3.
--
-- Unrecognized CUTypeX MUST be treated as Unknown.
data CUType
  = Individual
  | Group
  | Resource
  | Room
  | Unknown
  | CUTypeX CI

derive instance Eq CUType
derive instance Ord CUType

instance Default CUType where
  def = Individual

-- | Role. 3.2.16.
data Role
  = Chair
  | ReqParticipant
  | OptParticipant
  | NonParticipant
  | RoleX CI

derive instance Eq Role
derive instance Ord Role

instance Default Role where
  def = ReqParticipant

-- | Participation Status. 3.2.12.

data PartStat
  -- Splitting requires splitting attendee too...
  = PartStatNeedsAction
  | Accepted
  | Declined
  | Tentative
  | Delegated
  | PartStatCompleted
  | InProcess
  | PartStatX CI

derive instance Eq PartStat
derive instance Ord PartStat

instance Default PartStat where
  def = PartStatNeedsAction

-- | Contact. 3.8.4.2.

data Contact = Contact
  { contactValue :: Text
  , contactAltRep :: Maybe UriM
  , contactLanguage :: Maybe Language
  , contactOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Organizer. 3.8.4.3.
--
-- TODO: CAL-ADDRESS-related properties.

data Organizer = Organizer
  { organizerValue :: CalAddress
  , organizerCN :: Maybe Text
  , organizerDir :: Maybe UriM
  , organizerSentBy :: Maybe CalAddress
  , organizerLanguage :: Maybe Language
  , organizerOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Recurrence ID. 3.8.4.4.

data RecurrenceId
  = RecurrenceIdDate
      { recurrenceIdDate :: Date
      , recurrenceIdRange :: Maybe Range
      , recurrenceIdOther :: OtherParams
      }
  | RecurrenceIdDateTime
      { recurrenceIdDateTime :: ICalDateTime
      , recurrenceIdRange :: Maybe Range
      , recurrenceIdOther :: OtherParams
      }

derive instance Eq RecurrenceId
derive instance Ord RecurrenceId

-- | Recurrence Identifier Range. 3.2.13
data Range
  = ThisAndFuture
  | ThisAndPrior

derive instance Eq Range
derive instance Ord Range
-- | Related To. 3.8.4.5.
data RelatedTo = RelatedTo
  { relatedToValue :: Text
  , relatedToType :: RelationshipType
  , relatedToOther :: OtherParams
  }

derive instance Eq RelatedTo
derive instance Ord RelatedTo

-- | Relationship data. 3.2.15.
--
-- Unrecognized RelationshipTypeX values MUST be treated as Parent.
data RelationshipType
  = Parent
  | Child
  | Sibling
  | RelationshipTypeX CI -- deriving ( Show, Eq, Ord, dataable )

derive instance Eq RelationshipType
derive instance Ord RelationshipType

instance Default RelationshipType where
  def = Parent

-- | Uniform Resource Locator. 3.8.4.6.
data URL = URL
  { urlValue :: UriM
  , urlOther :: OtherParams
  }

derive instance Eq URL
derive instance Ord URL

-- | Unique Identifier. 3.8.4.7.
data UID = UID
  { uidValue :: Text
  , uidOther :: OtherParams
  }

derive instance Eq UID
derive instance Ord UID

-- | Exception Date-Times. 3.8.5.1.
data ExDate
  = ExDates
      { exDates :: Set Date
      , exDateOther :: OtherParams
      }
  | ExDateTimes
      { exDateTimes :: Set ICalDateTime
      , exDateOther :: OtherParams
      }

derive instance Eq ExDate
derive instance Ord ExDate

-- | Recurrence Date-Times. 3.8.5.2.
data RDate
  = RDateDates
      { rDateDates :: Set Date
      , rDateOther :: OtherParams
      }
  | RDateDateTimes
      { rDateDateTimes :: Set ICalDateTime
      , rDateOther :: OtherParams
      }
  | RDatePeriods
      { rDatePeriods :: Set Period
      , rDateOther :: OtherParams
      }

derive instance Eq RDate
derive instance Ord RDate

-- | Frequency in recurrences. 3.3.10.

data Frequency
  = Secondly
  | Minutely
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly

derive instance Eq Frequency
derive instance Ord Frequency

instance Show Frequency where
  show a = case a of
    Secondly -> "Secondly"
    Minutely -> "Minutely"
    Hourly -> "Hourly"
    Daily -> "Daily"
    Weekly -> "Weekly"
    Monthly -> "Monthly"
    Yearly -> "Yearly"

-- | Weekday, in recurrences. 3.3.10.
type Weekday = DT.Weekday

-- | Recur value. 3.3.10.

data Recur = Recur
  { recurFreq :: Frequency
  , recurUntilCount :: Maybe (Either (Either Date ICalDateTime) Int)
  , recurInterval :: Int
  , recurBySecond :: List Int
  , recurByMinute :: List Int
  , recurByHour :: List Int
  , recurByDay :: List (Either (T2 Int Weekday) Weekday)
  , recurByMonthDay :: List Int
  , recurByYearDay :: List Int
  , recurByWeekNo :: List Int
  , recurByMonth :: List Int
  , recurBySetPos :: List Int
  , recurWkSt :: Weekday
  }

derive instance Eq Recur
derive instance Ord Recur
-- | Recurrence Rule. 3.8.5.3.

data RRule = RRule
  { rRuleValue :: Recur
  , rRuleOther :: OtherParams
  }

derive instance Eq RRule
derive instance Ord RRule
-- | Repeat count. 3.8.6.2.

data Repeat = Repeat
  { repeatValue :: Integer
  , repeatOther :: OtherParams
  }

derive instance Eq Repeat
derive instance Ord Repeat

instance Default Repeat where
  def :: Repeat
  def = Repeat { repeatValue: 0, repeatOther: def }

-- | Alarm Trigger Relationship. 3.2.14.

data AlarmTriggerRelationship
  = Start
  | End

derive instance Eq AlarmTriggerRelationship
derive instance Ord AlarmTriggerRelationship

instance Default AlarmTriggerRelationship where
  def = Start

-- | Trigger. 3.8.6.3.

data Trigger
  = TriggerDuration
      { triggerDuration :: Duration
      , triggerRelated :: AlarmTriggerRelationship -- ^ 'def' = 'Start'
      , triggerOther :: OtherParams
      }
  | TriggerDateTime
      { triggerDateTime :: UTCTime
      , triggerOther :: OtherParams
      }

derive instance Eq Trigger
derive instance Ord Trigger

-- | Date-Time Created. 3.8.7.1.
data Created = Created
  { createdValue :: UTCTime
  , createdOther :: OtherParams
  }

derive instance Eq Created
derive instance Ord Created

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Date-Time Stamp. 3.8.7.2.

data DTStamp = DTStamp
  { dtStampValue :: UTCTime
  , dtStampOther :: OtherParams
  }

derive instance Eq DTStamp
derive instance Ord DTStamp

-- | Last Modified. 3.8.7.3.

data LastModified = LastModified
  { lastModifiedValue :: UTCTime
  , lastModifiedOther :: OtherParams
  }

derive instance Eq LastModified
derive instance Ord LastModified

-- | Sequence number. 3.8.7.4.

data Sequence = Sequence
  { sequenceValue :: Integer
  , sequenceOther :: OtherParams
  }

derive instance Eq Sequence
derive instance Ord Sequence

instance Default Sequence where
  def :: Sequence
  def = Sequence { sequenceValue: 0, sequenceOther: def }

-- | Request Status. 3.8.8.3.
data RequestStatus = RequestStatus
  { requestStatusCode :: List Int
  , requestStatusDesc :: Text
  , requestStatusLanguage :: Maybe Language
  , requestStatusExt :: Maybe Text
  , requestStatusOther :: OtherParams
  }

derive instance Eq RequestStatus
derive instance Ord RequestStatus

-- | Any other property.

data OtherProperty = OtherProperty
  { otherName :: CI
  , otherValue :: String
  , otherParams :: OtherParams
  }

derive instance Eq OtherProperty
derive instance Ord OtherProperty
