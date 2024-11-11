{- # LANGUAGE CPP                # -}
{- # LANGUAGE DeriveDataTypeable # -}
{- # LANGUAGE OverloadedStrings  # -}
-- | ICalendar types, based on RFC5545.

module Text.ICalendar.Types (module Text.ICalendar.Types) where

-- module Text.ICalendar.Types

import Codec.MIME.Type (MIMEType)
-- import           Data.ByteString.Lazy.Char8 (ByteString)
-- import Data.Default exposing (..)
-- import Data.Text.Lazy exposing (Text, pack)
-- import Data.Semigroup as Sem
-- import Data.Typeable exposing (Typeable)
-- import Data.Version exposing (Version(..), showVersion)
-- import Data.Time
-- import Network.URI exposing (URI)

import Prelude

import Data.CaseInsensitive (CI)
import Data.CaseInsensitive as CI
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

-- import Time (Posix)

-- import Paths_iCalendar (version)

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

-- type URI = Url

type Integer =
  Int

type Day =
  Weekday

data Posix = Posix

derive instance posixeq :: Eq Posix
derive instance posixord :: Ord Posix

type UTCTime =
  Posix

type LocalTime =
  Posix

-- | Language.

data Language = Language CI

-- TODO: RFC5646 types and parser.
-- deriving
-- ( Eq, Show, Ord, Typeable )

type URI = URI.URI UserInfo (HostPortPair Host Port) Path HierPath Query Fragment
type CalAddress = URI

-- | One other parameter, either x-param or iana-param.

data OtherParam = OtherParam CI (List Text) --deriving ( Show, Eq, Ord, Typeable )

derive instance Eq OtherParam
derive instance Ord OtherParam

-- | Other parameters, either x-param or other iana-param.

data OtherParams = OtherParams (Set OtherParam) --deriving ( Show, Eq, Ord, Typeable )

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
    , vcEvents :: Map (T2 Text (Maybe (Either Date DateTime))) VEvent

    -- ^ Map (UID-value, Maybe RecurrenceID-value) VEvent
    , vcTodos :: Map (T2 Text (Maybe (Either Date DateTime))) VTodo

    -- ^ Map (UID-value, Maybe RecurrenceID-value) VTodo
    , vcJournals :: Map (T2 Text (Maybe (Either Date DateTime))) VJournal

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
-- | Calendar Scale. 3.7.1.

data Scale = Scale
  { scaleValue :: CI
  , scaleOther :: OtherParams
  }

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
      { attachFmtdata :: Maybe MIMEType
      , attachUri :: URI
      , attachOther :: OtherParams
      }
  | BinaryAttachment
      { attachFmtdata :: Maybe MIMEType
      , attachContent :: String
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

-- deriving ( Show, Eq, Ord, dataable )
-- instance Default Class where

defClass :: Class
defClass =
  Class { classValue: def, classOther: def }

-- | Classification value. 3.8.1.3.
-- Unrecognized ClassValueX MUST be treated as Private.

data ClassValue
  = Public
  | Private
  | Confidential
  | ClassValueX CI -- deriving ( Show, Eq, Ord, dataable )

instance Default ClassValue where
  def = Public

-- | Date-Time Completed. 3.8.2.1.

data Completed = Completed
  { completedValue :: DateTime
  , completedOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Comment. 3.8.1.4.

data Comment = Comment
  { commentValue :: Text
  , commentAltRep :: Maybe URI
  , commentLanguage :: Maybe Language
  , commentOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Description. 3.8.1.5.

data Description = Description
  { descriptionValue :: Text
  , descriptionAltRep :: Maybe URI
  , descriptionLanguage :: Maybe Language
  , descriptionOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Geographic Position. 3.8.1.6.

data Geo = Geo
  { geoLat :: Float
  , geoLong :: Float
  , geoOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Location. 3.8.1.7.

data Location = Location
  { locationValue :: Text
  , locationAltRep :: Maybe URI
  , locationLanguage :: Maybe Language
  , locationOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Percent complete. 3.8.1.8.

data PercentComplete = PercentComplete
  { percentCompleteValue :: Int
  , percentCompleteOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Priority. 3.8.1.9.

data Priority = Priority
  { priorityValue :: Int
  , priorityOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- instance Default Priority where

defPriority :: Priority
defPriority =
  Priority { priorityValue: 0, priorityOther: def }

-- | Resources. 3.8.1.10.

data Resources = Resources
  { resourcesValue :: Set Text
  , resourcesAltRep :: Maybe URI
  , resourcesLanguage :: Maybe Language
  , resourcesOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
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

-- | Status, but only for Journals. 3.8.1.11.

data JournalStatus
  = DraftJournal { journalStatusOther :: OtherParams }
  | FinalJournal { journalStatusOther :: OtherParams }
  | CancelledJournal { journalStatusOther :: OtherParams } -- deriving ( Show, Eq, Ord, dataable )

-- | Summary. 3.8.1.12.

data Summary = Summary
  { summaryValue :: Text
  , summaryAltRep :: Maybe URI
  , summaryLanguage :: Maybe Language
  , summaryOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Date. 3.3.4

data Date = Date
  { dateValue :: Day
  }

derive instance Eq Date
derive instance Ord Date

-- deriving ( Show, Eq, Ord, dataable )
-- | Date-Time value. 3.3.5.

data DateTime
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

derive instance Eq DateTime
derive instance Ord DateTime

-- deriving ( Show, Eq, Ord, dataable )
-- | Date-Time End. 3.8.2.2.

data DTEnd
  = DTEndDateTime
      { dtEndDateTimeValue :: DateTime
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
      { dueDateTimeValue :: DateTime
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
      { dtStartDateTimeValue :: DateTime
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

-- deriving ( Show, Eq, Ord, dataable )
-- | Sign.

data Sign
  = Positive
  | Negative -- deriving ( Show, Eq, Ord, dataable )

instance Default Sign where
  def = Positive

-- | Duration property. 3.8.2.5.

data DurationProp = DurationProp
  { durationValue :: Duration
  , durationOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )

data FreeBusy = FreeBusy
  { freeBusydata :: FBType
  , freeBusyPeriods :: Set UTCPeriod
  , freeBusyOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Period of time. 3.3.9.

data Period
  = PeriodDates DateTime DateTime
  | PeriodDuration DateTime Duration -- deriving ( Show, Eq, Ord, dataable )

-- | Period of time which must be UTC, as in FreeBusy. 3.3.9.

data UTCPeriod
  = UTCPeriodDates UTCTime UTCTime
  | UTCPeriodDuration UTCTime Duration -- deriving ( Show, Eq, Ord, dataable )

-- | Free/Busy Time data. 3.2.9.
--
-- Unrecognized FBTypeX MUST be treated as Busy.

data FBType
  = Free
  | Busy
  | BusyUnavailable
  | BusyTentative
  | FBTypeX CI -- deriving ( Show, Eq, Ord, dataable )

instance Default FBType where
  def = Busy

-- | Time Transparency. 3.8.2.7.

data TimeTransparency
  = Opaque { timeTransparencyOther :: OtherParams }
  | Transparent { timeTransparencyOther :: OtherParams } -- deriving ( Show, Eq, Ord, dataable )

-- instance Default TimeTransparency where

defTimeTransparency :: TimeTransparency
defTimeTransparency =
  Opaque { timeTransparencyOther: def }

-- | Time Zone Identifier. 3.8.3.1.

data TZID = TZID
  { tzidValue :: Text
  , tzidGlobal :: Boolean
  , tzidOther :: OtherParams
  }

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

-- deriving ( Show, Eq, Ord, dataable )
-- | Time Zone URL. 3.8.3.5.

data TZUrl = TZUrl
  { tzUrlValue :: URI
  , tzUrlOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
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
  , attendeeDir :: Maybe URI
  , attendeeLanguage :: Maybe Language
  , attendeeOther :: OtherParams
  }

-- deriving ( Show, Eq, Ord, dataable )
-- | Calendar User data. 3.2.3.
--
-- Unrecognized CUTypeX MUST be treated as Unknown.

data CUType
  = Individual
  | Group
  | Resource
  | Room
  | Unknown
  | CUTypeX CI -- deriving ( Show, Eq, Ord, dataable )

instance Default CUType where
  def = Individual

-- | Role. 3.2.16.

data Role
  = Chair
  | ReqParticipant
  | OptParticipant
  | NonParticipant
  | RoleX CI -- deriving ( Show, Eq, Ord, dataable )

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
  | PartStatX CI -- deriving ( Show, Eq, Ord, dataable )

instance Default PartStat where
  def = PartStatNeedsAction

-- | Contact. 3.8.4.2.

data Contact = Contact
  { contactValue :: Text
  , contactAltRep :: Maybe URI
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
  , organizerDir :: Maybe URI
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
      { recurrenceIdDateTime :: DateTime
      , recurrenceIdRange :: Maybe Range
      , recurrenceIdOther :: OtherParams
      }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Recurrence Identifier Range. 3.2.13

data Range
  = ThisAndFuture
  | ThisAndPrior -- deriving ( Show, Eq, Ord, dataable )

-- | Related To. 3.8.4.5.

data RelatedTo = RelatedTo
  { relatedToValue :: Text
  , relatedTodata :: RelationshipType
  , relatedToOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Relationship data. 3.2.15.
--
-- Unrecognized RelationshipTypeX values MUST be treated as Parent.

data RelationshipType
  = Parent
  | Child
  | Sibling
  | RelationshipTypeX CI -- deriving ( Show, Eq, Ord, dataable )

instance Default RelationshipType where
  def = Parent

-- | Uniform Resource Locator. 3.8.4.6.

data URL = URL
  { urlValue :: URI
  , urlOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Unique Identifier. 3.8.4.7.

data UID = UID
  { uidValue :: Text
  , uidOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Exception Date-Times. 3.8.5.1.

data ExDate
  = ExDates
      { exDates :: Set Date
      , exDateOther :: OtherParams
      }
  | ExDateTimes
      { exDateTimes :: Set DateTime
      , exDateOther :: OtherParams
      }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Recurrence Date-Times. 3.8.5.2.

data RDate
  = RDateDates
      { rDateDates :: Set Date
      , rDateOther :: OtherParams
      }
  | RDateDateTimes
      { rDateDateTimes :: Set DateTime
      , rDateOther :: OtherParams
      }
  | RDatePeriods
      { rDatePeriods :: Set Period
      , rDateOther :: OtherParams
      }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- -- | Frequency in recurrences. 3.3.10.

data Frequency
  = Secondly
  | Minutely
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly -- deriving ( Show, Eq, Ord, dataable )

derive instance Eq Frequency
derive instance Ord Frequency
-- | Weekday, in recurrences. 3.3.10.

data Weekday
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday -- deriving ( Show, Eq, Ord, Bounded, Enum, dataable )

derive instance Eq Weekday
derive instance Ord Weekday

-- | Recur value. 3.3.10.

data Recur = Recur
  { recurFreq :: Frequency
  , recurUntilCount :: Maybe (Either (Either Date DateTime) Int)
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

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Recurrence Rule. 3.8.5.3.

data RRule = RRule
  { rRuleValue :: Recur
  , rRuleOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Repeat count. 3.8.6.2.

data Repeat = Repeat
  { repeatValue :: Integer
  , repeatOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- instance Default Repeat where

defRepeat :: Repeat
defRepeat =
  Repeat { repeatValue: 0, repeatOther: def }

-- | Alarm Trigger Relationship. 3.2.14.

data AlarmTriggerRelationship
  = Start
  | End --deriving ( Show, Eq, Ord, dataable )

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

-- deriving
-- ( Show, Eq, Ord, dataable )
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
-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Last Modified. 3.8.7.3.

data LastModified = LastModified
  { lastModifiedValue :: UTCTime
  , lastModifiedOther :: OtherParams
  }

derive instance Eq LastModified
derive instance Ord LastModified

-- ( Show, Eq, Ord, Typeable )
-- | Sequence number. 3.8.7.4.

data Sequence = Sequence
  { sequenceValue :: Integer
  , sequenceOther :: OtherParams
  }

derive instance Eq Sequence
derive instance Ord Sequence

-- deriving
-- ( Show, Eq, Ord, dataable )
-- instance Default Sequence where

defSequence :: Sequence
defSequence =
  Sequence { sequenceValue: 0, sequenceOther: def }

-- | Request Status. 3.8.8.3.

data RequestStatus = RequestStatus
  { requestStatusCode :: List Int
  , requestStatusDesc :: Text
  , requestStatusLanguage :: Maybe Language
  , requestStatusExt :: Maybe Text
  , requestStatusOther :: OtherParams
  }

-- deriving
-- ( Show, Eq, Ord, dataable )
-- | Any other property.

data OtherProperty = OtherProperty
  { otherName :: CI
  , otherValue :: String
  , otherParams :: OtherParams
  }

derive instance Eq OtherProperty
derive instance Ord OtherProperty

-- deriving
-- ( Show, Eq, Ord, Typeable )
