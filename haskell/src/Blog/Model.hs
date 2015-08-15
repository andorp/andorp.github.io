module Blog.Model where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Text.Pandoc.Definition (Pandoc)

-- This module is an experiment to apply algebraic style of programming
-- I have a gut feeling that _hole-s are similar to the applications of HoTT

data FileProperties = FileProperties {
    path             :: FilePath
  , modificationTime :: UTCTime
  } deriving (Eq, Show)

-- List

type ListAlgebra a b = (b, b -> a -> b)

listAlgebra :: ListAlgebra a b -> [a] -> b
listAlgebra (nil, cons) = foldl cons nil

-- Entry

type Entry = EntryT ()

data EntryT a = Entry {
    e_hole  :: a
  , e_lines :: Pandoc
  } deriving (Eq, Show)

entry lines = Entry () lines

type EntryAlgebra a b = (a -> Pandoc -> b)

entryAlgebra :: EntryAlgebra a b -> EntryT a -> b
entryAlgebra f (Entry hole lines) = f hole lines

instance Functor EntryT where
  fmap f (Entry x lines) = Entry (f x) lines

-- TopicName

type TopicName = TopicNameT ()

data TopicNameT a = TopicName {
    tn_hole :: a
  , tn_name :: Pandoc
  } deriving (Eq, Show)

topicName name = TopicName () name

type TopicNameAlgebra a b = (a -> Pandoc -> b)

topicNameAlgebra :: TopicNameAlgebra a b -> TopicNameT a -> b
topicNameAlgebra f (TopicName hole name) = f hole name

instance Functor TopicNameT where
  fmap f (TopicName x name) = TopicName (f x) name

-- Topic

type Topic = TopicT ()
data TopicT a = Topic {
    t_hole      :: a
  , t_topicName :: TopicNameT a
  , t_entries   :: [EntryT a]
  } deriving (Eq, Show)

topic topicName entries = Topic () topicName entries

type TopicAlgebra a t e es p = (TopicNameAlgebra a t, EntryAlgebra a e, ListAlgebra e es, a -> t -> es -> p)

topicAlgebra :: TopicAlgebra a t e es p -> TopicT a -> p
topicAlgebra (topicNameAlg, entryAlg, entriesListAlg, combine) (Topic hole topicName entries)
  = combine hole (topicNameAlgebra topicNameAlg topicName) (listAlgebra entriesListAlg (fmap (entryAlgebra entryAlg) entries))

instance Functor TopicT where
  fmap f (Topic hole topicName entries) = Topic (f hole) (fmap f topicName) (fmap (fmap f) entries)

-- Blog

type Blog = BlogT ()

data BlogT a = Blog {
    b_hole  :: a
  , b_topics :: [TopicT a]
  } deriving (Eq, Show)

blog pages = Blog () pages

type BlogAlgebra a t e es p bs b = (TopicAlgebra a t e es p, ListAlgebra p bs, a -> bs -> b)

blogAlgebra :: BlogAlgebra a t e es p bs b -> BlogT a -> b
blogAlgebra (topic, topicList, combine) (Blog hole topics) =
  combine hole (listAlgebra topicList (fmap (topicAlgebra topic) topics))

instance Functor BlogT where
  fmap f (Blog hole topics) = Blog (f hole) (fmap (fmap f) topics)

