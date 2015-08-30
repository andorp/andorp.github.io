module Blog.Model where

import Data.List (unfoldr)
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

type ListAlgebra a b = (b, a -> b -> b)

listCatamorphism :: ListAlgebra a b -> [a] -> b
listCatamorphism (nil, cons) = foldr cons nil

-- Entry

type Entry = EntryT ()

data EntryT a = Entry {
    e_hole  :: a
  , e_lines :: Pandoc
  } deriving (Eq, Show)

entry lines = Entry () lines

type EntryAlgebra a b = (a -> Pandoc -> b)

entryCatamorphism :: EntryAlgebra a b -> EntryT a -> b
entryCatamorphism f (Entry hole lines) = f hole lines

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

topicNameCatamorphism :: TopicNameAlgebra a b -> TopicNameT a -> b
topicNameCatamorphism f (TopicName hole name) = f hole name

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

topicCatamorphism :: TopicAlgebra a t e es p -> TopicT a -> p
topicCatamorphism (topicNameAlg, entryAlg, entriesListAlg, combine) (Topic hole topicName entries)
  = combine hole (topicNameCatamorphism topicNameAlg topicName) (listCatamorphism entriesListAlg (fmap (entryCatamorphism entryAlg) entries))

instance Functor TopicT where
  fmap f (Topic hole topicName entries) = Topic (f hole) (fmap f topicName) (fmap (fmap f) entries)

-- Blog

type Blog = BlogT ()

data BlogT a = Blog {
    b_hole    :: a
  , b_summary :: Pandoc
  , b_topics  :: [TopicT a]
  } deriving (Eq, Show)

blog pages = Blog () pages

type BlogAlgebra a t e es p bs b = (TopicAlgebra a t e es p, ListAlgebra p bs, a -> Pandoc -> bs -> b)

blogCatamorphism :: BlogAlgebra a t e es p bs b -> BlogT a -> b
blogCatamorphism (topic, topicList, combine) (Blog hole summary topics) =
  combine hole summary (listCatamorphism topicList (fmap (topicCatamorphism topic) topics))

instance Functor BlogT where
  fmap f (Blog hole summary topics) = Blog (f hole) summary (fmap (fmap f) topics)

