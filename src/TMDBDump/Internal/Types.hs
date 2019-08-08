module TMDBDump.Internal.Types
  ( Movie(..)
  , Genre(..)
  , Genres(..)
  , ISO639_1
  , ISO3166_1
  ) where

import           Control.Monad    (forM)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe       (catMaybes)
import qualified Data.Text        as T

-- | 2 letter code for ISO3166-1 countries.
type ISO3166_1 = (Char, Char)
-- | 2 letter code for ISO639-1 languages.
type ISO639_1 = (Char, Char)

-- | Movie data.
data Movie = Movie
  { movieId                  :: Int
  , movieIsAdult             :: Bool
  , movieStatus              :: T.Text
  , movieIMDBId              :: Maybe T.Text
  , movieTitle               :: T.Text
  , movieOriginalTitle       :: T.Text
  , movieOriginalLanguage    :: T.Text
  , movieReleaseDate         :: Maybe T.Text
  , movieGenres              :: [Genre]
  , movieSpokenLanguages     :: [ISO639_1]
  , movieProductionCountries :: [ISO3166_1]
  , movieRuntime             :: Int
  , movieBudget              :: Int
  , movieRevenue             :: Int
  }
  deriving (Show)

instance FromJSON Movie where
  parseJSON =
    withObject "Movie" $ \v ->
      Movie <$> v .: "id" <*> v .: "adult" <*> v .: "status" <*> v .:? "imdb_id" <*>
      v .: "title" <*>
      v .: "original_title" <*>
      v .: "original_language" <*>
      v .:? "release_date" <*>
      v .:? "genres" .!= [] <*>
      extractAll v "spoken_languages" "iso_639_1" <*>
      extractAll v "production_countries" "iso_3166_1" <*>
      v .: "runtime" .!= 0 <*>
      v .: "budget" .!= 0 <*>
      v .: "revenue" .!= 0
    where
      str2tup :: String -> Maybe (Char, Char)
      str2tup (x:y:_) = Just (x, y)
      str2tup _       = Nothing
      extractAll :: Object -> T.Text -> T.Text -> Parser [(Char, Char)]
      extractAll v f_arr f_obj = do
        arr <- v .:? f_arr .!= []
        catMaybes <$> forM arr (\o -> str2tup <$> o .: f_obj)

-- | Genre type.
data Genre = Genre
  { genreId   :: Int    -- ^ Unique id for a genre.
  , genreName :: T.Text -- ^ Genre name.
  }
  deriving (Show, Eq)

instance FromJSON Genre where
  parseJSON = withObject "Genre" $ \v -> Genre <$>
    v .: "id" <*>
    v .: "name"

-- | Newtype wrapper for List of genres.
newtype Genres = Genres { getGenreList :: [Genre] }
  deriving (Show)

instance FromJSON Genres where
  parseJSON = withObject "Genres" $ \v -> Genres <$> v .: "genres"
