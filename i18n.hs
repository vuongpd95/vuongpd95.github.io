module I18n where

import Data.Map (Map)
import qualified Data.Map as Map

data TranslatedString = TranslatedString {
    en :: String,
    vi :: String
}

translations :: Map String TranslatedString
translations = Map.fromList
    [("home", TranslatedString { en = "Home", vi = "Trang chủ" }),
     ("about", TranslatedString { en = "About", vi = "Về mình" })]


t :: String -> String -> String
t langStr keyStr = case Map.lookup keyStr translations of
                    Nothing -> "Translations key " ++ keyStr ++ " not found"
                    Just translation -> getLangTranslation langStr translation
                        where getLangTranslation "en" translation = en translation
                              getLangTranslation "vi" translation = vi translation
                              getLangTranslation lang translation
                                = "No translation available for " ++ lang ++ " lang"
