{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternGuards      #-}
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Exception

import Data.Char
import Data.Data (Data,Typeable)

import System.Environment
import System.Exit
import System.Process
import System.Console.GetOpt

import Text.HTML.TagSoup
import Prelude hiding (catch)

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Preference for format of article. If particular format is not
--   available anything could be downloaded instead
data Format = Any               -- ^ Anything will do (usually defaults to PDF)
            | PDF               -- ^ PDF
            | PS                -- ^ PostScript
            | Source            -- ^ Source of paper
              deriving (Show,Data,Typeable)

-- | URL
type URL = String


----------------------------------------------------------------
-- Network
----------------------------------------------------------------

-- | Download to file
urlToFile :: URL -> FilePath -> IO ExitCode
urlToFile url fname = 
  rawSystem "wget" [ "-U","Mozilla/4.0", url, "-O", fname ] 

-- | Read URL's content into 'String'
urlToString :: URL -> IO String
urlToString url =        
  readProcess "wget" ["-U","Mozilla/4.0", url, "-O", "-"] ""


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Storage for papers
data Storage = Storage {
    -- | Retrieve file name from web page text
    getName     :: Format -> String -> Maybe String
    -- | URL of web page with abstract.
  , abstractURL :: String -> Maybe URL
    -- | URL of paper
  , paperURL    :: Format -> String -> Maybe URL
  }

----------------------------------------
-- | arXiv 

arXiv :: Storage
arXiv = Storage {
    getName     = \f -> fmap ((++ arxivExtension f) . formatTitle) . arxivSelectTitle . parseTags
  , abstractURL = arxivAbstractURL
  , paperURL    = arxivPaperURL
  }

arxivExtension :: Format -> String
arxivExtension Any    = arxivExtension PDF
arxivExtension PDF    = ".pdf"
arxivExtension PS     = ".ps"
arxivExtension Source = ".tar.gz"

arxivAbstractURL :: String -> Maybe String
arxivAbstractURL s = Just $ "http://arxiv.org/abs/" ++ s

arxivPaperURL :: Format -> String -> Maybe URL
arxivPaperURL Any    s = arxivPaperURL PDF s
arxivPaperURL PDF    s = Just $ "http://arxiv.org/pdf/"    ++ s
arxivPaperURL PS     s = Just $ "http://arxiv.org/ps/"     ++ s
arxivPaperURL Source s = Just $ "http://arxiv.org/format/" ++ s

arxivSelectTitle :: [Tag String] -> Maybe String
arxivSelectTitle (TagOpen "title" _ : TagText title : TagClose "title" : _) = Just title
arxivSelectTitle (_:xs) = arxivSelectTitle xs
arxivSelectTitle _      = Nothing

formatTitle :: String -> String
formatTitle = ("arXiv"++) . reformat
  where
    reformat = \case
      ""     -> ""
      '/':cs -> reformat cs
      '$':cs -> reformat cs
      '{':cs -> reformat cs
      '}':cs -> reformat cs
      c:cs
        | isSpace c -> '_':reformat cs
        | otherwise -> c  :reformat cs


----------------------------------------------------------------

-- | Exception which could happen when dealing with URLs
data UrlException = UrlException String
                    deriving (Show,Typeable)
instance Exception UrlException

-- | Convert from 'Maybe' to value and throw exception on Nothing
fromM :: String -> Maybe a -> IO a
fromM _ (Just x) = return x
fromM s Nothing  = throwIO (UrlException s)

-- | Dowload 
download :: Storage -> Format -> String -> IO ()
download d fmt s = do
  title <- fromM "Cannot extract title" . getName d fmt 
       =<< urlToString 
       =<< fromM "Cannot get URL of abstract" (abstractURL d s)
  url   <- fromM "Cannot obtain URL of paper" (paperURL d fmt s)
  urlToFile url title
  return ()

dispach :: Format -> String -> IO ()
dispach _ str
  | ("arxiv:",s) <- at 6 = download arXiv PDF s
  | otherwise            = download arXiv PDF str
  where
    at n = first (map toLower) $ splitAt n str
      

----------------------------------------------------------------
      
main :: IO ()
main = do 
  -- Parse command line
  (p,ss,errs) <- getOpt Permute [ Option [] ["pdf"] (NoArg PDF)    "Download papers in PDF"
                                , Option [] ["ps"]  (NoArg PS)     "Download papers in PS"
                                , Option [] ["src"] (NoArg Source) "Download source (if availalble)"
                                ] <$> getArgs
  let fmt = case p of [] -> Any
                      _  -> last p
  -- Go-go-go
  forM_ ss $ dispach fmt
