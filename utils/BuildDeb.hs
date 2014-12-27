{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad              (foldM, forM_)
import           Data.List                  (intersperse, isInfixOf)
import           Data.Maybe                 (mapMaybe)
import           Data.Monoid                (mappend, mconcat)
import           System.Environment         (getArgs)
import           System.IO                  (hPutStrLn, stderr)

import           Control.Lens
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.List.Split            (splitOn)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Network.Wreq
import qualified Network.Wreq.Session       as Sess
import           Text.HTML.TagSoup

-- Testing version of 'main' whch skips straight to
-- processing a few source packages (to avoid hammering
-- on debian's web-server while I'm working on
-- this).
main2 :: IO ()
main2 =
  Sess.withSession $ \sess -> do
  let links =
          [ "https://packages.debian.org/source/wheezy/haskell/haskell-xml-types"
          , "https://packages.debian.org/source/wheezy/haskell/haskell-cereal"
          , "https://packages.debian.org/source/wheezy/haskell/haskell-blaze-builder"
          ]
  pkgMap <- foldM (onePackage sess) Map.empty links
  putStrLn $ makeConfig "wheezy" pkgMap
  return ()

main :: IO ()
main =
  -- open Wreq session for HTTP connection pooling
  Sess.withSession $ \sess -> do

  -- TODO argument parsing
  [distName] <- getArgs

  -- get the listing of Haskell packages
  resp <- Sess.get sess $ packagesURL distName
  let body = resp ^. responseBody
      tags = parseTags body
      links = packagePages (packagesURL distName) tags

  -- Per Haskell source package find the name and version
  pkgMap <- foldM (onePackage sess) Map.empty links
  putStrLn $ makeConfig distName pkgMap
  return ()

makeConfig
    :: String -- ^ Distribution name (e.g. 'testing')
    -> Map String String -- ^ Hackage package names and versions
    -> String
makeConfig distName pkgMap
    | Map.null pkgMap = ""
    | otherwise =
        concat $
        [ "-- Cabal config for applying constraints.\n"
        , "-- These constraints should lok similar to Debian "
        , distName
        , "\n"
        , "constraints:\n"
        ] ++ intersperse ",\n" pkgStrings
 where
   pkgStrings :: [String]
   pkgStrings =
       flip map (Map.toList pkgMap) $ \(name, version) ->
       "      " ++ name ++ " == " ++ version

onePackage
    :: Sess.Session -- ^ Wreq session
    -> Map.Map String String -- ^ Map of packages to version
    -> String -- ^ Source package info URL
    -> IO (Map.Map String String)
onePackage sess pkgMap packageURL = do
  resp <- Sess.get sess packageURL
  let body = resp ^. responseBody
      tags = parseTags body
      mVersion = findPackageVersion tags
      mHackageName = findHackageName tags
  case (mVersion, mHackageName) of
    (Just version, Just hackageName) -> do
        hPutStrLn stderr $ hackageName `mappend` " " `mappend` version
        return $ Map.insert hackageName version pkgMap
    _ ->
        return pkgMap

-- | Finds the version of a debian source package,
--   given the body of the package page.
findPackageVersion
    :: [Tag ByteString] -- ^ Parsed body of the page
    -> Maybe String
findPackageVersion tags =
    case splitOn "(" headerString of
      (_:str:_) ->
          case splitOn "-" str of
            (vers:_) -> Just vers
            _ -> Nothing
      _ -> Nothing

 where
   headerString =
    B8.unpack
    . innerText
    . takeWhile (not . isTagCloseName "h1")
    . dropWhile (not . isTagOpenName "h1")
    $ tags

-- | Finds the Hackage package name for a debian source
-- package.
findHackageName tags =
    case dropWhile (not . isHackageLink) tags of
      [] -> Nothing
      (tag:_) ->
          Just
          . last
          . splitOn "/"
          . B8.unpack
          $ fromAttrib "href" tag
 where
   isHackageLink tag =
       isTagOpenName "a" tag
       && "/hackage.haskell.org/package/" `isInfixOf` B8.unpack (fromAttrib "href" tag)
{-

- Grab the list of packages in the 'Haskell' group
- Follow the URL to the source-packages page
- Parse out the version to use, trim deb-specific versions
- Grab the homepage link
- If the homepage is not Hackage reject the package
- otherwise save off the name of the package from the Hackage link

-}

-- packagesURL :: ByteString -> ByteString
packagesURL distName =
    mconcat
    [ "https://packages.debian.org/source/"
    , distName
    , "/haskell/"
    ]

isPackageLink tag =
    isTagOpenName "a" tag
     && fromAttrib "id" tag == fromAttrib "href" tag

packagePages
    :: String -- ^ Base URL for haskell source packages
    -> [Tag ByteString] -- ^ Body of base URL page
    -> [String]
packagePages baseURL tags =
    flip mapMaybe tags $ \tag ->
    if isPackageLink tag
    then Just $ baseURL `mappend` B8.unpack (fromAttrib "href" tag)
    else Nothing
