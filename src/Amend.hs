{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
module Amend where

import Data.Time
import Options.Applicative
import Common
import Control.Lens.Tuple
import Control.Lens.Setter
import Text.Pretty.Simple (pPrint)


xyz :: String -> Maybe DiffTime
xyz x =   parseTimeM True defaultTimeLocale "%H:%M:%S" x 
      <|> parseTimeM True defaultTimeLocale "%H:%M" x
      <|> parseTimeM True defaultTimeLocale "%M" x

newtype Start = Start DiffTime deriving (Show)
newtype End = End DiffTime deriving (Show)
newtype Duration = Duration DiffTime deriving (Show)

data PitangaAmendCommand =
    PitangaAmendCommand (Maybe Start) (Maybe End) (Maybe Duration)
  | PitangaAmendRestartCommand

parser :: Parser PitangaAmendCommand
parser =
  (
        flag' PitangaAmendRestartCommand
        (  long "restart"
        )
  )
  <|>
  (
  PitangaAmendCommand
  <$>
  optional
  (
    Start <$>
    (
        option (maybeReader xyz)
        (  long "start"
        )
    )
  )
  <*>
  optional
  (
    End <$>
    (
        option (maybeReader xyz)
        (  long "end"
        )
    )
  )
  <*>
  optional
  (
    Duration <$>
    (
        option (maybeReader xyz)
        (  long "duration"
        )
    )
  )
  )

amend :: PitangaAmendCommand -> IO ()
amend (PitangaAmendRestartCommand) = do
  ct <- getCurrentTime
  (eitherDecodeLog) >>= \case
    Right [] -> error "empty..."
    Right x -> do
      case last x of
        nr@(t, d, s, Nothing) -> do
          encodeLogFile $ init x ++ [over _3 (const ct) nr]
          printAmendedTask nr
        _ -> error "Task already ended..."
    Left ee -> error ee
amend (PitangaAmendCommand (Just _) (Just _) (Just _)) = error "Can't set all three...?"
amend (PitangaAmendCommand Nothing Nothing Nothing) = error "Nothing to set"
amend (PitangaAmendCommand s e d) = do
  ct <- getCurrentTime
  let ctt v = const $ ct { utctDayTime = v}
  let s' = case s of
        Just (Start v) -> ctt v
        _ -> case (e, d) of
          (Just (End e'), Just (Duration d')) -> ctt (e' - d')
          _ -> id
  let e' = case e of
        Just (End v) -> Just <$> ctt v
        _ -> case (s, d) of
          (Just (Start sv), Just (Duration dv)) -> Just <$> ctt (sv + dv)
          _ -> id
  (eitherDecodeLog) >>= \case
    Right [] -> error "empty..."
    Right x -> do
      let nr =
              over _3 (s')
            $ over _4 (e')
            $ last x
      encodeLogFile $ init x ++ [nr]
      printAmendedTask nr
    Left ee -> error ee
-- amend (PitangaAmendCommand _ _ _) = error "???"
-- amend (PitangaAmendCommand (Just (Start s)) (Just (End e)) Nothing) = do
--   ct <- getCurrentTime
--   (eitherDecodeLog) >>= \case
--     Right [] -> error "empty..."
--     Right x -> do
--       let nr =
--               over _3 (const $ ct { utctDayTime = s})
--             $ over _4 (const $ Just $ ct { utctDayTime = e})
--             $ last x
--       encodeLogFile $ init x ++ [nr]
--       printAmendedTask nr
--     Left ee -> error ee
-- amend (PitangaAmendCommand (Just s) _ d) = do
--   print "test"
-- amend (PitangaAmendCommand _ (Just e) d) = do
--   print "test"
