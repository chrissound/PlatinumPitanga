{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Log where

import Options.Applicative
import Types

parser :: Parser PitangaLogCommand
parser =
  PitangaLogCommand
  <$>
  optional
  (
    First <$>
    (
        option auto
        (  long "first"
        <> help "show first n"
        )
    )
    <|>
    Last <$>
    (
        option auto
        (  long "last"
        <> help "show last n"
        )
    )
  )
