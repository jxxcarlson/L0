module L0.CamperdownParserTypes exposing (..)

import Camperdown.Camperdown as Camp
import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Parse.Syntax


type alias Document =
    Camp.Document OriginalDivert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias Section =
    Camp.Section OriginalDivert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias Element =
    Camp.Element RewrittenDivert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias Command =
    Camp.Command (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias OriginalDivert =
    Camp.Divert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias RewrittenDivert =
    Camp.Divert (Camp.Mark String) Char ( String, Maybe String, Maybe String ) String


type alias Text =
    Camp.Text Char ( String, Maybe String, Maybe String ) String


type alias Value =
    Camp.Value Char ( String, Maybe String, Maybe String ) String


type alias Markup =
    Camp.Markup Char ( String, Maybe String, Maybe String ) String
