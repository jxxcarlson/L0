module L0.MExpression exposing (MExpression(..), fromElement, getText)

import Camperdown.Camperdown as Camp
import L0.CamperdownParserTypes exposing (..)


type MExpression
    = Literal String
    | MElement String (List String) MExpression
    | MList (List MExpression)
    | MProblem String


fromElement : Element -> MExpression
fromElement element =
    case element of
        Camp.Paragraph markup ->
            fromMarkup markup

        Camp.Preformatted { contents } ->
            MElement "preformatted" [] (Literal contents)

        Camp.Item _ ->
            Literal "item: not implemented"

        Camp.Command _ ->
            Literal "command: not implemented"

        Camp.Problem { problem } ->
            Literal ("Problem: " ++ problem)


fromMarkup : Markup -> MExpression
fromMarkup textList =
    List.map fromText textList |> MList


fromText : Text -> MExpression
fromText text =
    case text of
        Camp.Raw str ->
            Literal str

        Camp.Verbatim _ ( _, str ) ->
            Literal str

        Camp.Annotation markup ->
            let
                ( _, ( ( mark, _, _ ), _ ) ) =
                    markup.annotation
            in
            case markup.markup of
                ( _, [] ) ->
                    Literal "(invalid annotation)"

                ( _, head :: rest ) ->
                    case head of
                        Camp.Raw str ->
                            if mark == "[" then
                                let
                                    firstSpace =
                                        List.head (String.indices " " str) |> Maybe.withDefault 1

                                    fname =
                                        String.left firstSpace str

                                    arg =
                                        String.dropLeft firstSpace str
                                in
                                case List.head rest of
                                    Nothing ->
                                        MElement fname [] (MList (Literal arg :: List.map fromText rest))

                                    Just possibleArg ->
                                        let
                                            ( mark2, args2 ) =
                                                case possibleArg of
                                                    Camp.Annotation yada ->
                                                        let
                                                            ( _, ( ( mark_, _, _ ), _ ) ) =
                                                                yada.annotation

                                                            ( _, rawtextlist ) =
                                                                yada.markup

                                                            args_ =
                                                                List.head rawtextlist
                                                                    |> Maybe.andThen getText
                                                                    |> Maybe.map (String.split "," >> List.map String.trim)
                                                                    |> Maybe.withDefault []
                                                        in
                                                        ( mark_, args_ )

                                                    _ ->
                                                        ( "(error)", [] )
                                        in
                                        if mark2 == "|" then
                                            let
                                                --args =
                                                --    String.split ", " "a:1, b:2" |> List.map String.trim
                                                rest2 =
                                                    List.drop 1 rest
                                            in
                                            MElement fname args2 (MList (List.map fromText rest2))

                                        else
                                            MElement fname [] (MList (Literal arg :: List.map fromText rest))

                            else
                                Literal "(I was expecting '[' or '|')"

                        _ ->
                            Literal "(malformed annotation)"

        Camp.InlineProblem _ ->
            Literal "(inline problem)"


getText t =
    case t of
        Camp.Raw text ->
            Just text

        _ ->
            Nothing
