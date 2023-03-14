module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De exposing(..)
import Html.Attributes exposing (style)


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }

descripitonIf : Repo -> Html msg
descripitonIf r =
    if r.description == Nothing then
        em [class "repo-description"] []
    else
        em [class "repo-description"] [text (Maybe.withDefault "text" r.description)]

view : Repo -> Html msg
view repo = div [class "repo", style "text-align" "center", style "background-color" "#ffcfcf"] 
    [
        h3 [class "repo-name", style "background-color" "grey"] [text repo.name],
        descripitonIf repo,
        h4 [class "repo-url"] [ a [href repo.url][text repo.url]],
        h5 [class "repo-pushedAt"] [text repo.pushedAt],
        h5 [class "repo-stars"] [text (String.fromInt repo.stars)]
    ]


repoComparison : Repo -> Repo -> Order
repoComparison a b =
    if a.stars == b.stars then
        EQ
    else
        if a.stars < b.stars then
            LT
        else
            GT

sortByStars : List Repo -> List Repo
sortByStars repos =
    List.sortWith repoComparison repos


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
    De.map5 Repo
        (field "name" string)
        (field "description" (nullable string))
        (field "html_url" string)
        (field "pushed_at" string)
        (field "stargazers_count" int)
