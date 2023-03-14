module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }

combineDetails : DetailWithName -> String
combineDetails detail = 
    detail.name ++ " : " ++ detail.detail

view : PersonalDetails -> Html msg
view details = div [style "text-align" "center"] 
  [ h1 [id "name", style "font-family" "arial" ] [text details.name],
  em [id "intro"] [ text details.intro],
  div[style "text-align" "center"] [
        p [class "contact-detail"] (List.map (\l -> li [] [ text (combineDetails l) ]) details.contacts),
        p [class "social-link"] (List.map (\l -> li [] [ a [ href l.detail][ text l.name ]]) details.socials)
  ]
  
  ]
