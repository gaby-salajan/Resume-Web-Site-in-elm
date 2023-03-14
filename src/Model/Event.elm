module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"

intervalComparison : Event -> Event -> Order
intervalComparison a b =
    Interval.compare a.interval b.interval

sortByInterval : List Event -> List Event
sortByInterval events =
    List.sortWith intervalComparison events


eventUrlIf : Maybe String -> Html msg
eventUrlIf cond =
    if (Maybe.withDefault "no" cond) == "no" then
        div [] []
    else
        h3 [class "event-url"] [a [href (Maybe.withDefault "url" cond)] [text "url"]]

eventImportantIf : Bool -> Html msg
eventImportantIf cond =
    if cond == True then
        h3 [class "event event-important"] [text "important"]
    else
        h3 [] []

view : Event -> Html Never
view event = div [class "event", style "text-align" "center", style "background-color" "#e8e8e8"] 
  [ h3 [class "event-title"]  [ text event.title ],
    h4 [class "event-interval"] [Interval.view (event.interval)],
    h4 [class "event-description"] [event.description],
    h4 [class "event-category", style "text-align" "right"] [categoryView event.category],
    eventUrlIf event.url,
    eventImportantIf event.important
    --h3 [] [text (String.concat event.tags)]
  ]
