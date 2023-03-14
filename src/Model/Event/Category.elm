module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories = [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type SelectedEventCategories = SelectedEventCategories {selectedEvents : List EventCategory}


{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected =
    -- TODOCompleteThisType
    SelectedEventCategories {selectedEvents = eventCategories}

{-| Returns an instance of `SelectedEventCategories` with no categories selected

-- isEventCategorySelected Academic noneSelected --> False

-}
noneSelected : SelectedEventCategories
noneSelected =
    -- TODOCompleteThisType
    SelectedEventCategories {selectedEvents = []}

{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =
    case category of
        Academic -> checkCategory Academic current
        Work -> checkCategory Work current
        Project -> checkCategory Project current
        Award -> checkCategory Award current

checkCategory : EventCategory -> SelectedEventCategories -> Bool
checkCategory categ (SelectedEventCategories se) =
    if List.member categ se.selectedEvents == True then
        True
    else
        False

{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
    -- current
    case category of
        Academic -> setEvent Academic value current
        Work -> setEvent Work value current
        Project -> setEvent Project value current
        Award -> setEvent Award value current

setEvent : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
setEvent cat val (SelectedEventCategories sec) =
    if List.member cat sec.selectedEvents == True then
        if val == False then
            SelectedEventCategories {sec | selectedEvents = List.filter(\x -> (x /= cat)) sec.selectedEvents}
        else
            (SelectedEventCategories sec)
    else    
        if val == True then
            SelectedEventCategories {sec | selectedEvents = (sec.selectedEvents ++ [cat])}
        else
            (SelectedEventCategories sec)

checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]

view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model = div [style "text-align" "center"] 
    [   
        checkbox "Academic" (isEventCategorySelected Academic model) Academic,
        checkbox "Work" (isEventCategorySelected Work model) Work,
        checkbox "Project" (isEventCategorySelected Project model) Project,
        checkbox "Award" (isEventCategorySelected Award model) Award
    ]
