--------------------------
-- CORE LIBRARY IMPORTS --
--------------------------
import Task         exposing (Task, ThreadID, andThen, sequence, succeed, spawn)
import Json.Decode  exposing (Decoder, list, int, string, (:=), map, object2)
import Signal       exposing (Signal, Mailbox, mailbox, send)
import List

---------------------------------
-- THIRD PARTY LIBRARY IMPORTS --
---------------------------------
import Http             exposing (Error, get)
import Html             exposing (Html, div, ul, li, a, text)
import Html.Attributes  exposing (href)

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Convenient for decoding large JSON objects
andMap : Decoder (a -> b) -> Decoder a -> Decoder b
andMap = object2 (<|)

-- Perform a list of tasks in parallel.
-- Analogous to `sequence`
parallel : List (Task x value) -> Task y (List ThreadID)
parallel =
  sequence << List.map spawn

-----------
-- MODEL --
-----------

type alias Story =
  { by    : String
  , id    : Int
  , score : Int
  , time  : Int
  , title : String
  , type' : String
  , url   : String
  }

type alias Model = List Story

initialModel : Model
initialModel = []

----------
-- VIEW --
----------

viewStory : Story -> Html
viewStory story =
  li []
     [ a [ href story.url ]
         [ text story.title ]
     ]

view : Model -> Html
view stories =
  ul []
     ( List.map viewStory stories )


--------------------------
-- LINKS TO HACKER NEWS --
--------------------------

idsUrl : String
idsUrl =
  "https://hacker-news.firebaseio.com/v0/topstories.json"

storyUrl : Int -> String
storyUrl id =
  "https://hacker-news.firebaseio.com/v0/item/"++ toString id ++ ".json"

-----------
-- TASKS --
-----------

getIDs : Task Error (List Int)
getIDs =
  get (list int) idsUrl


getStory : Int -> Task Error ()
getStory id = get storyDecoder (storyUrl id)
  `andThen` \story -> send newStoryMailbox.address (Just story)


mainTask : Task Error (List ThreadID)
mainTask = getIDs
  `andThen` \ids -> parallel (List.map getStory ids)


-------------------
-- JSON DECODING --
-------------------

storyDecoder : Decoder Story
storyDecoder = Story
  `map`    ("by"    := string)
  `andMap` ("id"    := int)
  `andMap` ("score" := int)
  `andMap` ("time"  := int)
  `andMap` ("title" := string)
  `andMap` ("type"  := string)
  `andMap` ("url"   := string)


---------------
-- MAILBOXES --
---------------

newStoryMailbox : Mailbox Action
newStoryMailbox =
  mailbox Nothing


mainTaskMailbox : Mailbox (Task Error (List ThreadID))
mainTaskMailbox =
  mailbox mainTask

-----------
-- PORTS --
-----------

port mainTaskPort : Signal (Task Error (List ThreadID))
port mainTaskPort =
  mainTaskMailbox.signal


-------------
-- ACTIONS --
-------------

type alias Action = Maybe Story


actions : Signal Action
actions =
  newStoryMailbox.signal

------------
-- UPDATE --
------------

update : Action -> Model -> Model
update maybeStory stories = case maybeStory of
  Nothing -> stories
  Just story -> stories ++ [ story ]


----------
-- MAIN --
----------

main : Signal Html
main =
  Signal.map view
    ( Signal.foldp update initialModel actions )
