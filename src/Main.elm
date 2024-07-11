module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Process
import Task

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type alias Model =
  { step : Step
  , searchTerm : String
  , selectedWebsite : Maybe String
  , suggestions : List String
  , sites : List Site
  , tosAgreed : Bool
  , generatedCard : Maybe CreditCard
  , error : Maybe String
  , disagreeCount : Int
  , mousePosition : (Float, Float)
  , caughtState : CaughtState
  , viewport : Maybe Viewport
  , firstName : String
  , lastName : String
  , postalCode : String
  }

type alias Viewport =
  { width : Float
  , height : Float
  }

type Step
  = SelectWebsite
  | InputPersonalInfo
  | TermsOfService
  | Generating
  | DisplayCard

type alias Site =
  { name : String
  , supported : Bool
  , slug : String
  }

type alias CreditCard =
  { website : String
  , number : String
  , expiry : String
  , cvv : String
  , company : String
  }

type CaughtState
  = NotCaught
  | JustCaught
  | Disappearing
  | Disappeared

init : () -> (Model, Cmd Msg)
init _ =
  ( { step = SelectWebsite
    , searchTerm = ""
    , selectedWebsite = Nothing
    , suggestions = []
    , sites = []
    , tosAgreed = False
    , generatedCard = Nothing
    , error = Nothing
    , disagreeCount = 0
    , mousePosition = (0, 0)
    , caughtState = NotCaught
    , viewport = Nothing
    , firstName = ""
    , lastName = ""
    , postalCode = ""
    }
  , Cmd.batch
    [ Task.perform GotViewport Browser.Dom.getViewport
    , Http.get
        { url = "/simple-icons.json"
        , expect = Http.expectJson GotSites sitesDecoder
        }
    ]
  )

sitesDecoder : Decode.Decoder (List Site)
sitesDecoder =
  Decode.field "icons"
    (Decode.list
      (Decode.map3 Site
        (Decode.field "title" Decode.string)
        (Decode.succeed True)  -- Assuming all sites are supported for now
        (Decode.field "title" (Decode.map titleToSlug Decode.string))
      )
    )

titleToSlug : String -> String
titleToSlug title =
  let
    replacements =
      [ ("+", "plus")
      , (".", "dot")
      , ("&", "and")
      , ("đ", "d")
      , ("ħ", "h")
      , ("ı", "i")
      , ("ĸ", "k")
      , ("ŀ", "l")
      , ("ł", "l")
      , ("ß", "ss")
      , ("ŧ", "t")
      ]
    
    applyReplacements : String -> String
    applyReplacements str =
      List.foldl (\(from, to) -> String.replace from to) str replacements
    
    removeNonAlphanumeric : String -> String
    removeNonAlphanumeric str =
      String.filter (\c -> Char.isAlphaNum c || c == '-') str
    
    normalizeString : String -> String
    normalizeString str =
      str
        |> String.toLower
        |> String.replace "ä" "a"
        |> String.replace "ö" "o"
        |> String.replace "ü" "u"
        |> String.replace "ß" "ss"
  in
  title
    |> String.toLower
    |> applyReplacements
    |> normalizeString
    |> removeNonAlphanumeric


type Msg
  = UpdateSearch String
  | SelectSite String
  | AgreeTOS
  | DisagreeTOS
  | StartGeneration
  | FinishGeneration (Result Http.Error CreditCard)
  | SetError String
  | MouseMove Float Float
  | CatchMe
  | StartDisappearing
  | FinishDisappearing
  | GotViewport Browser.Dom.Viewport
  | WindowResized Int Int
  | UpdateFirstName String
  | UpdateLastName String
  | UpdatePostalCode String
  | SubmitPersonalInfo
  | GotSites (Result Http.Error (List Site))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateSearch term ->
      let
        suggestions = 
          model.sites
            |> List.filter (\site -> String.contains (String.toLower term) (String.toLower site.name))
            |> List.take 3
            |> List.map .name
      in
      ( { model | searchTerm = term, suggestions = suggestions }, Cmd.none )

    SelectSite site ->
      case List.filter (\s -> s.name == site) model.sites of
        [] ->
          ( model, Cmd.none )
        (selectedSite :: _) ->
          if selectedSite.supported then
            ( { model | selectedWebsite = Just site, step = InputPersonalInfo, error = Nothing }, Cmd.none )
          else
            ( { model | error = Just (site ++ " is not supported.") }, Cmd.none )

    UpdateFirstName name ->
      ( { model | firstName = name }, Cmd.none )

    UpdateLastName name ->
      ( { model | lastName = name }, Cmd.none )

    UpdatePostalCode code ->
      ( { model | postalCode = code }, Cmd.none )

    SubmitPersonalInfo ->
      if String.length model.firstName > 2 && String.length model.lastName > 2 && isValidPostalCode model.postalCode then
        ( { model | step = TermsOfService }, Cmd.none )
      else
        ( { model | error = Just "Please ensure first name and last name are over 2 letters each, and the postal code is valid." }, Cmd.none )

    AgreeTOS ->
      ( { model | tosAgreed = True, step = Generating }
      , Http.post
          { url = "https://api.trial.cards/generate-card"
          , body = Http.jsonBody (Encode.object
              [ ("website", Encode.string (Maybe.withDefault "" model.selectedWebsite))
              , ("firstName", Encode.string model.firstName)
              , ("lastName", Encode.string model.lastName)
              , ("postalCode", Encode.string model.postalCode)
              ])
          , expect = Http.expectJson FinishGeneration creditCardDecoder
          }
      )

    DisagreeTOS ->
      ( { model | disagreeCount = model.disagreeCount + 1 }, Cmd.none )

    StartGeneration ->
      ( model, Cmd.none )

    FinishGeneration (Ok card) ->
      ( { model | generatedCard = Just card, step = DisplayCard }, Cmd.none )

    FinishGeneration (Err _) ->
      ( { model | error = Just "Error generating card" }, Cmd.none )

    SetError errorMsg ->
      ( { model | error = Just errorMsg }, Cmd.none )

    MouseMove x y ->
      ( { model | mousePosition = (x, y) }, Cmd.none )

    CatchMe ->
      ( { model | caughtState = JustCaught }
      , Task.perform (\_ -> StartDisappearing) (Process.sleep 2000)
      )

    StartDisappearing ->
      ( { model | caughtState = Disappearing }
      , Task.perform (\_ -> FinishDisappearing) (Process.sleep 1000)
      )

    FinishDisappearing ->
      ( { model | caughtState = Disappeared }, Cmd.none )

    GotViewport viewport ->
      ( { model | viewport = Just { width = viewport.viewport.width, height = viewport.viewport.height } }, Cmd.none )

    WindowResized width height ->
      ( { model | viewport = Just { width = toFloat width, height = toFloat height } }, Cmd.none )

    GotSites result ->
      case result of
        Ok sites ->
          ( { model | sites = sites }, Cmd.none )
        Err _ ->
          ( { model | error = Just "Failed to load sites" }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Browser.Events.onResize WindowResized


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container", onMouseMove ]
    [ h1 [ class "header" ] [ text "Trial Credit Card Generator" ]
    , case model.step of
        SelectWebsite ->
          viewWebsiteSelector model

        InputPersonalInfo ->
          viewPersonalInfoInput model

        TermsOfService ->
          viewTermsOfService model

        Generating ->
          viewGenerating

        DisplayCard ->
          viewGeneratedCard model
    ]

viewWebsiteSelector : Model -> Html Msg
viewWebsiteSelector model =
  let
    rejectedSites = 
      [ "Amazon Web Services", "DigitalOcean", "Google Cloud", "Microsoft Azure", "Linode", "Vultr", "Heroku", "IBM Cloud", "Oracle Cloud", "Rackspace", "Alibaba Cloud", "Scaleway", "OVHcloud", "Hetzner", "GoDaddy", "Namecheap", "Cloudflare", "Netlify", "Vercel", "GitHub", "GitLab", "Bitbucket", "Twilio", "Stripe", "PayPal", "Square", "Shopify", "WooCommerce", "Salesforce", "HubSpot", "Zendesk", "Atlassian", "New Relic", "Datadog", "Splunk", "Sumo Logic", "LogDNA", "PagerDuty", "SendGrid", "Mailgun", "Mailchimp", "ConvertKit", "Zoom", "Slack", "Asana", "Trello", "Monday.com", "Basecamp", "Dropbox", "Box", "Google Workspace", "Microsoft 365", "Adobe Creative Cloud", "Canva Pro", "InVision", "Figma", "Docker Hub", "CircleCI", "Travis CI", "Jenkins", "JFrog", "Sentry", "Loggly", "Papertrail", "Auth0", "Okta", "LastPass", "1Password", "Dashlane" ]
    
    handleSiteSelection site =
      if List.member site.name rejectedSites then
        SetError ("Sorry, we don't support generating cards for " ++ site.name ++ ".")
      else
        SelectSite site.name
  in
  div [ class "step" ]
    [ h2 [] [ text "Select a website" ]
    , input 
        [ type_ "text"
        , placeholder "Search for a website"
        , value model.searchTerm
        , onInput UpdateSearch
        , class "search-input"
        ] []
    , viewError model.error
    , ul [ class "suggestions" ] 
        (List.map 
          (\site -> 
            li [ onClick (handleSiteSelection site) ] 
              [ img [ src ("/icons/" ++ site.slug ++ ".svg"), class "site-icon" ] []
              , text site.name 
              ]
          ) 
          (List.filter (\site -> List.member site.name model.suggestions) model.sites
           |> List.take 3)  -- Limit to 3 suggestions
        )
    ]

viewPersonalInfoInput : Model -> Html Msg
viewPersonalInfoInput model =
  div [ class "step" ]
    [ h2 [] [ text "Enter Personal Information" ]
    , div [ class "personal-info" ]
        [ input [ type_ "text", value model.firstName, onInput UpdateFirstName, placeholder "First Name" ] []
        , input [ type_ "text", value model.lastName, onInput UpdateLastName, placeholder "Last Name" ] []
        , input [ type_ "text", value model.postalCode, onInput UpdatePostalCode, placeholder "Postal Code" ] []
        ]
    , button [ class "button agree", onClick SubmitPersonalInfo ] [ text "Continue" ]
    , viewError model.error
    ]
viewTermsOfService : Model -> Html Msg
viewTermsOfService model =
  div [ class "step" ]
    [ h2 [] [ text "Terms of Service" ]
    , div [ class "tos-box" ] 
        [ p [] [ text "Welcome to trial.cards. By using this service, you agree to be bound by the following terms and conditions:" ]
        , h3 [] [ text "1. Service Description" ]
        , p [] [ text "trial.cards provides virtual credit card numbers for online transactions. These virtual credit card numbers are linked to real financial accounts and can be used for actual online transactions. However, usage is subject to the terms and conditions outlined herein." ]
        , h3 [] [ text "2. User Responsibilities" ]
        , p [] [ text "You agree to use this service only for lawful purposes and in accordance with these Terms. You must not use the service for any illegal or unauthorized purpose. Any unauthorized use may result in termination of your access to the service and potential legal action." ]
        , h3 [] [ text "3. Account Security" ]
        , p [] [ text "You are responsible for maintaining the confidentiality of your account information, including your password, and for all activities that occur under your account. You agree to immediately notify us of any unauthorized use of your account or any other breach of security." ]
        , h3 [] [ text "4. Disclaimer of Warranties" ]
        , p [] [ text "This service is provided 'as is' without any warranties, express or implied. We do not guarantee the accuracy, completeness, or usefulness of any generated card numbers. Your use of the service is at your own risk. We disclaim all warranties, including but not limited to the implied warranties of merchantability, fitness for a particular purpose, and non-infringement." ]
        , h3 [] [ text "5. Limitation of Liability" ]
        , p [] [ text "We shall not be liable for any indirect, incidental, special, consequential, or punitive damages resulting from your use of or inability to use the service, even if we have been advised of the possibility of such damages. Our total liability to you for any damages, losses, and causes of action shall not exceed the amount paid by you, if any, for accessing this site." ]
        , h3 [] [ text "6. Indemnification" ]
        , p [] [ text "You agree to indemnify and hold us harmless from any claims, losses, or damages, including legal fees, arising from your use of the service or violation of these Terms." ]
        , h3 [] [ text "7. Modifications to Service" ]
        , p [] [ text "We reserve the right to modify or discontinue the service at any time without notice. We shall not be liable to you or any third party should we exercise our right to modify or discontinue the service." ]
        , h3 [] [ text "8. Governing Law" ]
        , p [] [ text "These Terms shall be governed by and construed in accordance with the laws of the jurisdiction in which the service provider is established, without regard to its conflict of law principles." ]
        , h3 [] [ text "9. Contact Information" ]
        , p [] [ text "If you have any questions about these Terms, please contact us at amiragassi04@gmail.com." ]
        ]
    , div [ class "button-group" ]
        [ viewDisagreeButton model
        , button [ class "button agree", onClick AgreeTOS ] [ text "Agree" ]
        ]
    ]

viewDisagreeButton : Model -> Html Msg
viewDisagreeButton model =
  let
    buttonText =
      case model.disagreeCount of
        0 -> "Disagree"
        1 -> "Umm... What? Are you sure?"
        2 -> "Just click agree."
        3 -> "Can you like, read the TOS maybe."
        4 -> "Dude."
        _ -> 
          case model.caughtState of
            NotCaught -> "Okay wise guy."
            JustCaught -> "Aw, you caught me!"
            Disappearing -> "Nerd."
            Disappeared -> ""

    (mouseX, mouseY) = model.mousePosition
    (buttonLeft, buttonTop) = calculateButtonPosition model.viewport mouseX mouseY

    buttonStyle =
      if model.disagreeCount >= 5 then
        [ style "position" "fixed"
        , style "left" buttonLeft
        , style "top" buttonTop
        , style "transform" "translate(-50%, -50%)"
        , style "transition" "opacity 1s ease-out, left 0.5s ease-out, top 0.5s ease-out"
        , style "opacity" (if model.caughtState == Disappearing then "0" else "1")
        ]
      else
        []

    buttonClass =
      "button disagree" ++ if model.disagreeCount >= 5 then " disagree-running" else ""

    buttonAttributes =
      if model.disagreeCount >= 5 && model.caughtState == NotCaught then
        buttonStyle ++ [ class buttonClass, onClick CatchMe ]
      else if model.disagreeCount < 5 then
        buttonStyle ++ [ class buttonClass, onClick DisagreeTOS ]
      else
        buttonStyle ++ [ class buttonClass ]
  in
  if model.caughtState /= Disappeared then
    button buttonAttributes [ text buttonText ]
  else
    text ""

viewGenerating : Html Msg
viewGenerating =
  div [ class "step" ]
    [ 
     div [ class "loading-spinner" ] []
    ]

viewGeneratedCard : Model -> Html Msg
viewGeneratedCard model =
  case model.generatedCard of
    Just card ->
      div [ class "step" ]
        [ div [ class "credit-card" ]
            [ div 
                [ class ("card-issuer-logo " ++ String.toLower card.company ++ "-logo") ] 
                []
            , div [ class "website-logo-container" ]
                [ img 
                    [ src ("/icons/" ++ (titleToSlug card.website) ++ ".svg")
                    , class "website-logo"
                    , style "filter" "invert(1)"
                    ] 
                    []
                ]
            , div [ class "card-number-container" ]
                [ div [ class "card-header" ] [ text "Card Number" ]
                , div [ class "card-number" ] 
                    [ text (String.join " " (List.map (String.slice 0 4) (List.map (\i -> String.slice i (i + 4) card.number) [0, 4, 8, 12]))) ]
                ]
            , div [ class "card-name-container" ]
                [div [ class "card-name" ] [ text (model.firstName ++ " " ++ model.lastName) ]
                ]
            , div [ class "card-details" ]
                [ div [ class "card-expiry-container" ]
                    [ div [ class "card-header" ] [ text "Expiry Date" ]
                    , div [ class "card-expiry" ] [ text card.expiry ]
                    ]
                , div [ class "card-cvv-container" ]
                    [ div [ class "card-header" ] [ text "CVC" ]
                    , div [ class "card-cvv" ] [ text card.cvv ]
                    ]
                ]
            , div 
                [ class 
                    ("card-logo " ++ 
                        if String.startsWith "4" card.number then 
                            "visa-logo" 
                        else 
                            "mastercard-logo"
                    )
                ] 
                []
            ]
        , div [ class "warning", style "text-align" "center" ]
            [ strong [] [ text ("Make sure to use your full name and postal code when activating your trial on " ++ card.website ++ ".") ]
            , br [] []
            , br [] []
            , text "Incorrect information or banned merchants may be rejected."
            ]
        ]

    Nothing ->
      div [] [ text "Error generating card" ]


viewError : Maybe String -> Html Msg
viewError maybeError =
  case maybeError of
    Just error ->
      div [ class "error" ] [ text error ]

    Nothing ->
      text ""


-- HELPERS

creditCardDecoder : Decode.Decoder CreditCard
creditCardDecoder =
  Decode.map5 CreditCard
    (Decode.field "website" Decode.string)
    (Decode.field "number" Decode.string)
    (Decode.field "expiry" Decode.string)
    (Decode.field "cvv" Decode.string)
    (Decode.field "company" Decode.string)

calculateButtonPosition : Maybe Viewport -> Float -> Float -> (String, String)
calculateButtonPosition maybeViewport mouseX mouseY =
  case maybeViewport of
    Just viewport ->
      let
        safeDistance = 100  -- pixels
        maxX = viewport.width - safeDistance
        maxY = viewport.height - safeDistance
        
        targetX = 
          if mouseX < viewport.width / 2 then
            Basics.max safeDistance (mouseX + 100)
          else
            Basics.min maxX (mouseX - 100)
            
        targetY = 
          if mouseY < viewport.height / 2 then
            Basics.max safeDistance (mouseY + 100)
          else
            Basics.min maxY (mouseY - 100)
      in
      ( String.fromFloat targetX ++ "px", String.fromFloat targetY ++ "px" )
    
    Nothing ->
      ( "50%", "50%" )  -- Fallback position

onMouseMove : Attribute Msg
onMouseMove =
  on "mousemove" (Decode.map2 MouseMove
    (Decode.field "clientX" Decode.float)
    (Decode.field "clientY" Decode.float))


isValidPostalCode : String -> Bool
isValidPostalCode code =
  let
    cleanCode = String.filter (\c -> c /= ' ' && c /= '-') (String.toUpper code)
    isUSCode = String.length cleanCode == 5 && String.all Char.isDigit cleanCode
    isCanadaCode = 
      String.length cleanCode == 6 &&
      Char.isAlpha (String.uncons cleanCode |> Maybe.map Tuple.first |> Maybe.withDefault ' ') &&
      Char.isDigit (String.dropLeft 1 cleanCode |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault ' ') &&
      Char.isAlpha (String.dropLeft 2 cleanCode |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault ' ') &&
      Char.isDigit (String.dropLeft 3 cleanCode |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault ' ') &&
      Char.isAlpha (String.dropLeft 4 cleanCode |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault ' ') &&
      Char.isDigit (String.dropLeft 5 cleanCode |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault ' ')
  in
  isUSCode || isCanadaCode
