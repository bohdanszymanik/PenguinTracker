module App

open FSharp.Core
open Elmish
open Elmish.React
open Feliz
open Feliz.PigeonMaps
open Feliz.Bulma
open Fulma
open Leaflet
open Fable.Core.JsInterop
// open Fable.React

module RL = ReactLeaflet
importAll "../node_modules/leaflet/dist/leaflet.css"
Leaflet.icon?Default?imagePath <- "//cdnjs.cloudflare.com/ajax/libs/leaflet/1.3.1/images/"
type RLMarker = { info: string; position: LatLngExpression}

type Point = { Lat: float; Lng: float }
type BoxStatus =
  { 
    Adults: int option
    Eggs: int option
    Chicks: int option
    Location: Point
  }

type Boxes = Map<int, BoxStatus>
type State =
    { Count: int
      Boxes: Boxes
      AdultInput: int
      EggsInput: int
      ChicksInput: int
      DisplayPopover: bool }

type Msg =
    | Increment
    | Decrement
    | BoxClick of int
    | SetAdultInput of int
    | SetEggsInput of int
    | SetChicksInput of int
    | DisplayPopover


let init() =

    // some dummy locations
    let boxes = Map.empty.
                  Add(1, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.34915; Lng = 174.77286007970827} }).
                  Add(2, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.3495; Lng = 174.7725} }).
                  Add(3, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.3495; Lng = 174.7732} }).
                  Add(4, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.3493; Lng = 174.7738} }).
                  Add(5, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.3491; Lng = 174.7739} })
                    
    { Count = 0
      Boxes = boxes 
      AdultInput = 0
      EggsInput = 0
      ChicksInput = 0
      DisplayPopover = false }

let update (msg: Msg) (state: State): State =
    match msg with
    | Increment ->
        { state with Count = state.Count + 1 }

    | Decrement ->
        { state with Count = state.Count - 1 }

    | BoxClick i->
        state

    | SetAdultInput i -> { state with AdultInput = i }
    | SetEggsInput i -> { state with EggsInput = i }
    | SetChicksInput i -> { state with ChicksInput = i }

    | DisplayPopover -> 
                        printfn "%A" state.DisplayPopover
                        { state with DisplayPopover = not state.DisplayPopover }


let LINZBasemap x y z dpr =
    // sprintf "https://stamen-tiles.a.ssl.fastly.net/terrain/%A/%A/%A.png" z x y
    printfn "z:%A x:%A y:%A" z x y
    printfn "https://basemaps.linz.govt.nz/v1/tiles/aerial/EPSG:3857/%A/%A/%A.webp?api=%s" z x y "c01fdbeerst1kd0r05hkbwa2k6z"
    sprintf "https://basemaps.linz.govt.nz/v1/tiles/aerial/EPSG:3857/%A/%A/%A.webp?api=%s" z x y "c01fdbeerst1kd0r05hkbwa2k6z"

let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Penguin nest box tracker"
  ]


let makeMarker (i:int) (box:BoxStatus) (dispatch: Msg -> unit) : IMapMarker =
  PigeonMaps.marker [
      marker.anchor(box.Location.Lat, box.Location.Lng)
      marker.offsetLeft 15
      marker.offsetTop 30
      marker.onClick (fun _ -> dispatch DisplayPopover )

      // TODO - some how pop up a modal dialog using fulma Modal component
      
      marker.render (fun marker -> [
          Html.i [
              if marker.hovered
              then
                prop.text i
                prop.style [ style.color.red; style.cursor.pointer ]
              else prop.style [ style.color.yellowGreen; style.cursor.pointer ]
              prop.className [ "fa"; "fab fa-map-marker"; "fa-2x" ]
          ]
          // Html.img [ prop.src "LittleBlue.png" ; prop.style [style.width 20]]
      ])
  ]


let pigeonMap (boxes: Boxes) (dispatch: Msg -> unit) =
    PigeonMaps.map [
        map.center(-41.34929470266615, 174.77286007970827)
        map.zoom 18
        map.maxZoom 24
        map.height 350
        map.provider LINZBasemap
        map.markers [
            for KeyValue(k,v) in boxes do makeMarker k v dispatch
        ]
    ]


let buildMarker (marker: RLMarker): ReactElement =
    RL.marker 
      [ 
        RL.MarkerProps.Position marker.position ] 
      [ RL.popup 
          [ RL.PopupProps.Key marker.info]
          [ Control.p 
              [] 
              [ Html.p [ !!marker.info ] ]
            Html.form [
              Bulma.columns [
                  Bulma.column [
                      column.is2 // <-- note context helper here
                      prop.children [
                        Bulma.field.div [
                          Bulma.label "Adults"
                          Bulma.control.div [
                            Bulma.input.text [
                              prop.placeholder "0"
                            ]
                          ]
                        ]
                      ]
                  ]
                  Bulma.column [
                      column.is2 // <-- note context helper here
                      prop.children [
                          Bulma.button.button "Click me"
                      ]
                  ]
              ]
            ]

            Html.div [
              Html.input [
                prop.classes ["input"; "is-medium"]
                prop.valueOrDefault "test"
                // prop.onTextChange (SetEditedDescription >> dispatch)
              ]
            ]
            Control.p 
                [] 
                [ Button.a
                    [ Button.Size IsSmall
                      Button.Props [  ] ]
                    [ Icon.icon [ ]
                        [ ]
                      Html.span "Go to" ] ] ] ]   

let tile =
  RL.tileLayer
    [
      let url = sprintf "https://basemaps.linz.govt.nz/v1/tiles/aerial/EPSG:3857/{z}/{x}/{y}.webp?api=%s" "c01fc5hzfekvvz92x9z7yrwc5hr"
      RL.TileLayerProps.Url url
      RL.TileLayerProps.Attribution "&amp;<a href=&quot;https://www.linz.govt.nz/linz-copyright&quot;>LINZ CC BY 4.0</a> © <a href=&quot;https://www.linz.govt.nz/data/linz-data/linz-basemaps/data-attribution&quot;>Imagery Basemap contributors</a>"
      ]
    []

let mapBoxes (state: State) =
  let markers = 
    [for KeyValue(k,v) in state.Boxes do
      buildMarker { info = (string)k; position = Fable.Core.U3.Case3(v.Location.Lat, v.Location.Lng) } ]
    |> List.tail
    
  tile :: markers


let render (state: State) (dispatch: Msg -> unit) =
  Html.div [ prop.children [ 
                appTitle

                Html.button [
                  prop.onClick (fun _ -> dispatch Msg.Increment)
                  prop.text "Increment"
                ]

                pigeonMap state.Boxes dispatch
                match state.DisplayPopover with
                | true -> Html.div "popover"
                | false -> Html.div "no popover!"


                RL.map [
                    RL.MapProps.Animate false ;
                    RL.MapProps.Zoom 18.;
                    RL.MapProps.MaxZoom 22.;
                    RL.MapProps.Style [ Fable.React.Props.CSSProp.Height 500; Fable.React.Props.CSSProp.MinWidth 200; Fable.React.Props.CSSProp.Width Column.IsFull ];
                    RL.MapProps.Center ( Fable.Core.U3.Case3 (-41.34929470266615, 174.77286007970827))  ]
                    (mapBoxes state)


                Html.button [
                  prop.onClick (fun _ -> dispatch Decrement)
                  prop.text "Decrement"
                ]
                Html.h1 state.Count

              ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run