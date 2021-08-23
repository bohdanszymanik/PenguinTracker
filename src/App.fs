module App

open FSharp.Core
open Elmish
open Elmish.React
open Feliz

type Point = { Lat: float; Lng: float }
// type Box =
//   { BoxNum: int
//     Adults: int option
//     Eggs: int option
//     Chicks: int option
//     Location: Point
//   }

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
      Boxes: Boxes }

type Msg =
    | Increment
    | Decrement
    | BoxClick of int


let init() =

    let boxes = Map.empty.
                  Add(1, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.34915; Lng = 174.77286007970827} }).
                  Add(2, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.3495; Lng = 174.7725} }).
                  Add(3, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.3495; Lng = 174.7732} }).
                  Add(4, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.3493; Lng = 174.7738} }).
                  Add(5, {Adults = None; Eggs = None; Chicks = None; Location = {Lat = -41.3491; Lng = 174.7739} })
                    
    { Count = 0
      Boxes = boxes }

let update (msg: Msg) (state: State): State =
    match msg with
    | Increment ->
        { state with Count = state.Count + 1 }

    | Decrement ->
        { state with Count = state.Count - 1 }

    | BoxClick i->
        state

open Feliz.PigeonMaps

let LINZBasemap x y z dpr =
    // sprintf "https://stamen-tiles.a.ssl.fastly.net/terrain/%A/%A/%A.png" z x y
    printfn "z:%A x:%A y:%A" z x y
    printfn "https://basemaps.linz.govt.nz/v1/tiles/aerial/EPSG:3857/%A/%A/%A.webp?api=%s" z x y "c01fdbeerst1kd0r05hkbwa2k6z"
    sprintf "https://basemaps.linz.govt.nz/v1/tiles/aerial/EPSG:3857/%A/%A/%A.webp?api=%s" z x y "c01fdbeerst1kd0r05hkbwa2k6z"

let makeMarker (i:int) (box:BoxStatus) : IMapMarker =
  PigeonMaps.marker [
      marker.anchor(box.Location.Lat, box.Location.Lng)
      marker.offsetLeft 15
      marker.offsetTop 30
      // marker.onClick (fun _ -> marker.
      // TODO - some how pop up a modal dialog using fulma Modal component
      
      // dispatch Msg.BoxClick
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


let pigeonMap (boxes: Boxes) = 
    PigeonMaps.map [
        map.center(-41.34929470266615, 174.77286007970827)
        map.zoom 18
        map.maxZoom 24
        map.height 350
        map.provider LINZBasemap
        map.markers [
            for KeyValue(k,v) in boxes do makeMarker k v
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [ prop.children [ 
                Html.button [
                  prop.onClick (fun _ -> dispatch Msg.Increment)
                  prop.text "Increment"
                ]

                pigeonMap state.Boxes

                Html.button [
                  prop.onClick (fun _ -> dispatch Decrement)
                  prop.text "Decrement"
                ]
                Html.h1 state.Count
              ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run