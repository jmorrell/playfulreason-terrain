/* externals */

open ReasonJs.Dom;

/* canvas/context setup */

let width = Window.innerWidth window;
let height = Window.innerHeight window;

let canvas = Document.createElement "canvas" document;
let ctx = Canvas.getContext canvas "2d";

document
  |> Document.asHtmlDocument
  |> Option.andThen  HtmlDocument.body
  |> Option.map (Element.appendChild canvas);

Element.setAttribute "height" (string_of_int height) canvas;
Element.setAttribute "width" (string_of_int width) canvas;

/* app code */

let drawBackground context color width height => {
  Canvas.fillStyle context color;
  Canvas.fillRect context 0. 0. (float_of_int width) (float_of_int height);
};

drawBackground ctx "#000000" width height;

let terrain = Terrain.create 9;
Js.log terrain;
Terrain.generate terrain 0.5;
Terrain.draw terrain ctx width height;
