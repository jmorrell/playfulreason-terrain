/* externals */

open ReasonJs.Dom;

/* canvas/context setup */

let width = Window.innerWidth window;
let height = Window.innerHeight window;

let canvas = Document.createElement "canvas" document;
let ctx = Canvas.getContext canvas "2d";

document
  |> Document.asHtmlDocument
  |> Option.andThen HtmlDocument.body
  |> Option.map (Element.appendChild canvas);

Element.setAttribute "height" (string_of_int height) canvas;
Element.setAttribute "width" (string_of_int width) canvas;

/* app code */

let drawBackground context color width height => {
  Canvas.fillStyle context color;
  Canvas.fillRect context 0. 0. (float_of_int width) (float_of_int height);
};

drawBackground ctx "#000000" width height;

let t1 = Terrain.create 1;
let t2 = Terrain.refine t1 5.0;
let t3 = Terrain.refine t2 5.0;
let t4 = Terrain.refine t3 5.0;
let t5 = Terrain.refine t4 5.0;
let t6 = Terrain.refine t5 5.0;
let t7 = Terrain.refine t6 1.0;
let t8 = Terrain.refine t7 1.0;
let t9 = Terrain.refine t8 5.0;
Terrain.draw t9 ctx width height;
