/* externals */

open Bs_webapi.Dom;
open Bs_webapi.Canvas;

/* canvas/context setup */

let width = Window.innerWidth window;
let height = Window.innerHeight window;

let canvas = Document.createElement "canvas" document;
let ctx = CanvasElement.getContext2d canvas;

document
  |> Document.asHtmlDocument
  |> Js.Option.andThen ((fun doc => HtmlDocument.body doc) [@bs])
  |> Js.Option.map ((fun doc => Element.appendChild canvas doc) [@bs]);

Element.setAttribute "height" (string_of_int height) canvas;
Element.setAttribute "width" (string_of_int width) canvas;

/* app code */

let drawBackground context color width height => {
  Canvas2d.setFillStyle context String color;
  Canvas2d.fillRect x::0. y::0. w::(float_of_int width) h::(float_of_int height) context;
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
