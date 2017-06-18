/* config */

let maxParticles = 4000;
let newParticlesPerFrame = 10;
let damping = 0.999;
let gravity = 0.3;

/* externals */

open ReasonJs.Dom;

/* main app state */

type vecT = {
  mutable x: float,
  mutable y: float
};

type particleT = {
  pos: vecT,
  oldPos: vecT
};

type windowT = {
  mutable width: int,
  mutable height: int
};

type particleSpeed = {
  vX: float,
  vY: float
};

type stateT = {
  mutable particles: array particleT,
  window: windowT,
};

let state : stateT = {
  particles: [| |],
  window: {
    width: 0,
    height: 0
  }
};

/* canvas/context setup */

let width = Window.innerWidth window;
let height = Window.innerHeight window;

state.window.width = width;
state.window.height = height;

let canvas = Document.createElement "canvas" document;
let ctx = Canvas.getContext canvas "2d";

document
  |> Document.asHtmlDocument
  |> Option.andThen  HtmlDocument.body
  |> Option.map (Element.appendChild canvas);

Element.setAttribute "height" (string_of_int height) canvas;
Element.setAttribute "width" (string_of_int width) canvas;

/* app code */

let genItems num callback => {
  let emptyArray = Array.make num 0;

  Array.map (fun i => {
    callback i;
  }) emptyArray;
};

let addParticles num callback => {
  let newParticles = genItems num callback;
  state.particles = Array.append state.particles newParticles;
  ();
};

let integrateParticle p => {
  let velocityX = (p.pos.x -. p.oldPos.x) *. damping;
  let velocityY = (p.pos.y -. p.oldPos.y) *. damping;
  p.oldPos.x = p.pos.x;
  p.oldPos.y = p.pos.y;
  p.pos.x = p.pos.x +. velocityX;
  p.pos.y = p.pos.y +. velocityY;
  ();
};

let getVelocity p => {
  {
    vX: p.pos.x -. p.oldPos.x,
    vY: p.pos.y -. p.oldPos.y
  }
};

let move p x y => {
   p.pos.x = p.pos.x +. x;
   p.pos.y = p.pos.y +. y;
   ();
};

let bounce p => {
  if (p.pos.y > float_of_int height) {
    let velocity = getVelocity p;
    p.oldPos.y = float_of_int height;
    p.pos.y = p.oldPos.y -. velocity.vY *. 0.3;
  };
  ();
};

let updateParticle p => {
  move p 0. gravity;
  integrateParticle p;
  bounce p;
  ();
};

let update state => {
  if (Array.length state.particles < maxParticles) {
    addParticles newParticlesPerFrame (fun _ => {
      let x = (float_of_int width) *. 0.5;
      let y = float_of_int height;
      let p = { pos: { x, y }, oldPos: { x, y } };
      move p (Random.float 4. -. 2.) (Random.float (-10.) -. 15.);
      p;
    });
  };
  Array.iter updateParticle state.particles;
};

let drawBackground color width height => {
  Canvas.fillStyle ctx color;
  Canvas.fillRect ctx 0 0 width height;
};

let drawParticle p => {
  Canvas.strokeStyle ctx "#0099ff";
  Canvas.lineWidth ctx 3;
  Canvas.beginPath ctx;
  Canvas.moveTo ctx (int_of_float p.oldPos.x) (int_of_float p.oldPos.y);
  Canvas.lineTo ctx (int_of_float p.pos.x) (int_of_float p.pos.y);
  Canvas.stroke ctx;
  ();
};

let draw state => {
  drawBackground "#000000" width height;
  Array.iter drawParticle state.particles;
};

let rec loop () => {
  update state;
  draw state;
  ReasonJs.requestAnimationFrame loop;
};

loop ();
