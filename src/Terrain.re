
open Js_typed_array;

type t = {
   size: int,
   max: int,
   map: Float32Array.t,
};

type projection = {
  x: float,
  y: float,
};

let create detail => {
  let size = (Js_math.pow_int base::2 exp::detail) + 1;
  let max = size - 1;
  let map = Float32Array.make (Array.make (size * size) 0.);
  { size, max, map };
};

let get terrain x y => {
  if (x < 0 || x > terrain.max || y < 0 || y > terrain.max) {
    -1.
  } else {
    /* why is this unsafe? */
    Float32Array.unsafe_get terrain.map (x + terrain.size * y);
  }
};

let set terrain x y v => {
  Float32Array.unsafe_set terrain.map (x + terrain.size * y) v;
};

let generate terrain roughness => {
  let {max} = terrain;

  let average values => {
    let valid = Js.Array.filter (fun x => x != -1.) values;
    let total = Js.Array.reduce (+.) 0. valid;
    let len = float_of_int (Array.length valid);
    total /. len;
  };

  let square x y size offset => {
    let avg = average [|
      /* upper left */
      get terrain (x - size) (y - size),
      /* upper right */
      get terrain (x + size) (y - size),
      /* lower right */
      get terrain (x + size) (y + size),
      /* lower left */
      get terrain (x - size) (y + size),
    |];

    set terrain x y (avg +. offset);
  };

  let diamond x y size offset => {
    let avg = average [|
      /* top */
      get terrain x (y - size),
      /* right */
      get terrain (x + size) y,
      /* bottom */
      get terrain x (y + size),
      /* left */
      get terrain (x - size) y,
    |];
    set terrain x y (avg +. offset);
  };

  let rec divide size => {
    let half = size / 2;
    let scale = float_of_int size *. roughness;

    if (half < 1) {
      ()
    } else {
      let y = ref half;
      while (!y < max) {
        let x = ref half;
        while (!x < max) {
          square !x !y half (Js.Math.random () *. scale *. 2. -. scale);
          x := !x + size;
        };
        y:= !y + size;
      };


      let y = ref 0;
      while (!y <= max) {
        let x = ref ((!y + half) mod size);
        while (!x <= max) {
          diamond !x !y half (Js.Math.random () *. scale *. 2. -. scale);
          x := !x + size;
        };
        y := !y + half;
      };

      divide (size / 2);
    }
  };

  set terrain 0 0 (float_of_int max);
  set terrain max 0 (float_of_int max /. 2.);
  set terrain max max 0.;
  set terrain 0 max (float_of_int max /. 2.);
  divide max;
};

let draw terrain context width height => {
  let waterVal = float_of_int terrain.size *. 0.3;
  let {size, max} = terrain;

  let rect context a b style => {
    if (b.y < a.y) {
      ();
    } else {
      Canvas.fillStyle context style;
      Canvas.fillRect context a.x a.y (b.x -. a.x) (b.y -. a.y);
    }
  };

  let brightness x y slope => {
    if (y == max || x == max) {
      "#000"
    } else {
      let b = (int_of_float (slope *. 50.)) + 128;
      {j|rgba($b,$b,$b,1)|j};
    };
  };

  let iso x y => {
    {
      x: 0.5 *. float_of_int (size + x - y),
      y: 0.5 *. float_of_int (x + y),
    }
  };

  let project flatX flatY flatZ => {
    let point = iso flatX flatY;
    let x0 = (float_of_int width) *. 0.5;
    let y0 = (float_of_int height) *. 0.2;
    let z = (float_of_int size) *. 0.5 -. flatZ +. point.y *. 0.75;
    let x = (point.x -. (float_of_int size) *. 0.5) *. 6.;
    let y = ((float_of_int size) -. point.y) *. 0.005 +. 1.;

    {
      x: x0 +. x /. y,
      y: y0 +. z /. y
    };
  };

  for y in 0 to (size - 1) {
    for x in 0 to (size - 1) {
      let v = get terrain x y;
      let top = project x y v;
      let bottom = project (x + 1) y 0.;
      let water = project x y waterVal;
      let style = brightness x y ((get terrain (x + 1) y) -. v);
      rect context top bottom style;
      rect context water bottom "rgba(50, 150, 200, 0.15)";
    };
  };
};
