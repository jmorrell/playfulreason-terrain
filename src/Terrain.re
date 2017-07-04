
type t = {
  detail: int,
  size: int,
  max: int,
  map: PointMap.t
};

type point = {
  x: float,
  y: float,
};

let create detail => {
  let size = (Js_math.pow_int base::2 exp::detail) + 1;
  let max = size - 1;
  let map = PointMap.make size;
  { size, max, map, detail };
};

let needsSquare x y => x mod 2 == 1 && y mod 2 == 1;
let needsDiamond x y => (x mod 2 == 1) != (y mod 2 == 1);

let getSquare m x y => {
  [|
    /* upper left */
    PointMap.get m (x - 1) (y - 1),
    /* upper right */
    PointMap.get m (x + 1) (y - 1),
    /* lower right */
    PointMap.get m (x + 1) (y + 1),
    /* lower left */
    PointMap.get m (x - 1) (y + 1),
  |];
};

let getDiamond m x y => {
  [|
    /* top */
    PointMap.get m x (y - 1),
    /* right */
    PointMap.get m (x + 1) y,
    /* bottom */
    PointMap.get m x (y + 1),
    /* left */
    PointMap.get m (x - 1) y,
  |]
};

let average values => {
  let valid = Js.Array.filter (Option.isSome) values;
  let total = Js.Array.reduce (fun acc x => {
    switch x {
      | Some v => acc +. v
      | None => acc
    };
  }) 0. values;
  let len = float_of_int (Array.length valid);
  total /. len;
};

let refine terrain roughness => {
  let t = create (terrain.detail + 1);

  /* Copy over the values from the smaller map */
  PointMap.iter terrain.map (fun x y v => {
    PointMap.set t.map (x * 2) (y * 2) (2. *. v);
  });

  /* For each of the empty fields, set a new value */
  PointMap.iter t.map (fun x y _v => {
    if (needsSquare x y) {
      let avg = average (getSquare t.map x y);
      let offset = (Js.Math.random () -. 0.5) *. roughness;
      PointMap.set t.map x y (avg +. offset);
    };
  });

  PointMap.iter t.map (fun x y _v => {
    if (needsDiamond x y) {
      let avg = average (getDiamond t.map x y);
      let offset = (Js.Math.random () -. 0.5) *. roughness;
      PointMap.set t.map x y (avg +. offset);
    };
  });

  t;
};

let draw terrain context width height => {
  let waterVal = float_of_int terrain.size *. 0.1;
  let {size, max, map} = terrain;

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

  let someOrZero = fun
    | Some x => x
    | None => 0.;

  PointMap.iter map (fun x y v => {
    let v' = someOrZero (PointMap.get map (x + 1) y);
    let top = project x y v;
    let bottom = project (x + 1) y 0.;
    let water = project x y waterVal;
    let style = brightness x y (v' -. v);
    rect context top bottom style;
    rect context water bottom "rgba(50, 150, 200, 0.15)";
  });
};
