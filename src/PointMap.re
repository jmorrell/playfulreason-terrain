
type t = {
  n: int,
  size: int,
  map: array float,
};

let make n => {
  {
    n,
    map:Array.make (n * n) 0.,
    size: n * n,
  }
};

let size m => m.n;

let get m x y => {
  if (x < 0 || x >= m.n || y < 0 || y >= m.n) {
    None
  } else {
    Some (Array.get m.map (x + m.n * y));
  }
};

let set m x y v => {
  if (x < 0 || x >= m.n || y < 0 || y >= m.n) {
    ()
  } else {
    Array.set m.map (x + m.n * y) v;
  }
};

let iter m f => {
  Array.iteri (fun i v => {
    let y = i / m.n;
    let x = i - y * m.n;
    f x y v;
  }) m.map;
};
