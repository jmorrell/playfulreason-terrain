/*
https://github.com/BuckleTypes/bs-webapi-incubator has Canvas bindings
in master, but they are not included in the last version published to
npm
*/

type context;

external getContext : Dom.element => string => context = "getContext" [@@bs.send];

external beginPath : context => unit = "beginPath" [@@bs.send];
external strokeStyle : context => string => unit = "strokeStyle" [@@bs.set];
external stroke : context => unit = "stroke" [@@bs.send];
external lineWidth : context => int => unit = "lineWidth" [@@bs.set];
external moveTo : context => int => int => unit = "moveTo" [@@bs.send];
external lineTo : context => int => int => unit = "lineTo" [@@bs.send];
external clearRect : context => int => int => int => int => unit = "clearRect" [@@bs.send];

external fillStyle : context => string => unit = "fillStyle" [@@bs.set];
external fillRect : context => int => int => int => int => unit = "fillRect" [@@bs.send];
