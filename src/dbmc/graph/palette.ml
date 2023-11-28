let int_of_rgb r g b = (r * 256 * 256) + (g * 256) + b
let white = int_of_rgb 255 255 255
let light = int_of_rgb 200 200 200
let dark = int_of_rgb 100 100 100
let black = int_of_rgb 0 0 0
let red = int_of_rgb 255 0 0
let light_purple = int_of_rgb 203 201 226
let red_orange = int_of_rgb 253 190 133
let blue = int_of_rgb 0 0 255
let cyan = int_of_rgb 0 200 200
let lime = int_of_rgb 0 255 0

let greens_with_alert =
  [
    red_orange;
    int_of_rgb 204 236 230;
    int_of_rgb 153 216 201;
    int_of_rgb 102 194 164;
    int_of_rgb 44 162 95;
    int_of_rgb 0 109 44;
  ]
