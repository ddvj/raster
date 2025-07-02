open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image ~threshold =
  let max = Image.max_val image in
  let bar = float_of_int threshold *. 0.01 *. float_of_int max in
  Image.map image ~f:(fun pixel ->
    Tuple3.map pixel ~f:(fun color ->
      if Float.( >. ) (float_of_int color) bar then max - color else color))
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.int)
          ~doc:"Threshold to invert as a percentage"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~threshold in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
