open! Core

(* This should look familiar by now! *)

let is_in x y image =
  x > -1 && x < Image.width image && y > -1 && y < Image.height image
;;

let flip_and_error pixel threshold =
  let total = Pixel.red pixel in
  if total > threshold / 2 then true, total - threshold else false, total
;;

let change_pixel_by image change x y =
  Image.set
    image
    ~x
    ~y
    (Pixel.( + ) (Image.get image ~x ~y) (change, change, change))
;;

let transform image =
  let gray_image = Grayscale.transform image in
  Image.mapi gray_image ~f:(fun ~x ~y pixel ->
    let max = Image.max_val image in
    let white, error = flip_and_error pixel max in
    if is_in (x + 1) y image
    then change_pixel_by gray_image (error * 7 / 16) (x + 1) y;
    if is_in (x - 1) (y + 1) image
    then change_pixel_by gray_image (error * 3 / 16) (x - 1) (y + 1);
    if is_in x (y + 1) image
    then change_pixel_by gray_image (error * 5 / 16) x (y + 1);
    if is_in (x + 1) (y + 1) image
    then change_pixel_by gray_image (error / 16) (x + 1) (y + 1);
    if white then max, max, max else Pixel.zero)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
