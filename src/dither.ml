open! Core

(* This should look familiar by now! *)

let is_in x y image =
  x > -1 && x < Image.width image && y > -1 && y < Image.height image
;;

let flip_and_error pixel threshold =
  let total = Pixel.red pixel in
  if Float.( >. ) (float_of_int total) (float_of_int threshold /. 2.0)
  then true, total - threshold
  else false, total
;;

let change_pixel_by image change x y =
  let rounded = Int.of_float (Float.round_nearest change) in
  Image.set
    image
    ~x
    ~y
    (Pixel.( + ) (Image.get image ~x ~y) (rounded, rounded, rounded))
;;

let transform image =
  let gray_image = Grayscale.transform image in
  Image.mapi gray_image ~f:(fun ~x ~y pixel ->
    let max = Image.max_val image in
    let white, error = flip_and_error pixel max in
    if is_in (x + 1) y image
    then
      change_pixel_by
        gray_image
        (float_of_int error *. 7.0 /. 16.0)
        (x + 1)
        y;
    if is_in (x - 1) (y + 1) image
    then
      change_pixel_by
        gray_image
        (float_of_int error *. 3.0 /. 16.0)
        (x - 1)
        (y + 1);
    if is_in x (y + 1) image
    then
      change_pixel_by gray_image (float_of_int error *. 5.0 /. 16.0) x (y + 1);
    if is_in (x + 1) (y + 1) image
    then
      change_pixel_by gray_image (float_of_int error /. 16.0) (x + 1) (y + 1);
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

let%expect_test "dither test" =
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  let generated =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let result =
    Image.foldi generated ~init:true ~f:(fun ~x ~y state pixel ->
      if state && not (Pixel.equal pixel (Image.get reference ~x ~y))
      then (
        print_endline
          ("first error: expected "
           ^ Pixel.to_string (Image.get reference ~x ~y)
           ^ " got "
           ^ Pixel.to_string pixel
           ^ " at "
           ^ Int.to_string x
           ^ " "
           ^ Int.to_string y);
        false)
      else state)
  in
  print_endline (Bool.to_string result);
  [%expect {|true|}]
;;
