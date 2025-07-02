open! Core

let is_in x y image =
  x > -1 && x < Image.width image && y > -1 && y < Image.height image
;;

let round_value current_value increment =
  let remainder = current_value % increment in
  let quo = current_value / increment in
  if current_value + remainder > increment * (quo + 1)
  then increment * (quo + 1)
  else increment * quo
;;

let update_pixel pixel increment : Pixel.t =
  ( round_value (Pixel.red pixel) increment
  , round_value (Pixel.green pixel) increment
  , round_value (Pixel.blue pixel) increment )
;;

let get_change before after =
  ( Pixel.red before - Pixel.red after
  , Pixel.green before - Pixel.green after
  , Pixel.blue before - Pixel.blue after )
;;

let change_pixel_by image change x y =
  let rounded =
    Tuple3.map change ~f:(fun x -> Int.of_float (Float.round_nearest x))
  in
  Image.set image ~x ~y (Pixel.( + ) (Image.get image ~x ~y) rounded)
;;

let adjust image x y proportion error =
  if is_in x y image
  then
    change_pixel_by
      image
      (Tuple3.map error ~f:(fun e -> float_of_int e *. proportion /. 16.0))
      x
      y
;;

let transform image ~n =
  let max = Image.max_val image in
  if n > max / 2
  then image
  else (
    let inc = max / (n - 1) in
    Image.mapi image ~f:(fun ~x ~y old ->
      let (res : Pixel.t) = update_pixel old inc in
      let error = get_change old res in
      adjust image (x + 1) y 7.0 error;
      adjust image (x - 1) (y + 1) 3.0 error;
      adjust image x (y + 1) 5.0 error;
      adjust image (x + 1) (y + 1) 1.0 error;
      res))
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
      and n =
        flag
          "n"
          (required Command.Param.int)
          ~doc:"number of colors per channel"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~n in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_color_dither.ppm")]
;;
