open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    if Pixel.blue pixel > Pixel.red pixel + Pixel.green pixel
    then Image.get background ~x ~y
    else pixel)
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;

let%expect_test "blue screen test" =
  let reference =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  let generated =
    transform
      ~foreground:(Image.load_ppm ~filename:"../images/oz_bluescreen.ppm")
      ~background:(Image.load_ppm ~filename:"../images/meadow.ppm")
  in
  let result =
    Image.foldi generated ~init:true ~f:(fun ~x ~y state pixel ->
      state && Pixel.equal pixel (Image.get reference ~x ~y))
  in
  print_endline (Bool.to_string result);
  [%expect {|true|}]
;;
