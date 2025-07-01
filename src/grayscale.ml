open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun pixel ->
    let average =
      (Pixel.red pixel + Pixel.green pixel + Pixel.blue pixel) / 3
    in
    average, average, average)
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
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

let%expect_test "grayscale test" =
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  let generated =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let result =
    Image.foldi generated ~init:true ~f:(fun ~x ~y state pixel ->
      state && Pixel.equal pixel (Image.get reference ~x ~y))
  in
  print_endline (Bool.to_string result);
  [%expect {|true|}]
;;
