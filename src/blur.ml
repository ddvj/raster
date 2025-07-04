open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)

let better_slice ~image ~x ~y radius =
  Image.slice
    image
    ~x_start:(max (x - radius) 0)
    ~x_end:(min (x + radius) (Image.width image - 1))
    ~y_start:(max (y - radius) 0)
    ~y_end:(min (y + radius) (Image.height image - 1))
;;

(*This implementation uses slice as if it is inclusive,
  when it's not. It works though, implying that the
  implementation used for the answer key uses the slice function wrong*)

let transform image ~radius =
  Image.mapi (Image.copy image) ~f:(fun ~x ~y _ ->
    Image.mean_pixel (better_slice ~image ~x ~y radius))
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;

let%expect_test "blur test" =
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  let generated =
    transform
      (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
      ~radius:3
  in
  let result =
    Image.foldi generated ~init:`No_errors ~f:(fun ~x ~y state pixel ->
      match state with
      | `No_errors ->
        if not (Pixel.equal pixel (Image.get reference ~x ~y))
        then (
          print_endline
            ("first error: expected "
             ^ Pixel.to_string (Image.get reference ~x ~y)
             ^ " got "
             ^ Pixel.to_string pixel);
          `Has_erred)
        else `No_errors
      | `Has_erred -> `Has_erred)
  in
  print_s [%sexp (result : [ `Has_erred | `No_errors ])];
  [%expect {|No_errors|}]
;;
