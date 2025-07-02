open! Core

let con x_original y_original image kernel =
  Image.foldi
    (Image.slice
       image
       ~x_start:(x_original - 1)
       ~x_end:(x_original + 2)
       ~y_start:(y_original - 1)
       ~y_end:(y_original + 2))
    ~init:0
    ~f:(fun ~x ~y total pixel ->
      let mult =
        match List.sub kernel ~pos:((y * 3) + x) ~len:1 with
        | [ head ] -> head
        | _ -> failwith "unexpected, list did not have one element"
      in
      total + (mult * Pixel.blue pixel))
;;

let magnitude x y image =
  let gx = con x y image [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ] in
  let gy = con x y image [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ] in
  (gx * gx) + (gy * gy) |> float_of_int |> Float.sqrt
;;

let transform image ~threshold ~blur =
  let blurred =
    match blur with 0 -> image | _ -> Blur.transform image ~radius:blur
  in
  let gray = Grayscale.transform blurred in
  let width = Image.width image in
  let height = Image.height image in
  let max = Image.max_val image in
  let float_threshold = float_of_int threshold *. 0.01 *. float_of_int max in
  let bordered =
    Image.mapi
      (Image.make
         ?max_val:(Some max)
         ~width:(width + 2)
         ~height:(height + 2)
         Pixel.zero)
      ~f:(fun ~x ~y _ ->
        if x > 0 && x < width - 1 && y > 0 && y < height
        then Image.get gray ~x:(x - 1) ~y:(y - 1)
        else Pixel.zero)
  in
  Image.mapi bordered ~f:(fun ~x ~y pixel ->
    if
      x > 0
      && x < Image.width bordered - 1
      && y > 0
      && y < Image.height bordered - 1
    then
      if Float.( >. ) (magnitude x y bordered) float_threshold
      then max, max, max
      else Pixel.zero
    else pixel)
;;

let command =
  Command.basic
    ~summary:"Detect edges in an image"
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
          ~doc:
            "N the magnitude to use as a threshold for detecting edges \
             (lower = more sensitive/more random edges)"
      and blur =
        flag
          "blur"
          (required Command.Param.int)
          ~doc:"radius of any blur for preprocessing"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~threshold ~blur in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
