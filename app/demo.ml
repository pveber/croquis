open Gg
open Core
open Croquis

let single_letter () =
  let open Picture in
  blend [
    points ~col:Color.red ~x:[|3.|] ~y:[|4.|] () ;
    text ~size:0.5 ~x:3. ~y:4. "croquis" ~font:Croquis.Font.free_sans_bold;
  ]

let vstack () =
  let open Picture in
  vstack [
    rect ~fill:Color.green ~xmin:5. ~xmax:30. ~ymin:0. ~ymax:5. () ;
    text ~x:5. ~y:0. "croquis" ;
  ]

let scatter_plot () =
  let n = 100 in
  let x = Array.init n ~f:(fun i -> float (i - n / 2) /. 20.) in
  let y1 = Array.map x ~f:(fun x -> Float.cos x +. Random.float 0.1) in
  let y2 = Array.map x ~f:(fun x -> Float.sin x +. Random.float 0.1) in
  Plot.(
    render [
      points ~shape:`bullet ~col:Color.blue x y1 ;
      points ~shape:`circle ~col:Color.green x y2 ;
    ]
  )

let demo name f =
  name,
  Command.basic ~summary:name (Command.Param.return (fun () ->
      Layout.render_pdf (Layout.simple (f ())) "demo.pdf"
    ))
  
let command = Command.group ~summary:"Demo application for croquis" [
    demo "scatter-plot" scatter_plot ;
    demo "single-letter" single_letter ;
    demo "vstack" vstack ;
  ]

let () = Command.run command
