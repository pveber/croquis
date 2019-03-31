open Core
open Croquis

let single_letter () =
  Picture.text ~x:5. ~y:0. "croquis"

let demo name f =
  name,
  Command.basic ~summary:name (Command.Param.return (fun () ->
      Layout.render_pdf (Layout.simple (f ())) "demo.pdf"
    ))
  
let command = Command.group ~summary:"Demo application for croquis" [
    demo "single-letter" single_letter ;
  ]

let () = Command.run command
