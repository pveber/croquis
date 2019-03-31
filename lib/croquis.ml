open Gg
open Vg
open Core_kernel

type point = float * float

module Scaling = struct
  type 'a t = 'a -> float

  let id x = x
  let linear ~domain:(from_lo, from_hi) ~range:(to_lo, to_hi) =
    let delta = to_hi -. to_lo in
    let rho = delta /. (from_hi -. from_lo) in
    fun x -> (x -. from_lo) *. rho +. to_lo
end

module Viewport = struct
  type t = {
    scale_x : float -> float ;
    scale_y : float -> float ;
  }

  let linear ~xlim ~ylim ~size:(w, h) =
    { scale_x = Scaling.linear ~domain:xlim ~range:(0., w) ;
      scale_y = Scaling.linear ~domain:ylim ~range:(0., h) }

  let make ?(scale_x = Scaling.id) ?(scale_y = Scaling.id) () =
    { scale_x ; scale_y }

  let id = {
    scale_x = ident ;
    scale_y = ident ;
  }
  let scale_x vp = vp.scale_x
  let scale_y vp = vp.scale_y

  let scale vp (x, y) =
    (vp.scale_x x, vp.scale_y y)

  let v2scale vp x y = V2.v (scale_x vp x) (scale_y vp y)
end

type thickness = [
  | `normal
  | `thick
]

let thickness_value = function
  | `normal -> 0.01
  | `thick -> 0.1

let default_font =
  match Vg_text.Font.load_from_string Linux_libertine.regular with
  | Ok f -> f
  | _ -> assert false

module Picture = struct
  class type t = object
    method render : Viewport.t -> image
    method bbox : Box2.t
  end

  let rect ?(vp = Viewport.id) ?draw ?fill ?(thickness = `normal) ~xmin ~xmax ~ymin ~ymax () =
    let xmin = Viewport.scale_x vp xmin in
    let xmax = Viewport.scale_x vp xmax in
    let ymin = Viewport.scale_y vp ymin in
    let ymax = Viewport.scale_y vp ymax in
    object
      method render vp =
        let sw = Viewport.v2scale vp xmin ymin in
        let nw = Viewport.v2scale vp xmin ymax in
        let ne = Viewport.v2scale vp xmax ymax in
        let se = Viewport.v2scale vp xmax ymin in
        let p =
          P.empty
          |> P.sub sw
          |> P.line nw
          |> P.line ne
          |> P.line se
          |> P.line sw
        in
        let outline = match draw with
          | None -> I.void
          | Some col ->
            let area = `O { P.o with P.width = thickness_value thickness ;
                                     P.cap = `Square } in
            I.cut ~area p (I.const col)
        in
        let background = match fill with
          | None -> I.void
          | Some col ->
            I.cut ~area:`Anz p (I.const col)
        in
        I.blend outline background

      method bbox =
        Box2.v (V2.v xmin ymin) (V2.v (xmax -. xmin) (ymax -. ymin))
    end

  let void =
    object
      method render _ = I.void
      method bbox = Box2.empty
    end

  let blend xs =
    object
      method render vp =
        List.fold xs ~init:I.void ~f:(fun acc p -> I.blend acc (p#render vp))

      method bbox =
        List.map xs ~f:(fun x -> x#bbox)
        |> List.fold ~init:Box2.empty ~f:Box2.union
    end

  let blend2 x y = blend [ x ; y ]

  let path ?(vp = Viewport.id) ?(col = Color.black) ?(thickness = `normal) ?(arrow_head = false) points =
    let points = List.map points ~f:(fun (x, y) -> Viewport.scale_x vp x, Viewport.scale_y vp y) in
    object
      method render vp =
        let body = match points with
          | [] | [ _ ] -> I.void
          | (ox, oy) :: (_ :: _ as t) ->
            let path =
              List.fold t ~init:(P.empty |> P.sub (Viewport.v2scale vp ox oy)) ~f:(fun acc (x, y) ->
                  acc |> P.line (Viewport.v2scale vp x y)
                )
            in
            let area = `O { P.o with P.width = thickness_value thickness } in
            I.cut ~area path (I.const col)
        and head = match arrow_head with
          | false -> I.void
          | true ->
            match List.rev points with
            | [] | [ _ ] -> I.void
            | (x1, y1) :: (x2, y2) :: _ ->
              let tip = Viewport.v2scale vp x1 y1 in
              let top = Viewport.v2scale vp x2 y2 in
              let delta_colinear = V2.(sub top tip |> unit) in
              let delta_ortho = V2.(delta_colinear |> ortho |> smul 0.3) in
              let tap = V2.(add tip delta_colinear) in
              let path =
                P.empty
                |> P.sub tip
                |> P.line V2.(add tap delta_ortho)
                |> P.line V2.(sub tap delta_ortho)
              in
              I.cut ~area:`Anz path (I.const col)
        in
        I.blend head body

      method bbox = (* FIXME: take arrow head into account *)
        List.fold points ~init:Box2.empty ~f:(fun acc (x, y) ->
            Box2.add_pt acc (V2.v x y)
          )
    end

  let text ?(vp = Viewport.id) ?(col = Color.black) ?(size = 12.) ~x ~y text =
    let x = Viewport.scale_x vp x in
    let y = Viewport.scale_y vp y in
    object
      method render vp =
        Vg_text.cut ~col:col ~size:size default_font text
        |> fst
        |> I.move (Viewport.v2scale vp x y)

      method bbox =
        Vg_text.bbox ~size:size default_font (* FIXME: allow other fonts *) text
        |> Box2.move (V2.v x y)
    end


  let translate ?(dx = 0.) ?(dy = 0.) t =
    object
      method bbox = Box2.move (V2.v dx dy) (t#bbox)
      method render vp =
        t#render vp
        |> I.move (V2.v dx dy)
    end

  module Pileup_layout = struct
    type block = {
      bbox : Box2.t ;
      contents : t ;
    }

    let block_intersects b1 b2 =
      let b1 = b1.bbox in
      let b2 = b2.bbox in
      Box2.(
        minx b1 <= minx b2 && minx b2 <= maxx b1
        || minx b2 <= minx b1 && minx b1 <= maxx b2
      )

    let block_compare b1 b2 =
      let b1 = b1.bbox in
      let b2 = b2.bbox in
      Caml.compare Box2.(minx b1, maxx b1) Box2.(minx b2, maxx b2)

    let make_block contents = {
      bbox = contents#bbox ;
      contents ;
    }

    let x_overlap_partition = function
      | [] -> [], []
      | h :: t ->
        let rec loop inside outside last = function
          | [] -> List.rev (last :: inside), List.rev outside
          | h :: t ->
            if block_intersects last h then
              loop inside (h :: outside) last t
            else
              loop (last :: inside) outside h t
        in
        loop [] [] h t

    let make items =
      let rec loop acc base_y = function
        | [] -> List.rev acc
        | items ->
          let layer, rest = x_overlap_partition items in
          let layer_height =
            List.map layer ~f:(fun bl -> Box2.h bl.bbox)
            |> List.reduce_exn ~f:Float.max
          in
          let translated_layer =
            List.map layer ~f:(fun bl ->
                translate ~dy:(base_y -. Box2.miny bl.bbox) bl.contents
              )
          in
          loop (translated_layer :: acc) (base_y +. layer_height) rest
      in
      let sorted_blocks =
        List.map items ~f:make_block
        |> List.sort ~compare:(fun x y -> block_compare x y)
      in
      let layers =
        match sorted_blocks with
        | [] -> []
        | h :: _ -> loop [] (Box2.maxy h.bbox) sorted_blocks
      in
      List.concat layers
  end

  let pileup xs = blend (Pileup_layout.make xs)

  module VStack_layout = struct
    let make items =
      let rec loop acc base_y = function
        | [] -> List.rev acc
        | h :: t ->
          let height = Box2.h h.Pileup_layout.bbox in
          let translated_image =
            translate ~dy:(base_y -. Box2.maxy h.bbox) h.contents
          in
          loop (translated_image :: acc) (base_y -. height) t
      in
      match List.map items ~f:Pileup_layout.make_block with
      | [] -> []
      | h :: _ as xs ->
        loop [] (Box2.maxy h.bbox) xs
  end

  let vstack ?(align = `none) xs =
    match align with
    | `none -> blend (VStack_layout.make xs)
    | _ -> assert false (* not implemented *)
end

module Layout = struct
  type t =
    | Simple of Picture.t

  let simple x = Simple x

  let rec size ?width ?height view =
    let res w h = w *. 10., h *.10. in
    match width, height with
    | Some w, Some h -> res w h
    | Some w, None ->
      let h = Box2.h view *. w /. Box2.w view in
      res w h
    | None, Some h ->
      let w = Box2.w view *. h /. Box2.h view in
      res w h
    | None, None -> size ~width:10. view

  let box2_padding alpha b =
    let w = Box2.w b in
    let h = Box2.h b in
    let delta = Float.(min w h * alpha) in
    Box2.v_mid
      (Box2.mid b)
      (V2.v (w +. delta) (h +. delta))


  let render_pdf ?width ?height (Simple pic) fn =
    let view = box2_padding 0.01 pic#bbox in
    let size = size ?width ?height view in
    let image = pic#render Viewport.id in
    let font = match Vg_text.Font.load_from_string Linux_libertine.regular with
      | Ok x -> x
      | _ -> assert false
    in
    match Vgr_pdf.otf_font (Vg_text.Font.data font) with
    | Ok otf_font ->
      let font f =
        if Vg_text.Font.name font = f.Font.name then
          otf_font
        else
          Vgr_pdf.font f
      in
      Out_channel.with_file fn ~f:(fun oc ->
          let r = Vgr.create (Vgr_pdf.target ~font ()) (`Channel oc) in
          ignore (Vgr.render r (`Image (V2.of_tuple size, view, image))) ;
          ignore (Vgr.render r `End)
        )
    | _ -> assert false
end
