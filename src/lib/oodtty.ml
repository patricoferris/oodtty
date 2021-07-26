open Nottui
open Lwt.Infix
open Ood
module Nw = Nottui_widgets
module W = Widgets

let header =
  Lwd.var
  @@ Lwd.pure
       (Nw.string
          ~attr:Notty.A.(fg (rgb_888 ~r:234 ~g:89 ~b:12) ++ st italic)
          "ocaml.org")

let body = Lwd.var W.empty

let footer =
  Lwd.var
  @@ Lwd.pure (Nw.string ~attr:Notty.A.(fg white ++ st italic) Build.time)

let ui =
  let place_ui_var v = Lwd.(v |> get |> join |> map ~f:(Ui.resize ~w:0)) in
  Lwd_utils.pack Ui.pack_y
    [ place_ui_var header; Lwd.get body |> Lwd.join; place_ui_var footer ]

let render_list_item text highlight =
  let attr =
    if highlight then Notty.A.(st bold ++ fg lightblue ++ st reverse)
    else Notty.A.(st bold ++ fg lightblue)
  in
  Nw.string ~attr text

let show_text pane f t =
  let pane = W.Pane.open_subview pane in
  W.Pane.set pane (Some (fun _ -> Scroll.show_text pane @@ f t)) Nw.empty_lwd;
  Lwt.async (fun () -> Lwt.return ())

let show_simple ~meta ~footer pane text =
  let open W in
  let pane = Pane.open_subview pane in
  let text =
    String.split_on_char ' ' text
    |> List.map (fun t -> Lwd.pure @@ Nw.fmt "%s " t)
  in
  Pane.set pane None
    (Nw.vbox (meta @ [ Nw.flex_box ~w:(Lwd.pure 80) text ] @ footer));
  Lwt.async (fun () -> Lwt.return ())

let title text = Lwd.pure @@ Nw.string ~attr:Notty.A.(st bold) text

let comma_list ~attr items =
  Lwd.pure @@ Nw.fmt ~attr "%a" Fmt.(list ~sep:Fmt.comma string) items

let link txt =
  Lwd.pure
  @@ Nw.fmt ~attr:Notty.A.(st underline ++ fg white ++ st italic) "%s" txt

let ocamlorg pane =
  [
    ( "Language",
      [
        ( "Tutorials",
          W.list_box ~items:Tutorial.all
            ~render:(fun t b -> render_list_item t.title b)
            ~select:(fun t -> show_text pane (fun t -> t.Tutorial.body_md) t) );
        ( "Papers",
          W.list_box ~items:Paper.all
            ~render:(fun t b -> render_list_item t.title b)
            ~select:(fun t ->
              show_simple
                ~meta:
                  [ title t.title; comma_list ~attr:Notty.A.(fg red) t.authors ]
                ~footer:[ link (List.hd t.links) ]
                pane t.abstract) );
        ( "Books",
          W.list_box ~items:Book.all
            ~render:(fun t b -> render_list_item t.title b)
            ~select:(fun t ->
              show_simple
                ~meta:
                  [ title t.title; comma_list ~attr:Notty.A.(fg red) t.authors ]
                ~footer:[] pane t.Book.description) );
      ] );
    ( "Principles",
      [
        ( "Successes",
          W.list_box ~items:Success_story.all
            ~render:(fun t b -> render_list_item t.title b)
            ~select:(fun _ -> ()) );
        ( "Institutions",
          W.list_box ~items:Academic_institution.all
            ~render:(fun t b -> render_list_item t.name b)
            ~select:(fun _ -> ()) );
      ] );
  ]

let select_item pane sub t =
  let open W in
  let pane = Pane.open_subview pane in
  let ui, dispatch = List.assoc t @@ List.assoc sub (ocamlorg pane) in
  Pane.set pane (Some dispatch) ui;
  Lwt.async (fun () -> Lwt.return ())

let select_sub pane sub =
  let open W in
  let pane = Pane.open_subview pane in
  let items = List.assoc sub @@ ocamlorg pane in
  let ui, dispatch =
    W.list_box ~items:(List.map fst items) ~render:render_list_item
      ~select:(select_item pane sub)
  in
  Pane.set pane (Some dispatch) ui;
  Lwt.async (fun () -> Lwt.return ())

let add_views pane =
  let open W in
  let ui, dispatch =
    W.list_box
      ~items:(List.map fst (ocamlorg pane))
      ~render:render_list_item ~select:(select_sub pane)
  in
  Pane.set pane (Some dispatch) ui;
  Lwt.return_ok ()

let main () =
  let open W in
  let pane = Pane.make () in
  let dispatch pos action =
    match Pane.current_view pane pos with
    | None -> `Unhandled
    | Some view -> (
        match Pane.get view with
        | None -> `Unhandled
        | Some dispatch ->
            dispatch action;
            `Handled)
  in
  let focus_handle = Focus.make () in
  Focus.request focus_handle;
  Lwd.set body
    (Pane.render pane
    |> Lwd.map2
         ~f:(fun focus ->
           Ui.keyboard_area ~focus @@ function
           | (`Arrow `Up | `ASCII 'k'), [] -> dispatch `Middle `Select_prev
           | (`Arrow `Down | `ASCII 'j'), [] -> dispatch `Middle `Select_next
           | (`Arrow `Left | `ASCII 'h'), [] -> dispatch `Left `Activate
           | (`Arrow `Right | `Enter | `ASCII 'l'), [] ->
               dispatch `Right `Activate
           | (`Escape | `ASCII 'q'), [] -> exit 0
           | _ -> `Unhandled)
         (Focus.status focus_handle));
  Lwt_main.run
  @@ (add_views (Pane.open_root pane) >>= fun _ -> Nottui_lwt.run ui)
