open Lwt.Infix
open Nottui
module NW = Nottui_widgets
module W = Widgets

let clampi mn mx a : int = if a > mx then mx else if a < mn then mn else a

let spinner =
  let rec frames =
    "⠋"
    ::
    "⠙"
    ::
    "⠹"
    :: "⠸" :: "⠼" :: "⠴" :: "⠦" :: "⠧" :: "⠇" :: "⠏" :: frames
  in
  Lwd.prim
    ~release:(fun _ (running, _) -> running := false)
    ~acquire:(fun _ ->
      let running = ref true in
      let frame = Lwd.var frames in
      let rec next_frame () =
        Lwt_unix.sleep 0.080 >>= fun () ->
        let frames = Lwd.peek frame in
        Lwd.set frame (List.tl frames);
        if !running then next_frame () else Lwt.return_unit
      in
      Lwt.async next_frame;
      (running, Lwd.get frame))
  |> Lwd.get_prim
  |> Lwd.map ~f:(fun (_running, var) -> var)
  |> Lwd.join |> Lwd.map ~f:List.hd

let render_list_item highlight text =
  let attr =
    if highlight then Notty.A.(st bold ++ fg lightblue ++ st reverse)
    else Notty.A.(st bold ++ fg lightblue)
  in
  NW.string ~attr text

let log_file = Filename.temp_file "citty" ".log"

let () = at_exit (fun () -> Sys.remove log_file)

let open_in_editor refresh log_lines = function
  | `Activate ->
      let oc = open_out_bin log_file in
      Lwd_table.iter (output_string oc) log_lines;
      close_out_noerr oc;
      let safe_name = Filename.quote log_file in
      let candidates =
        match Sys.getenv_opt "VISUAL" with
        | Some x -> [ x ]
        | None -> (
            match Sys.getenv_opt "EDITOR" with
            | Some x -> [ x ]
            | None -> (
                match Sys.getenv_opt "PAGER" with Some x -> [ x ] | None -> []))
      in
      let candidates = candidates @ [ "xdg-open"; "open" ] in
      ignore
        (List.exists
           (fun bin ->
             Sys.command (Filename.quote bin ^ " " ^ safe_name) <> 127)
           candidates);
      refresh ()
  | _ -> ()

let show_text pane text =
  let open W in
  let dispatch, dispatch_var = Lwt.wait () in
  let open_editor_asap = ref false in
  let footer, set_footer =
    let display msg = Lwd.map ~f:(fun img -> NW.string (img ^ msg)) spinner in
    let var = Lwd.var (display " Receiving log") in
    let footer_content = function
      | `Opening -> display " Opening editor as soon as possible"
      | `Done -> W.empty
      | `Refresh -> Lwd.peek var
    in
    (Lwd.join (Lwd.get var), fun status -> Lwd.set var (footer_content status))
  in
  Lwt.ignore_result (Lwt.map (fun _ -> set_footer `Done) dispatch);
  let dispatch_fun action =
    match Lwt.state dispatch with
    | Return fn -> fn action
    | Fail _ -> ()
    | Sleep ->
        if (not !open_editor_asap) && action = `Activate then (
          open_editor_asap := true;
          set_footer `Opening;
          Lwt.ignore_result (Lwt.map (fun fn -> fn `Activate) dispatch))
  in
  let map' x f = Lwd.map ~f x in
  Pane.set pane (Some dispatch_fun)
    (map' footer
       (Ui.resize ~sh:1 ~crop:(Gravity.make ~h:`Negative ~v:`Positive)));
  let log_lines = Lwd_table.make () in
  Lwt.async (fun () ->
      let show_log table =
        let add str = if str <> "" then Lwd_table.append' table str in
        add text
      in
      show_log log_lines;
      let refresh () = set_footer `Refresh in
      Lwt.wakeup dispatch_var (open_in_editor refresh log_lines);
      Lwt.return ());
  let description = Lwd.pure (text |> NW.string |> Ui.resize ~w:0 ~sw:1) in
  let text_view =
    (* Setup scrolling *)
    let scroll_state = Lwd.var NW.default_scroll_state in
    let set_scroll reason st =
      let off_screen = reason = `Content && st.NW.position > st.NW.bound in
      let scroll_on_output =
        reason = `Content
        &&
        let st' = Lwd.peek scroll_state in
        st'.NW.position = st'.NW.bound
        && st.NW.position = st'.NW.position
        && st.NW.position < st.NW.bound
      in
      if scroll_on_output || off_screen then
        Lwd.set scroll_state { st with position = st.NW.bound }
      else Lwd.set scroll_state st
    in
    let text_body =
      W.dynamic_width ~w:0 ~sw:1 ~h:0 ~sh:1 (fun width ->
          Lwd.bind width ~f:(W.word_wrap_string_table log_lines)
          |> NW.vscroll_area ~state:(Lwd.get scroll_state) ~change:set_scroll
          |> (* Scroll when dragging *)
          Lwd.map
            ~f:
              (Ui.mouse_area (fun ~x:_ ~y:y0 -> function
                 | `Left ->
                     let st = Lwd.peek scroll_state in
                     `Grab
                       ( (fun ~x:_ ~y:y1 ->
                           let position = st.position + y0 - y1 in
                           let position = clampi 0 st.bound position in
                           set_scroll `Change { st with position }),
                         fun ~x:_ ~y:_ -> () )
                 | _ -> `Unhandled)))
    in
    let scroll_bar =
      Lwd.get scroll_state
      |> Lwd.map ~f:(fun x ->
             x
             |> W.vertical_scrollbar ~set_scroll:(set_scroll `Change)
             |> Ui.resize ~w:1 ~sw:0 ~h:0 ~sh:1)
    in
    Lwd_utils.pack Ui.pack_x [ text_body; scroll_bar ]
  in
  Lwd_utils.pack Ui.pack_y [ description; text_view; footer ]
  |> Pane.set pane (Some dispatch_fun)
