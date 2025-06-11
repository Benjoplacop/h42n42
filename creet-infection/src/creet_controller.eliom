module CreetController = struct
  open Eliom_content.Html.D
  open Lwt.Infix
  open CreetTypes
  open CreetModel

  let creets = ref []

  let update_creets () =
    List.iter (fun creet ->
      match creet.state with
      | Healthy -> ()
      | Sick -> 
          if Random.float 1.0 < 0.02 then
            creet.state <- Sick
      | Berserk -> 
          creet.size <- creet.size * 4;
          creet.color <- "red"  (* Special color for berserk *)
      | Mean -> 
          creet.size <- creet.size - 15;
          creet.color <- "darkred"  (* Color for mean *)
    ) !creets

  let handle_mouse_drag creet =
    let rec drag_loop () =
      Lwt_js_events.mousemove (fun _ ->
        (* Update position of the dragged Creet *)
        Lwt.return ()
      ) >>= fun () ->
      Lwt_js_events.mouseup (fun _ ->
        (* Stop dragging and make Creet vulnerable again *)
        Lwt.return ()
      )
    in
    Lwt.async drag_loop

  let game_over () =
    if List.for_all (fun c -> c.state <> Healthy) !creets then
      (* Display GAME OVER message *)
      ()

  let start_game () =
    Lwt.async (fun () ->
      while true do
        update_creets ();
        game_over ();
        Lwt_unix.sleep 1.0  (* Game loop interval *)
      done
    )

  let main () =
    start_game ();
    (* Additional initialization code *)

end