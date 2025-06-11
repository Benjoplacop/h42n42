module CreetView = struct
  open Eliom_content.Html.D

  let render_creet (creet : CreetTypes.creet) =
    let position = CreetTypes.get_position creet in
    let size = CreetTypes.get_size creet in
    let color = CreetTypes.get_color creet in
    let state = CreetTypes.get_state creet in
    let special_state = CreetTypes.get_special_state creet in
    let style = Printf.sprintf "left: %fpx; top: %fpx; width: %fpx; height: %fpx; background-color: %s;" 
      (fst position) (snd position) size size color in
    div ~a:[a_style style; a_class (match state with
      | CreetTypes.Healthy -> "creet healthy"
      | CreetTypes.Sick -> "creet sick"
      | CreetTypes.Berserk -> "creet berserk"
      | CreetTypes.Mean -> "creet mean")] 
      [pcdata (Printf.sprintf "State: %s" (CreetTypes.string_of_state state))]

  let render_game (creets : CreetTypes.creet list) =
    div ~a:[a_id "game-area"] 
      (List.map render_creet creets)

  let main_page () =
    let creets = CreetModel.get_all_creets () in
    html (head (title (pcdata "Creet Infection")) [])
      (body [
        h1 [pcdata "Creet Infection"];
        div ~a:[a_id "creet-container"] (render_game creets);
        script ~a:[a_src "/js/creet_infection_js.js"] []
      ])
end