(* Module pour le jeu des Creets *)

[%%shared
[@@@ocaml.warning "-32-22"]
open Eliom_content.Html.F

(* Types pour le jeu *)
type creet_health = 
  | Healthy
  | Infected
  | Berserk
  | Evil
  [@@deriving json]

type position = {
  x: float;
  y: float;
} [@@deriving json]

type velocity = {
  vx: float;
  vy: float;
} [@@deriving json]

type creet = {
  id: int;
  position: position;
  velocity: velocity;
  health: creet_health;
  size: float; (* diamètre en pixels *)
  is_grabbed: bool;
  last_direction_change: float;
  infection_time: float option;
} [@@deriving json]

type game_state = {
  creets: creet list;
  game_running: bool;
  start_time: float;
  panic_level: float;
} [@@deriving json]]

(* Configuration du jeu *)
let%shared game_width = 800.0
let%shared game_height = 600.0
let%shared river_height = 50.0
let%shared hospital_height = 50.0
let%shared base_creet_size = 20.0
let%shared base_speed = 50.0 (* pixels par seconde *)

(* État du jeu côté serveur *)
let%server game_state = ref {
  creets = [];
  game_running = false;
  start_time = 0.0;
  panic_level = 1.0;
}
let%server next_id = ref 1

(* Fonctions utilitaires partagées *)
let%shared random_float min_val max_val = 
  min_val +. (Random.float (max_val -. min_val))

let%shared distance p1 p2 = 
  sqrt ((p1.x -. p2.x) ** 2.0 +. (p1.y -. p2.y) ** 2.0)

let%shared normalize_velocity v speed =
  let length = sqrt (v.vx ** 2.0 +. v.vy ** 2.0) in
  if length = 0.0 then v
  else { vx = v.vx *. speed /. length; vy = v.vy *. speed /. length }

(* Création d'un nouveau creet *)
let%server create_creet () =
  let id = !next_id in
  incr next_id;
  {
    id;
    position = { 
      x = random_float 50.0 (game_width -. 50.0);
      y = random_float (river_height +. 50.0) (game_height -. hospital_height -. 50.0);
    };
    velocity = {
      vx = random_float (-.base_speed) base_speed;
      vy = random_float (-.base_speed) base_speed;
    };
    health = Healthy;
    size = base_creet_size;
    is_grabbed = false;
    last_direction_change = Unix.time ();
    infection_time = None;
  }
(* Logique de mouvement et collision *)
let%server update_creet_position creet dt =
  if creet.is_grabbed then creet
  else
    let current_time = Unix.time () in
    let speed_modifier = match creet.health with
      | Healthy -> 1.0
      | Infected -> 0.85 (* 15% plus lent *)
      | Berserk -> 1.0
      | Evil -> 1.3 (* plus rapide pour chasser *)
    in
    
    (* Changement de direction aléatoire *)
    let velocity = 
      if current_time -. creet.last_direction_change > 2.0 && Random.float 1.0 < 0.1 then
        { vx = random_float (-.base_speed) base_speed;
          vy = random_float (-.base_speed) base_speed }
      else creet.velocity
    in
    
    let velocity = normalize_velocity velocity (base_speed *. speed_modifier *. !game_state.panic_level) in
    
    (* Nouvelle position *)
    let new_x = creet.position.x +. velocity.vx *. dt in
    let new_y = creet.position.y +. velocity.vy *. dt in
    
    (* Gestion des collisions avec les bords *)
    let (final_x, final_vx) = 
      if new_x <= creet.size /. 2.0 then (creet.size /. 2.0, abs_float velocity.vx)
      else if new_x >= game_width -. creet.size /. 2.0 then (game_width -. creet.size /. 2.0, -.abs_float velocity.vx)
      else (new_x, velocity.vx)
    in
    
    let (final_y, final_vy, new_health) = 
      if new_y <= river_height +. creet.size /. 2.0 then 
        (* Collision avec la rivière - infection possible *)
        let infected = if creet.health = Healthy && Random.float 1.0 < 0.3 then Infected else creet.health in
        (river_height +. creet.size /. 2.0, abs_float velocity.vy, infected)
      else if new_y >= game_height -. hospital_height -. creet.size /. 2.0 then 
        (game_height -. hospital_height -. creet.size /. 2.0, -.abs_float velocity.vy, creet.health)
      else (new_y, velocity.vy, creet.health)
    in
    
    { creet with
      position = { x = final_x; y = final_y };
      velocity = { vx = final_vx; vy = final_vy };
      health = new_health;
      last_direction_change = if velocity <> creet.velocity then current_time else creet.last_direction_change;
      infection_time = if new_health = Infected && creet.health = Healthy then Some current_time else creet.infection_time;
    }

(* Logique de contagion *)
let%server check_infections creets =
  List.map (fun creet ->
    match creet.health with
    | Infected ->
        let current_time = Unix.time () in
        let infection_duration = match creet.infection_time with
          | Some t -> current_time -. t
          | None -> 0.0
        in
        
        (* Évolution vers Berserk ou Evil *)
        if infection_duration > 3.0 then
          if Random.float 1.0 < 0.1 then
            { creet with health = Berserk; size = creet.size *. 1.1 }
          else if Random.float 1.0 < 0.1 then
            { creet with health = Evil; size = creet.size *. 0.85 }
          else creet
        else creet
    | Berserk ->
        (* Croissance progressive jusqu'à 4x la taille *)
        let new_size = min (base_creet_size *. 4.0) (creet.size *. 1.02) in
        { creet with size = new_size }
    | _ -> creet
  ) creets

(* Vérification des contacts entre creets *)
let%server check_creet_contacts creets =
  let rec check_contacts acc = function
    | [] -> acc
    | creet :: rest ->
        let updated_creet = 
          if creet.health = Healthy && not creet.is_grabbed then
            List.fold_left (fun c other_creet ->
              if other_creet.id <> c.id && 
                 (other_creet.health = Infected || other_creet.health = Berserk || other_creet.health = Evil) &&
                 distance c.position other_creet.position < (c.size +. other_creet.size) /. 2.0 then
                if Random.float 1.0 < 0.02 then (* 2% de chance *)
                  { c with health = Infected; infection_time = Some (Unix.time ()) }
                else c
              else c
            ) creet rest
          else creet
        in
        check_contacts (updated_creet :: acc) rest
  in
  List.rev (check_contacts [] creets)

(* Reproduction des creets *)
let%server reproduce_creets creets =
  let healthy_count = List.length (List.filter (fun c -> c.health = Healthy) creets) in
  if healthy_count > 0 && Random.float 1.0 < 0.01 then (* 1% chance par frame *)
    (create_creet ()) :: creets
  else creets

(* Mise à jour de l'état du jeu *)
let%server update_game_state dt =
  if !game_state.game_running then
    let current_time = Unix.time () in
    let new_panic_level = 1.0 +. (current_time -. !game_state.start_time) *. 0.01 in
    
    let updated_creets = !game_state.creets
      |> List.map (fun creet -> update_creet_position creet dt)
      |> check_infections
      |> check_creet_contacts
      |> reproduce_creets
    in
    
    let healthy_count = List.length (List.filter (fun c -> c.health = Healthy) updated_creets) in
    let game_over = healthy_count = 0 in
    
    game_state := {
      creets = updated_creets;
      game_running = not game_over;
      start_time = !game_state.start_time;
      panic_level = new_panic_level;
    }

(* RPC pour démarrer le jeu *)
let%rpc start_game () : unit Lwt.t =
  Random.self_init ();
  let initial_creets = List.init 5 (fun _ -> create_creet ()) in
  game_state := {
    creets = initial_creets;
    game_running = true;
    start_time = Unix.time ();
    panic_level = 1.0;
  };
  Lwt.return_unit

(* RPC pour obtenir l'état du jeu *)
let%rpc get_game_state () : game_state Lwt.t =
  Lwt.return !game_state

(* RPC pour déplacer un creet (quand l'utilisateur le saisit) *)
let%rpc move_creet (creet_id : int) (new_x : float) (new_y : float) (grabbed : bool) : unit Lwt.t =
  let updated_creets = List.map (fun creet ->
    if creet.id = creet_id then
      let final_y = 
        if not grabbed && new_y >= game_height -. hospital_height && creet.health <> Healthy then
          (* Guérison à l'hôpital *)
          new_y
        else new_y
      in
      let new_health = 
        if not grabbed && final_y >= game_height -. hospital_height && creet.health <> Healthy then
          Healthy
        else creet.health
      in
      { creet with 
        position = { x = new_x; y = final_y }; 
        is_grabbed = grabbed;
        health = new_health;
        size = if new_health = Healthy then base_creet_size else creet.size;
      }
    else creet
  ) !game_state.creets in
  game_state := { !game_state with creets = updated_creets };
  Lwt.return_unit

(* RPC pour mettre à jour l'état du jeu *)
let%rpc update_game_tick (dt : float) : unit Lwt.t =
  update_game_state dt;
  Lwt.return_unit

(* Interface utilisateur du jeu *)
let%shared creets_interface () =
  let game_canvas = canvas ~a:[
    a_id "game-canvas";
    a_class ["game-canvas"];
    a_width (int_of_float game_width);
    a_height (int_of_float game_height);
    a_style "border: 2px solid #333; background: linear-gradient(to bottom, #87CEEB 0%, #87CEEB 8%, #90EE90 8%, #90EE90 92%, #FFB6C1 92%, #FFB6C1 100%);"
  ] [] in
  
  let start_button = button ~a:[a_class ["btn"; "btn-primary"]] [txt "Démarrer le Jeu"] in
  let info_div = div ~a:[a_id "game-info"; a_class ["game-info"]] [] in
  
  ignore [%client (
    let canvas = Eliom_content.Html.To_dom.of_canvas ~%game_canvas in
    let ctx = canvas##getContext (Js_of_ocaml.Dom_html._2d_) in
    let start_btn = Eliom_content.Html.To_dom.of_button ~%start_button in
    let info_elem = Eliom_content.Html.To_dom.of_div ~%info_div in
    
    let dragging : int option ref = ref None in
    let last_update = ref 0.0 in
    
    (* Fonction pour dessiner un creet *)
    let draw_creet creet =
      let color = match creet.health with
        | Healthy -> "#00FF00"
        | Infected -> "#FF4500"  
        | Berserk -> "#8B0000"
        | Evil -> "#9932CC"
      in
      ctx##.fillStyle := Js_of_ocaml.Js.string color;
      ctx##beginPath;
      ctx##arc creet.position.x creet.position.y (creet.size /. 2.0) 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
      ctx##fill;
      
      (* Bordure pour les creets saisis *)
      if creet.is_grabbed then (
        ctx##.strokeStyle := Js_of_ocaml.Js.string "#FFFFFF";
        ctx##.lineWidth := 3.0;
        ctx##stroke
      )
    in
    
    (* Fonction pour dessiner la scène *)
    let draw_scene game_state =
      (* Effacer le canvas *)
      ctx##clearRect 0.0 0.0 ~%game_width ~%game_height;
      
      (* Dessiner la rivière toxique *)
      ctx##.fillStyle := Js_of_ocaml.Js.string "#8B4513";
      ctx##fillRect 0.0 0.0 ~%game_width ~%river_height;
      
      (* Dessiner l'hôpital *)
      ctx##.fillStyle := Js_of_ocaml.Js.string "#FFB6C1";
      ctx##fillRect 0.0 (~%game_height -. ~%hospital_height) ~%game_width ~%hospital_height;
      
      (* Dessiner les creets *)
      List.iter draw_creet game_state.creets;
      
      (* Afficher les informations *)
      let healthy_count = List.length (List.filter (fun c -> c.health = Healthy) game_state.creets) in
      let infected_count = List.length (List.filter (fun c -> c.health <> Healthy) game_state.creets) in
      let info_text = Printf.sprintf "Sains: %d | Malades: %d | Niveau de panique: %.1f" 
        healthy_count infected_count game_state.panic_level in
      info_elem##.innerHTML := Js_of_ocaml.Js.string info_text;
      
      (* Message de fin de jeu *)
      if not game_state.game_running then (
        ctx##.fillStyle := Js_of_ocaml.Js.string "#FF0000";
        ctx##.font := Js_of_ocaml.Js.string "48px Arial";
        ctx##.textAlign := Js_of_ocaml.Js.string "center";
        ctx##fillText (Js_of_ocaml.Js.string "JEU TERMINÉ") (~%game_width /. 2.0) (~%game_height /. 2.0)
      )
    in
    
    (* Fonction pour trouver le creet sous la souris *)
    let find_creet_at_position x y creets =
      List.find_opt (fun creet ->
        let dx = x -. creet.position.x in
        let dy = y -. creet.position.y in
        dx *. dx +. dy *. dy <= (creet.size /. 2.0) *. (creet.size /. 2.0)
      ) creets
    in
    
    (* Gestionnaires d'événements souris *)
    let handle_mouse_down event =
      let rect = canvas##getBoundingClientRect in
      let x = float_of_int event##.clientX -. Js_of_ocaml.Js.to_float rect##.left in
      let y = float_of_int event##.clientY -. Js_of_ocaml.Js.to_float rect##.top in
      
      Lwt.async (fun () ->
        let%lwt game_state = ~%get_game_state () in
        match find_creet_at_position x y game_state.creets with
        | Some creet ->
            dragging := Some creet.id;
            let%lwt () = ~%move_creet creet.id x y true in
            Lwt.return_unit
        | None -> Lwt.return_unit
      );
      Js_of_ocaml.Js._false
    in
    
    let handle_mouse_move event =
      match !dragging with
      | Some creet_id ->
          let rect = canvas##getBoundingClientRect in
          let x = float_of_int event##.clientX -. Js_of_ocaml.Js.to_float rect##.left in
          let y = float_of_int event##.clientY -. Js_of_ocaml.Js.to_float rect##.top in
          Lwt.async (fun () -> ~%move_creet creet_id x y true);
          Js_of_ocaml.Js._false
      | None -> Js_of_ocaml.Js._true
    in
    
    let handle_mouse_up event =
      match !dragging with
      | Some creet_id ->
          let rect = canvas##getBoundingClientRect in
          let x = float_of_int event##.clientX -. Js_of_ocaml.Js.to_float rect##.left in
          let y = float_of_int event##.clientY -. Js_of_ocaml.Js.to_float rect##.top in
          Lwt.async (fun () -> ~%move_creet creet_id x y false);
          dragging := None;
          Js_of_ocaml.Js._false
      | None -> Js_of_ocaml.Js._true
    in
    
    (* Attacher les gestionnaires d'événements *)
    canvas##.onmousedown := Js_of_ocaml.Dom_html.handler handle_mouse_down;
    canvas##.onmousemove := Js_of_ocaml.Dom_html.handler handle_mouse_move;
    canvas##.onmouseup := Js_of_ocaml.Dom_html.handler handle_mouse_up;
    
    (* Gestionnaire pour démarrer le jeu *)
    start_btn##.onclick := Js_of_ocaml.Dom_html.handler (fun _ ->
      Lwt.async (fun () -> ~%start_game ());
      Js_of_ocaml.Js._false
    );
    
    (* Boucle de jeu *)
    let rec game_loop () =
      let current_time = 0.0 in
      let dt = 0.016 in (* 60 FPS fixe *)
      last_update := current_time;
      
      Lwt.async (fun () ->
        let%lwt () = ~%update_game_tick dt in
        let%lwt game_state = ~%get_game_state () in
        draw_scene game_state;
        Lwt.return_unit
      );
      
      let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep 0.016 in (* ~60 FPS *)
      game_loop ()
    in
    
    Lwt.async game_loop
    : unit)];
  
  div ~a:[a_class ["creets-game"]]
    [ h2 [txt "Jeu des Creets"]
    ; div ~a:[a_class ["game-instructions"]]
        [ h3 [txt "Comment jouer :"]
        ; ul 
            [ li [txt "🟢 Creets verts = sains"]
            ; li [txt "🟠 Creets orange = infectés"]  
            ; li [txt "🔴 Creets rouge foncé = berserks (grossissent)"]
            ; li [txt "🟣 Creets violets = méchants (chassent les autres)"]
            ; li [txt "🏊 Rivière toxique en haut = danger !"]
            ; li [txt "🏥 Hôpital en bas = soigne les malades"]
            ; li [txt "🖱️ Cliquez et glissez pour déplacer les creets"]
            ]
        ]
    ; div ~a:[a_class ["game-controls"]]
        [ start_button
        ; p [txt "Sauvez les creets de la contamination ! Le jeu devient de plus en plus difficile..."]
        ]
    ; info_div
    ; game_canvas
    ]
