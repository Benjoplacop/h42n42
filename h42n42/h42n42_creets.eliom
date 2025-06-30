(* Module pour le jeu des Creets *)

(* Types partagés uniquement *)
[%%shared
[@@@ocaml.warning "-32-22"]
open Eliom_content.Html.F

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
} [@@deriving json]

(* Constantes du jeu - uniquement les valeurs, pas les calculs *)
let game_width = 800.0
let game_height = 600.0
let river_height = 50.0
let hospital_height = 50.0
let base_creet_size = 30.0
let base_speed = 50.0
]

(* État du jeu côté serveur *)
let%server game_state = ref {
  creets = [];
  game_running = false;
  start_time = 0.0;
  panic_level = 1.0;
}
let%server next_id = ref 1

(* Fonctions utilitaires côté serveur *)
let%server random_float min_val max_val = 
  min_val +. (Random.float (max_val -. min_val))

let%server distance p1 p2 = 
  sqrt ((p1.x -. p2.x) ** 2.0 +. (p1.y -. p2.y) ** 2.0)

let%server normalize_velocity v speed =
  let length = sqrt (v.vx ** 2.0 +. v.vy ** 2.0) in
  if length = 0.0 then v
  else { vx = v.vx *. speed /. length; vy = v.vy *. speed /. length }

(* Création d'un nouveau creet *)
let%server create_creet current_time =
  let id = !next_id in
  incr next_id;
  let new_creet = {
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
    last_direction_change = current_time;
    infection_time = None;
  } in 
  Printf.printf "🎯 Creet créé: ID=%d pos=(%.1f,%.1f) size=%.1f\n%!" 
    new_creet.id new_creet.position.x new_creet.position.y new_creet.size;
  new_creet
(* Logique de mouvement et collision *)
let%server update_creet_position creet dt current_time =
  if creet.is_grabbed then creet
  else
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
let%server check_infections creets current_time =
  List.map (fun creet ->
    match creet.health with
    | Infected ->
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
let%server check_creet_contacts creets current_time =
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
                  { c with health = Infected; infection_time = Some current_time }
                else c
              else c
            ) creet rest
          else creet
        in
        check_contacts (updated_creet :: acc) rest
  in
  List.rev (check_contacts [] creets)

let%server count_healthy_creets creets =
  let rec count acc = function
    | [] -> acc
    | creet :: rest -> 
        let new_acc = if creet.health = Healthy then acc + 1 else acc in
        count new_acc rest
  in
  count 0 creets

(* Reproduction des creets *)
let%server reproduce_creets creets current_time =
  let healthy_count = List.length (List.filter (fun c -> c.health = Healthy) creets) in
  if healthy_count > 0 && Random.float 1.0 < 0.01 then (* 1% chance par frame *)
    (create_creet current_time) :: creets
  else creets

(* Fonctions utilitaires pour éviter les fonctions anonymes *)
let%server update_creets_positions creets dt current_time =
  let rec update_list acc = function
    | [] -> List.rev acc
    | creet :: rest -> 
        let updated_creet = update_creet_position creet dt current_time in
        update_list (updated_creet :: acc) rest
  in
  update_list [] creets

(* Mise à jour de l'état du jeu *)
let%server update_game_state dt =
  if !game_state.game_running then
    let current_time = Unix.time () in
    let new_panic_level = 1.0 +. (current_time -. !game_state.start_time) *. 0.01 in
    
    let updated_creets = 
      let step1 = update_creets_positions !game_state.creets dt current_time in
      let step2 = check_infections step1 current_time in
      let step3 = check_creet_contacts step2 current_time in
      reproduce_creets step3 current_time
    in
    
    let healthy_count = count_healthy_creets updated_creets in
    let game_over = healthy_count = 0 in
    
    game_state := {
      creets = updated_creets;
      game_running = not game_over;
      start_time = !game_state.start_time;
      panic_level = new_panic_level;
    }

(* Fonctions utilitaires côté serveur *)
let%server update_creet_for_move creet_id new_x new_y grabbed creet =
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

let%server update_creets_for_move creets creet_id new_x new_y grabbed =
  let rec update_list acc = function
    | [] -> List.rev acc
    | creet :: rest -> 
        let updated_creet = update_creet_for_move creet_id new_x new_y grabbed creet in
        update_list (updated_creet :: acc) rest
  in
  update_list [] creets

let%server count_healthy_creets creets =
  let rec count acc = function
    | [] -> acc
    | creet :: rest -> 
        let new_acc = if creet.health = Healthy then acc + 1 else acc in
        count new_acc rest
  in
  count 0 creets

(* Services Eliom pour les actions du jeu - définis côté serveur *)
let%server start_game_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["api"; "creets"; "start"])
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    ()

let%server get_game_state_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["api"; "creets"; "state"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%server move_creet_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["api"; "creets"; "move"])
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, 
      Eliom_parameter.(int "id" ** float "x" ** float "y" ** bool "grabbed")))
    ()

let%server update_tick_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["api"; "creets"; "tick"])
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.float "dt"))
    ()

(* Références côté client *)
let%client start_game_service = ~%start_game_service
let%client get_game_state_service = ~%get_game_state_service
let%client move_creet_service = ~%move_creet_service
let%client update_tick_service = ~%update_tick_service

(* Handlers pour les services *)
let%server () =
  Eliom_registration.String.register ~service:start_game_service
    (fun () () ->
      try%lwt
        Random.self_init ();
        let current_time = Unix.time () in
        let initial_creets = List.init 8 (fun _ -> create_creet current_time) in
        game_state := {
          creets = initial_creets;
          game_running = true;
          start_time = current_time;
          panic_level = 1.0;
        };
        Lwt.return ("text/plain", "OK")
      with e ->
        let error = Printexc.to_string e in
        prerr_endline ("❌ start_game_service error: " ^ error);
        Lwt.return ("text/plain", "Error starting game")
    )


let%server () =
  Eliom_registration.String.register ~service:get_game_state_service
    (fun () () ->
      (* Format simple: count|running|panic_level puis pour chaque creet: id,x,y,health,grabbed,size *)
      let creets_data = String.concat ";" (List.map (fun c -> 
        Printf.sprintf "%d,%f,%f,%s,%b,%f"
          c.id c.position.x c.position.y 
          (match c.health with Healthy -> "healthy" | Infected -> "infected" | Berserk -> "berserk" | Evil -> "evil")
          c.is_grabbed
          c.size
      ) !game_state.creets) in
      let response = Printf.sprintf "%d|%b|%f|%s"
        (List.length !game_state.creets)
        !game_state.game_running
        !game_state.panic_level
        creets_data in
      Lwt.return ("text/plain", response)
    )

let%server spawn_creet_loop () =
  let rec loop () =
    let%lwt () = Lwt_unix.sleep 0.1 in
    let dt = 0.1 in
    update_game_state dt;
    loop ()
  in
  Lwt.async loop


let%server () =
  Eliom_registration.String.register ~service:move_creet_service
    (fun () (creet_id, (new_x, (new_y, grabbed))) ->
      let updated_creets = update_creets_for_move !game_state.creets creet_id new_x new_y grabbed in
      game_state := { !game_state with creets = updated_creets };
      Lwt.return ("text/plain", "OK")
    )

let%server () =
  Eliom_registration.String.register ~service:update_tick_service
    (fun () dt ->
      update_game_state dt;
      Lwt.return ("text/plain", "OK")
    )

(* Interface utilisateur du jeu *)
let%shared creets_interface () =
  div ~a:[a_class ["creets-game"]]
    [ h2 [txt "Jeu des Creets"]
    ; div ~a:[a_class ["game-instructions"]]
        [ h3 [txt "Comment jouer :"]
        ; ul 
            [ li [txt "🟢 Creets noirs = sains"]
            ; li [txt "🟠 Creets orange = infectés"]  
            ; li [txt "🔴 Creets rouge foncé = berserks (grossissent)"]
            ; li [txt "🟣 Creets violets = méchants (chassent les autres)"]
            ; li [txt "🏊 Rivière toxique en haut = danger !"]
            ; li [txt "🏥 Hôpital en bas = soigne les malades"]
            ; li [txt "🖱️ Cliquez et glissez pour déplacer les creets"]
            ]
        ]
    ; div ~a:[a_class ["game-controls"]]
        [ button ~a:[a_id "start-button"; a_class ["btn"; "btn-primary"]] [txt "Démarrer le Jeu"]
        ; p [txt "Sauvez les creets de la contamination ! Le jeu devient de plus en plus difficile..."]
        ]
    ; div ~a:[a_id "game-info"; a_class ["game-info"]] []
    ; canvas ~a:[
        a_id "game-canvas";
        a_class ["game-canvas"];
        a_width (int_of_float game_width);
        a_height (int_of_float game_height);
        a_style "border: 3px solid #2c3e50; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); background: linear-gradient(to bottom, #87CEEB 0%, #87CEEB 8%,rgb(255, 255, 255) 8%,rgb(255, 255, 255) 92%, #FFB6C1 92%, #FFB6C1 100%); cursor: pointer;"
      ] []
    ]

(* Logique côté client - version simplifiée *)
let%client init_game_client () =
  Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string "🎮 Initialisation du jeu côté client");
  
  (* Récupérer les éléments DOM *)
  let canvas_opt = Js_of_ocaml.Dom_html.document##getElementById (Js_of_ocaml.Js.string "game-canvas") in
  let start_btn_opt = Js_of_ocaml.Dom_html.document##getElementById (Js_of_ocaml.Js.string "start-button") in
  let info_elem_opt = Js_of_ocaml.Dom_html.document##getElementById (Js_of_ocaml.Js.string "game-info") in
  
  match (Js_of_ocaml.Js.Opt.to_option canvas_opt, 
         Js_of_ocaml.Js.Opt.to_option start_btn_opt, 
         Js_of_ocaml.Js.Opt.to_option info_elem_opt) with
  | (Some canvas_elem, Some start_btn_elem, Some info_elem) ->
      let canvas = Js_of_ocaml.Js.Unsafe.coerce canvas_elem in
      let start_btn = Js_of_ocaml.Js.Unsafe.coerce start_btn_elem in
      let ctx = canvas##getContext (Js_of_ocaml.Dom_html._2d_) in
      
      (* Test simple : dessiner un creet noir au centre *)
      let draw_test_creet () =
        let _ = ctx##.fillStyle := Js_of_ocaml.Js.string "#000000" in
        let _ = ctx##beginPath in
        let _ = ctx##arc 400.0 300.0 15.0 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false in
        let _ = ctx##fill in
        info_elem##.innerHTML := Js_of_ocaml.Js.string "✅ Test: Creet dessiné au centre"
      in
      
      (* Gestionnaire de clic sur le bouton *)
      start_btn##.onclick := Js_of_ocaml.Dom_html.handler (fun _ ->
        Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string "🔥 Bouton cliqué!");
        info_elem##.innerHTML := Js_of_ocaml.Js.string "🎮 Bouton cliqué - Test en cours...";
        draw_test_creet ();
        Js_of_ocaml.Js._false
      );
      
      Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string "✅ Jeu initialisé avec succès")
  | _ ->
      Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string "❌ Éléments DOM non trouvés")

(* Initialiser le jeu quand la page est chargée *)
let%client () = 
  Js_of_ocaml.Dom_html.window##.onload := Js_of_ocaml.Dom_html.handler (fun _ ->
    let () = Js_of_ocaml_lwt.Lwt_js_events.async (fun () -> 
      let%lwt () = Js_of_ocaml_lwt.Lwt_js.yield () in
      init_game_client ();
      Lwt.return_unit
    ) in
    Js_of_ocaml.Js._true
  )
