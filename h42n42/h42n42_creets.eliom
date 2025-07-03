(* Types partagés uniquement *)
[%%shared
[@@@ocaml.warning "-32-22"] 

module Html = Eliom_content.Html.F

(* Constantes exportées *)
let river_height = 50.0
let hospital_height = 50.0

let game_width = ref 950.0
let game_height = ref 700.0
let base_speed = ref 100.0
let base_creet_size = ref 40.0

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
  transformation_checked: bool; (* Pour s'assurer qu'on ne vérifie la transformation qu'une fois *)
} [@@deriving json]

type game_state = {
  creets: creet list;
  game_running: bool;
  start_time: float;
  panic_level: float;
} [@@deriving json]

(* Constantes du jeu - uniquement les valeurs, pas les calculs *)
let game_width_default = 950.0
let game_height_default = 700.0
let river_height_default = 50.0
let hospital_height_default = 50.0
let base_creet_size_default = 40.0
let base_speed_default = 100.0
]

(* État du jeu côté client *)
let%client game_state = ref {
  creets = [];
  game_running = false;
  start_time = 0.0;
  panic_level = 1.0;
}
let%client next_id = ref 1

(* Fonctions utilitaires côté client *)
let%client random_float min_val max_val = 
  min_val +. (Random.float (max_val -. min_val))

(* Interface utilisateur du jeu - Création du formulaire avec TyXML *)
let%shared creets_interface () =
  Html.div ~a:[Html.a_class ["creets-game"]] [
    Html.h2 [Html.txt "Jeu des Creets"];

    Html.div ~a:[Html.a_class ["game-controls"]] [
      Html.label [Html.txt "Vitesse des Creets :"];
      Html.input ~a:[
        Html.a_class ["form-control"];
        Html.a_input_type `Range;
        Html.a_input_min (`Number 0);
        Html.a_input_max (`Number 200);
        Html.a_value (string_of_float !base_speed); 
        Html.a_id "creet-speed"
      ] ();

      Html.label [Html.txt "Taille des Creets :"];
      Html.input ~a:[
        Html.a_class ["form-control"];
        Html.a_input_type `Range;
        Html.a_input_min (`Number 10);
        Html.a_input_max (`Number 100);
        Html.a_value (string_of_float !base_creet_size);
        Html.a_id "creet-size"
      ] ();

      Html.label [Html.txt "Longueur de la Carte :"];
      Html.input ~a:[
        Html.a_class ["form-control"];
        Html.a_input_type `Range;
        Html.a_input_min (`Number 500);
        Html.a_input_max (`Number 1300);
        Html.a_value (string_of_float !game_width);
        Html.a_id "map-size"
      ] ();

      Html.label [Html.txt "Hauteur de la Carte :"];
      Html.input ~a:[
        Html.a_class ["form-control"];
        Html.a_input_type `Range;
        Html.a_input_min (`Number 400);
        Html.a_input_max (`Number 1000);
        Html.a_value (string_of_float !game_height);
        Html.a_id "map-height"
      ] ();

      (* Bouton pour appliquer les paramètres *)
      Html.button ~a:[Html.a_id "apply-settings"; Html.a_class ["btn"; "btn-primary"]] [Html.txt "Appliquer"]
    ];

    Html.div ~a:[Html.a_class ["game-controls"]] [
      Html.button ~a:[Html.a_id "start-button"; Html.a_class ["btn"; "btn-primary"]] [Html.txt "Démarrer le Jeu"]
    ];

    Html.div ~a:[Html.a_id "game-info"; Html.a_class ["game-info"]] [];

    (* Zone de jeu avec DOM elements *)
    Html.div ~a:[Html.a_class ["game-area"]; Html.a_id "game-area"; 
                 Html.a_style ("position: relative; width: " ^ (string_of_float !game_width) ^ "px; height: " ^ (string_of_float !game_height) ^ "px; border: 2px solid #333; margin: 10px auto; overflow: hidden;")] [
      (* Rivière toxique *)
      Html.div ~a:[
        Html.a_class ["river-zone"]; 
        Html.a_id "river-zone";
        Html.a_style ("position: absolute; top: 0px; left: 0px; width: 100%; height: " ^ (string_of_float river_height) ^ "px; background: linear-gradient(to bottom, #2196F3, #1976D2); color: white; text-align: center; line-height: " ^ (string_of_float river_height) ^ "px; font-weight: bold; z-index: 1;")
      ] [Html.txt "🌊 RIVIÈRE TOXIQUE - DANGER !!!"];
      
      (* Zone de jeu principale *)
      Html.div ~a:[
        Html.a_class ["play-zone"]; 
        Html.a_id "play-zone";
        Html.a_style ("position: absolute; top: " ^ (string_of_float river_height) ^ "px; left: 0px; width: 100%; height: " ^ (string_of_float (!game_height -. river_height -. hospital_height)) ^ "px; background: linear-gradient(to bottom, #C8E6C9, #4CAF50); z-index: 2;")
      ] [];
      
      (* Hôpital *)
      Html.div ~a:[
        Html.a_class ["hospital-zone"]; 
        Html.a_id "hospital-zone";
        Html.a_style ("position: absolute; bottom: 0px; left: 0px; width: 100%; height: " ^ (string_of_float hospital_height) ^ "px; background: linear-gradient(to bottom, #FFB6C1, #FF69B4); color: #2E7D32; text-align: center; line-height: " ^ (string_of_float hospital_height) ^ "px; font-weight: bold; z-index: 1;")
      ] [Html.txt "🏥 HÔPITAL - Déposez les creets malades ici !!!"];
    ];
  ]

(* Types pour les Creets avec éléments DOM *)
type%client creet_dom = {
  creet: creet;
  element: Dom_html.divElement Js.t;
  thread_cancel: unit Lwt.u option;
}

(* État du jeu côté client *)
let%client game_state = ref {
  creets = [];
  game_running = false;
  start_time = 0.0;
  panic_level = 1.0;
}
let%client next_id = ref 1
let%client creet_doms = ref []
let%client game_container = ref None

(* Variables pour le drag & drop *)
let%client dragging_creet = ref None
let%client mouse_offset = ref { x = 0.0; y = 0.0 }

(* Fonctions utilitaires côté client *)
let%client random_float min_val max_val = 
  min_val +. (Random.float (max_val -. min_val))

(* Fonction pour créer l'élément DOM d'un Creet *)
let%client create_creet_element creet =
  let doc = Dom_html.document in
  let div = Dom_html.createDiv doc in
  
  (* Style de base du Creet *)
  let size_px = Printf.sprintf "%.0fpx" creet.size in
  let x_pos = Printf.sprintf "%.0fpx" creet.position.x in
  let y_pos = Printf.sprintf "%.0fpx" creet.position.y in
  
  let color = match creet.health with
    | Healthy -> "#4CAF50"    (* Vert *)
    | Infected -> "#FF9800"   (* Orange *)
    | Berserk -> "#F44336"    (* Rouge *)
    | Evil -> "#8b00ff"       (* Violet *)
  in
  
  div##.className := Js.string "creet";
  div##.style##.position := Js.string "absolute";
  div##.style##.width := Js.string size_px;
  div##.style##.height := Js.string size_px;
  div##.style##.backgroundColor := Js.string color;
  div##.style##.borderRadius := Js.string "50%";
  div##.style##.border := Js.string "2px solid #333";
  div##.style##.left := Js.string x_pos;
  div##.style##.top := Js.string y_pos;
  div##.style##.cursor := Js.string "pointer";
  div##.style##.transition := Js.string "all 0.1s ease";
  div##.style##.zIndex := Js.string "10";
  
  (* Ajouter des yeux et une bouche avec pseudo-éléments via du contenu *)
  div##.innerHTML := Js.string "👁️👁️";
  div##.style##.display := Js.string "flex";
  div##.style##.alignItems := Js.string "center";
  div##.style##.justifyContent := Js.string "center";
  div##.style##.fontSize := Js.string (Printf.sprintf "%.0fpx" (creet.size /. 4.0));
  
  div

(* Fonction pour mettre à jour la position d'un élément DOM *)
let%client update_creet_element_position element creet =
  let x_pos = Printf.sprintf "%.0fpx" creet.position.x in
  let y_pos = Printf.sprintf "%.0fpx" creet.position.y in
  let size_px = Printf.sprintf "%.0fpx" creet.size in
  
  element##.style##.left := Js.string x_pos;
  element##.style##.top := Js.string y_pos;
  element##.style##.width := Js.string size_px;
  element##.style##.height := Js.string size_px;
  
  (* Mettre à jour la couleur selon la santé *)
  let color = match creet.health with
    | Healthy -> "#4CAF50"    
    | Infected -> "#FF9800"   
    | Berserk -> "#F44336"    
    | Evil -> "#8b00ff"       
  in
  element##.style##.backgroundColor := Js.string color;
  
  (* Mettre à jour l'expression selon la santé *)
  let expression = match creet.health with
    | Healthy -> "😊"
    | Infected -> "😷"
    | Berserk -> "😡"
    | Evil -> "👹"
  in
  element##.innerHTML := Js.string expression;
  element##.style##.fontSize := Js.string (Printf.sprintf "%.0fpx" (creet.size /. 2.0))

(* Fonction pour appliquer les paramètres du formulaire *)
let%client apply_game_settings () =
  let speed_input = Dom_html.document##getElementById (Js.string "creet-speed") in
  let size_input = Dom_html.document##getElementById (Js.string "creet-size") in
  let map_width_input = Dom_html.document##getElementById (Js.string "map-size") in
  let map_height_input = Dom_html.document##getElementById (Js.string "map-height") in

  match (Js.Opt.to_option speed_input, 
         Js.Opt.to_option size_input, 
         Js.Opt.to_option map_width_input,
         Js.Opt.to_option map_height_input) with
  | (Some speed_elem, Some size_elem, Some map_width_elem, Some map_height_elem) ->
      (match (Js.Opt.to_option (Dom_html.CoerceTo.input speed_elem),
              Js.Opt.to_option (Dom_html.CoerceTo.input size_elem),
              Js.Opt.to_option (Dom_html.CoerceTo.input map_width_elem),
              Js.Opt.to_option (Dom_html.CoerceTo.input map_height_elem)) with
      | (Some speed_input_elem, Some size_input_elem, Some map_width_input_elem, Some map_height_input_elem) ->
          let speed = float_of_string (Js.to_string speed_input_elem##.value) in
          let size = float_of_string (Js.to_string size_input_elem##.value) in
          let map_width = float_of_string (Js.to_string map_width_input_elem##.value) in
          let map_height = float_of_string (Js.to_string map_height_input_elem##.value) in

          (* Mise à jour des variables globales *)
          game_width := map_width;
          game_height := map_height;
          base_speed := speed;
          base_creet_size := size;

          (* Mettre à jour les zones de jeu *)
          let game_area_opt = Dom_html.document##getElementById (Js.string "game-area") in
          (match Js.Opt.to_option game_area_opt with
          | Some game_area_elem -> 
              game_area_elem##.style##.width := Js.string (Printf.sprintf "%.0fpx" map_width);
              game_area_elem##.style##.height := Js.string (Printf.sprintf "%.0fpx" map_height);
          | None -> ());

      | _ -> ())
  | _ -> ()

(* Création d'un nouveau creet côté client *)
let%client create_creet current_time =
  let id = !next_id in
  incr next_id;
  let new_creet = {
    id;
    position = { 
      x = random_float 50.0 (!game_width -. 50.0);
      y = random_float (river_height +. 50.0) (!game_height -. hospital_height -. 50.0);
    };
    velocity = {
      vx = random_float (-.(!base_speed)) (!base_speed);
      vy = random_float (-.(!base_speed)) (!base_speed);
    };
    health = Healthy;
    size = !base_creet_size;
    is_grabbed = false;
    last_direction_change = current_time;
    infection_time = None;
    transformation_checked = false;
  } in
  new_creet
(* Fonctions utilitaires côté client *)
let%client distance p1 p2 = 
  sqrt ((p1.x -. p2.x) ** 2.0 +. (p1.y -. p2.y) ** 2.0)

let%client normalize_velocity v speed =
  let length = sqrt (v.vx ** 2.0 +. v.vy ** 2.0) in
  if length = 0.0 then v
  else { vx = v.vx *. speed /. length; vy = v.vy *. speed /. length }

let%client is_in_hospital pos =
  pos.y >= (!game_height -. hospital_height)

let%client is_in_river pos =
  pos.y <= river_height

let%client heal_creet creet =
  if creet.health <> Healthy then
    { creet with 
      health = Healthy; 
      size = !base_creet_size;
      infection_time = None;
      transformation_checked = false;
    }
  else creet

(* Fonctions pour le drag & drop *)
let%client get_mouse_pos canvas event =
  let rect = canvas##getBoundingClientRect () in
  let x = (Js.to_float event##.clientX) -. (Js.to_float rect##.left) in
  let y = (Js.to_float event##.clientY) -. (Js.to_float rect##.top) in
  { x; y }

let%client find_creet_at_position pos creets =
  List.find_opt (fun creet ->
    let dist = distance pos creet.position in
    dist <= creet.size /. 2.0
  ) creets

(* Logique de mouvement et collision *)
let%client update_creet_position creet dt current_time =
  if creet.is_grabbed then creet
  else
    let speed_modifier = match creet.health with
      | Healthy -> 1.0
      | Infected -> 0.85
      | Berserk -> 1.0
      | Evil -> 1.3
    in
    
    (* Changement de direction aléatoire *)
    let velocity = 
      if current_time -. creet.last_direction_change > 2.0 && Random.float 1.0 < 0.1 then
        { vx = random_float (-.(!base_speed)) (!base_speed);
          vy = random_float (-.(!base_speed)) (!base_speed) }
      else creet.velocity
    in
    
    let velocity = normalize_velocity velocity (!base_speed *. speed_modifier *. !game_state.panic_level) in
    
    (* Nouvelle position *)
    let new_x = creet.position.x +. velocity.vx *. dt in
    let new_y = creet.position.y +. velocity.vy *. dt in
    
    (* Gestion des collisions avec les bords *)
    let (final_x, final_vx) = 
      if new_x <= creet.size /. 2.0 then (creet.size /. 2.0, abs_float velocity.vx)
      else if new_x >= !game_width -. creet.size /. 2.0 then (!game_width -. creet.size /. 2.0, -.abs_float velocity.vx)
      else (new_x, velocity.vx)
    in
    
    let (final_y, final_vy, new_health) = 
      if new_y <= river_height +. creet.size /. 2.0 then 
        let infected = if creet.health = Healthy then Infected else creet.health in
        (river_height +. creet.size /. 2.0, abs_float velocity.vy, infected)
      else if new_y >= !game_height -. hospital_height -. creet.size /. 2.0 then 
        (!game_height -. hospital_height -. creet.size /. 2.0, -.abs_float velocity.vy, creet.health)
      else (new_y, velocity.vy, creet.health)
    in
    
    { creet with
      position = { x = final_x; y = final_y };
      velocity = { vx = final_vx; vy = final_vy };
      health = new_health;
      last_direction_change = if velocity <> creet.velocity then current_time else creet.last_direction_change;
      infection_time = if new_health = Infected && creet.health = Healthy then Some current_time else creet.infection_time;
      transformation_checked = if new_health = Infected && creet.health = Healthy then false else creet.transformation_checked;
    }

(* Logique de contagion *)
let%client check_infections creet current_time =
  match creet.health with
  | Infected ->
      let infection_duration = match creet.infection_time with
        | Some t -> current_time -. t
        | None -> 0.0
      in
      
      if infection_duration > 3.0 && not creet.transformation_checked then
        let random_val = Random.float 1.0 in
        if random_val < 0.1 then
          { creet with health = Berserk; size = creet.size *. 1.1; transformation_checked = true }
        else if random_val < 0.2 then
          { creet with health = Evil; size = creet.size *. 0.85; transformation_checked = true }
        else
          { creet with transformation_checked = true }
      else creet
  | Berserk ->
      let new_size = min (!base_creet_size *. 4.0) (creet.size *. 1.02) in
      { creet with size = new_size }
  | _ -> creet

(* Thread Lwt pour contrôler un Creet individuel *)
let%client creet_controller_thread creet_dom =
  let rec loop creet =
    let%lwt () = Lwt_js.sleep 0.016 in (* ~60 FPS *)
    if !game_state.game_running then (
      let current_time = Unix.time () in
      let dt = 0.016 in
      
      (* Mise à jour du creet *)
      let updated_creet = update_creet_position creet dt current_time in
      let updated_creet = check_infections updated_creet current_time in
      
      (* Mise à jour de l'élément DOM *)
      update_creet_element_position creet_dom.element updated_creet;
      
      (* Mise à jour dans le game_state *)
      game_state := { !game_state with 
        creets = List.map (fun c -> if c.id = updated_creet.id then updated_creet else c) !game_state.creets 
      };
      
      loop updated_creet
    ) else 
      Lwt.return_unit
  in
  loop creet_dom.creet

(* Gestionnaire d'événements souris avec Lwt_js_events *)
let%client setup_creet_mouse_events creet_dom =
  let element = creet_dom.element in
  
  (* Variables pour le drag & drop du creet *)
  let dragging = ref false in
  let drag_offset = ref { x = 0.0; y = 0.0 } in
  
  (* Thread pour gérer mousedown *)
  Lwt.async (fun () ->
    Lwt_js_events.mousedowns element (fun event _ ->
      let rect = element##getBoundingClientRect () in
      let mouse_x = (Js.to_float event##.clientX) -. (Js.to_float rect##.left) in
      let mouse_y = (Js.to_float event##.clientY) -. (Js.to_float rect##.top) in
      
      (* Calculer l'offset de la souris par rapport au centre du creet *)
      let creet_center_x = creet_dom.creet.size /. 2.0 in
      let creet_center_y = creet_dom.creet.size /. 2.0 in
      drag_offset := { x = mouse_x -. creet_center_x; y = mouse_y -. creet_center_y };
      
      dragging := true;
      element##.style##.zIndex := Js.string "20";
      
      (* Marquer le creet comme saisi *)
      let grabbed_creet = { creet_dom.creet with is_grabbed = true } in
      game_state := { !game_state with 
        creets = List.map (fun c -> if c.id = grabbed_creet.id then grabbed_creet else c) !game_state.creets 
      };
      
      Lwt.return_unit
    )
  );
  
  (* Thread pour gérer mousemove *)
  Lwt.async (fun () ->
    Lwt_js_events.mousemoves Dom_html.document (fun event _ ->
      if !dragging then (
        let rect = element##getBoundingClientRect () in
        let mouse_x = (Js.to_float event##.clientX) -. (Js.to_float rect##.left) in
        let mouse_y = (Js.to_float event##.clientY) -. (Js.to_float rect##.top) in
        
        (* Calculer la nouvelle position du creet *)
        let new_x = mouse_x -. !drag_offset.x in
        let new_y = mouse_y -. !drag_offset.y in
        
        (* Contraindre dans les limites du jeu *)
        let constrained_x = max (creet_dom.creet.size /. 2.0) (min (!game_width -. creet_dom.creet.size /. 2.0) new_x) in
        let constrained_y = max (river_height +. creet_dom.creet.size /. 2.0) (min (!game_height -. hospital_height -. creet_dom.creet.size /. 2.0) new_y) in
        
        (* Mettre à jour la position du DOM *)
        element##.style##.left := Js.string (Printf.sprintf "%.0fpx" (constrained_x -. creet_dom.creet.size /. 2.0));
        element##.style##.top := Js.string (Printf.sprintf "%.0fpx" (constrained_y -. creet_dom.creet.size /. 2.0));
        
        (* Mettre à jour le creet dans le game_state *)
        let moved_creet = { creet_dom.creet with position = { x = constrained_x; y = constrained_y } } in
        game_state := { !game_state with 
          creets = List.map (fun c -> if c.id = moved_creet.id then moved_creet else c) !game_state.creets 
        };
      );
      Lwt.return_unit
    )
  );
  
  (* Thread pour gérer mouseup *)
  Lwt.async (fun () ->
    Lwt_js_events.mouseups Dom_html.document (fun event _ ->
      if !dragging then (
        dragging := false;
        element##.style##.zIndex := Js.string "10";
        
        (* Récupérer la position finale du creet *)
        let final_creet = List.find (fun c -> c.id = creet_dom.creet.id) !game_state.creets in
        let released_creet = { final_creet with is_grabbed = false } in
        
        (* Vérifier si dans l'hôpital pour guérison *)
        let healed_creet = if is_in_hospital released_creet.position then 
          heal_creet released_creet 
        else 
          released_creet 
        in
        
        (* Mettre à jour l'apparence si guéri *)
        update_creet_element_position element healed_creet;
        
        game_state := { !game_state with 
          creets = List.map (fun c -> if c.id = healed_creet.id then healed_creet else c) !game_state.creets 
        };
      );
      Lwt.return_unit
    )
  )

(* Création et ajout d'un nouveau Creet avec son thread - version DOM pure *)
let%client create_and_add_creet current_time =
  let creet = create_creet current_time in
  let element = create_creet_element creet in
  
  (* Ajouter à la zone de jeu principale *)
  let play_zone_opt = Dom_html.document##getElementById (Js.string "play-zone") in
  (match Js.Opt.to_option play_zone_opt with
  | Some play_zone -> 
      Dom.appendChild play_zone element;
      game_container := Some play_zone
  | None -> ());
  
  let creet_dom = { creet; element; thread_cancel = None } in
  
  (* Configurer les événements souris avec Lwt_js_events *)
  setup_creet_mouse_events creet_dom;
  
  (* Démarrer le thread de contrôle *)
  Lwt.async (fun () -> creet_controller_thread creet_dom);
  
  (* Ajouter à la liste *)
  creet_doms := creet_dom :: !creet_doms;
  
  creet

(* Démarrage du jeu côté client - version DOM pure *)
let%client start_game () =
  Random.self_init ();
  let current_time = Unix.time () in
  
  (* Nettoyer les anciens creets DOM *)
  List.iter (fun creet_dom ->
    (match !game_container with
    | Some container -> Dom.removeChild container creet_dom.element
    | None -> ())
  ) !creet_doms;
  creet_doms := [];
  
  (* Créer les nouveaux creets avec leurs éléments DOM *)
  let initial_creets = List.init 15 (fun _ -> create_and_add_creet current_time) in
  
  game_state := {
    creets = initial_creets;
    game_running = true;
    start_time = current_time;
    panic_level = 1.0;
  }

(* Logique de contagion côté client *)
let%client check_infections creets current_time =
  List.map (fun creet ->
    match creet.health with
    | Infected ->
        let infection_duration = match creet.infection_time with
          | Some t -> current_time -. t
          | None -> 0.0
        in
        
        (* Vérifier la transformation vers Berserk ou Evil (une seule fois après 3 secondes) *)
        if infection_duration > 3.0 && not creet.transformation_checked then
          let random_val = Random.float 1.0 in
          if random_val < 0.1 then
            (* 10% de chance de devenir Berserk *)
            { creet with health = Berserk; size = creet.size *. 1.1; transformation_checked = true }
          else if random_val < 0.2 then
            (* 10% de chance de devenir Evil (entre 0.1 et 0.2) *)
            { creet with health = Evil; size = creet.size *. 0.85; transformation_checked = true }
          else
            (* 80% de chance de rester Infected *)
            { creet with transformation_checked = true }
        else creet
    | Berserk ->
        (* Croissance progressive jusqu'à 4x la taille *)
        let new_size = min (!base_creet_size *. 4.0) (creet.size *. 1.02) in
        { creet with size = new_size }
    | _ -> creet
  ) creets

(* Vérification des contacts entre creets côté client *)
let%client check_creet_contacts creets current_time =
  let rec check_contacts acc = function
    | [] -> acc
    | creet :: rest ->
        let updated_creet = 
          if creet.health = Healthy && not creet.is_grabbed then
            List.fold_left (fun c other_creet ->
              if other_creet.id <> c.id && 
                 (other_creet.health = Infected || other_creet.health = Berserk || other_creet.health = Evil) &&
                 not other_creet.is_grabbed && (* Les creets saisis ne contaminent pas *)
                 distance c.position other_creet.position < (c.size +. other_creet.size) /. 2.0 then
                if Random.float 1.0 < 0.02 then (* 2% de chance *)
                  { c with health = Infected; infection_time = Some current_time; transformation_checked = false }
                else c
              else c
            ) creet rest
          else creet
        in
        check_contacts (updated_creet :: acc) rest
  in
  List.rev (check_contacts [] creets)

let%client count_healthy_creets creets =
  let rec count acc = function
    | [] -> acc
    | creet :: rest -> 
        let new_acc = if creet.health = Healthy then acc + 1 else acc in
        count new_acc rest
  in
  count 0 creets

(* Reproduction des creets côté client *)
let%client reproduce_creets creets current_time =
  let healthy_count = List.length (List.filter (fun c -> c.health = Healthy) creets) in
  if healthy_count > 0 && Random.float 1.0 < 0.01 then (* 1% chance par frame *)
    (create_creet current_time) :: creets
  else creets

(* Fonctions utilitaires pour éviter les fonctions anonymes côté client *)
let%client update_creets_positions creets dt current_time =
  let rec update_list acc = function
    | [] -> List.rev acc
    | creet :: rest -> 
        let updated_creet = update_creet_position creet dt current_time in
        update_list (updated_creet :: acc) rest
  in
  update_list [] creets

(* Mise à jour des creets pendant le drag *)
let%client update_dragged_creets creets mouse_pos =
  List.map (fun creet ->
    if creet.is_grabbed then
      update_creet_position_with_mouse creet mouse_pos
    else creet
  ) creets

(* Mise à jour de l'état du jeu côté client *)
let%client update_game_state dt =
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

(* Fonction de calcul de score basée sur les performances *)
let%client calculate_final_score creets game_duration panic_level =
  let total_creets = List.length creets in
  let healthy_count = List.length (List.filter (fun c -> c.health = Healthy) creets) in
  let infected_count = List.length (List.filter (fun c -> c.health = Infected) creets) in
  let berserk_count = List.length (List.filter (fun c -> c.health = Berserk) creets) in
  let evil_count = List.length (List.filter (fun c -> c.health = Evil) creets) in
  
  (* Calcul du score basé sur différents facteurs *)
  let base_score = total_creets * 100 in (* 100 points par creet total *)
  let healthy_bonus = healthy_count * 200 in (* 200 points bonus par creet sain *)
  let survival_bonus = int_of_float (game_duration *. 10.0) in (* 10 points par seconde de survie *)
  let panic_penalty = int_of_float (panic_level *. 50.0) in (* Pénalité selon le niveau de panique *)
  let infection_penalty = (infected_count + berserk_count + evil_count) * 50 in (* Pénalité par creet malade *)
  
  let final_score = max 0 (base_score + healthy_bonus + survival_bonus - panic_penalty - infection_penalty) in
  
  (final_score, total_creets, healthy_count, infected_count, berserk_count, evil_count)

(* Logique côté client - version DOM pure avec Lwt_js_events *)
let%client init_game_client () =
  
  (* Récupérer les éléments DOM *)
  let start_btn_opt = Dom_html.document##getElementById (Js.string "start-button") in
  let apply_btn_opt = Dom_html.document##getElementById (Js.string "apply-settings") in
  let info_elem_opt = Dom_html.document##getElementById (Js.string "game-info") in
  let game_area_opt = Dom_html.document##getElementById (Js.string "game-area") in
  
  match (Js.Opt.to_option start_btn_opt,
         Js.Opt.to_option apply_btn_opt,
         Js.Opt.to_option info_elem_opt,
         Js.Opt.to_option game_area_opt) with
  | (Some start_btn_elem, Some apply_btn_elem, Some info_elem, Some game_area_elem) ->
      let start_btn = Js.Unsafe.coerce start_btn_elem in
      let apply_btn = Js.Unsafe.coerce apply_btn_elem in
      
      (* Gestionnaire de clic sur le bouton Démarrer avec Lwt_js_events *)
      Lwt.async (fun () ->
        Lwt_js_events.clicks start_btn (fun _ _ ->
          start_game ();
          info_elem##.innerHTML := Js.string "✅ Jeu démarré...";
          Lwt.async (fun () -> start_game_info_loop info_elem);
          Lwt.return_unit
        )
      );
      
      (* Gestionnaire de clic sur le bouton Appliquer avec Lwt_js_events *)
      Lwt.async (fun () ->
        Lwt_js_events.clicks apply_btn (fun _ _ ->
          apply_game_settings ();
          (* Mettre à jour la taille de la zone de jeu *)
          game_area_elem##.style##.width := Js.string (Printf.sprintf "%.0fpx" !game_width);
          game_area_elem##.style##.height := Js.string (Printf.sprintf "%.0fpx" !game_height);
          info_elem##.innerHTML := Js.string "✅ Paramètres appliqués !";
          Lwt.return_unit
        )
      );
      
      () (* Jeu initialisé avec succès *)
  | _ ->
      () (* Éléments DOM non trouvés *)

(* Boucle d'information du jeu - version simplifiée *)
let%client start_game_info_loop info_elem =
  let rec loop () =
    let%lwt () = Lwt_js.sleep 0.1 in (* Mise à jour toutes les 100ms *)
    if !game_state.game_running then (
      let healthy_count = count_healthy_creets !game_state.creets in
      if healthy_count = 0 then (
        (* Calculer le score final et les statistiques *)
        let total_creets = List.length !game_state.creets in
        let infected_count = List.length (List.filter (fun c -> c.health = Infected) !game_state.creets) in
        let berserk_count = List.length (List.filter (fun c -> c.health = Berserk) !game_state.creets) in
        let evil_count = List.length (List.filter (fun c -> c.health = Evil) !game_state.creets) in
        let game_duration = Unix.time () -. !game_state.start_time in
        
        let score_message = Printf.sprintf 
          "💀 GAME OVER 💀<br/>🎯 Total creets: %d<br/>🔴 Infectés: %d | 😡 Berserk: %d | 👹 Evil: %d<br/>⏱️ Durée: %.1f secondes | ⚡ Panique finale: %.1fx"
          total_creets infected_count berserk_count evil_count game_duration !game_state.panic_level
        in
        
        let (final_score, _, _, _, _, _) = calculate_final_score !game_state.creets game_duration !game_state.panic_level in
        let final_score_message = Printf.sprintf "<br/>🏆 SCORE FINAL: %d points" final_score in
        
        info_elem##.innerHTML := Js.string (score_message ^ final_score_message);
        game_state := { !game_state with game_running = false };
        Lwt.return_unit
      ) else (
        info_elem##.innerHTML := Js.string 
          (Printf.sprintf "🔵 Creets sains: %d | 🎯 Total: %d | ⚡ Panique: %.1fx" 
            healthy_count (List.length !game_state.creets) !game_state.panic_level);
        loop ()
      )
    ) else 
      Lwt.return_unit
  in
  loop ()

(* Initialiser le jeu quand la page est chargée *)
let%client () = 
  Js_of_ocaml.Dom_html.window##.onload := Js_of_ocaml.Dom_html.handler (fun _ ->
    let () = Js_of_ocaml_lwt.Lwt_js_events.async (fun () -> 
      (* Attendre un peu pour s'assurer que tous les éléments DOM sont créés *)
      let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep 0.1 in
      init_game_client ();
      Lwt.return_unit
    ) in
    Js_of_ocaml.Js._true
  )