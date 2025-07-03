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

    (* Canvas de jeu *)
    Html.canvas ~a:[
      Html.a_id "game-canvas";
      Html.a_class ["game-canvas"];
      Html.a_width (int_of_float !game_width);
      Html.a_height (int_of_float !game_height);
      Html.a_style ("border: 2px solid #333; display: block; margin: 10px auto;")
    ] [];
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

(* Variables pour le rendu de l'herbe *)
let%client grass_pattern = ref [||]
let%client grass_generated = ref false

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

let%client update_creet_position_with_mouse creet mouse_pos =
  let new_pos = { 
    x = mouse_pos.x -. !mouse_offset.x; 
    y = mouse_pos.y -. !mouse_offset.y 
  } in
  { creet with position = new_pos }

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

(* Création et ajout d'un nouveau Creet avec son thread *)
let%client create_and_add_creet current_time =
  let creet = create_creet current_time in
  let element = create_creet_element creet in
  
  (* Ajouter à la zone de jeu *)
  (match !game_container with
  | Some container -> Dom.appendChild container element
  | None -> 
      let play_zone_opt = Dom_html.document##getElementById (Js.string "play-zone") in
      match Js.Opt.to_option play_zone_opt with
      | Some play_zone -> 
          Dom.appendChild play_zone element;
          game_container := Some play_zone
      | None -> ());
  
  let creet_dom = { creet; element; thread_cancel = None } in
  
  (* Configurer les événements souris *)
  setup_creet_mouse_events creet_dom;
  
  (* Démarrer le thread de contrôle *)
  Lwt.async (fun () -> creet_controller_thread creet_dom);
  
  (* Ajouter à la liste *)
  creet_doms := creet_dom :: !creet_doms;
  
  creet

(* Fonction pour démarrer le jeu *)
let%client start_game () =
  Random.self_init ();
  let current_time = Unix.time () in
  
  (* Nettoyer les anciens creets *)
  List.iter (fun creet_dom ->
    Dom.removeChild (Option.get !game_container) creet_dom.element
  ) !creet_doms;
  creet_doms := [];
  
  (* Créer les nouveaux creets *)
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

(* Fonction pour dessiner le fond de la carte *)
let%client draw_map_background ctx =
  (* Effacer le canvas *)
  ctx##clearRect 0.0 0.0 !game_width !game_height;
  
  (* Dessiner le fond de pelouse avec des nuances de vert *)
  let grass_base_color = "#4CAF50" in
  ctx##.fillStyle := Js_of_ocaml.Js.string grass_base_color;
  ctx##fillRect 0.0 0.0 !game_width !game_height;
  
  (* Générer l'herbe une seule fois si pas déjà fait *)
  if not !grass_generated then (
    let grass_colors = [|"#388E3C"; "#66BB6A"; "#43A047"; "#2E7D32"; "#81C784"|] in
    let grass_data = Array.make 150 (0.0, 0.0, 0.0, 0.0, "") in
    for i = 0 to 149 do
      let random_val1 = Js_of_ocaml.Js.to_float (Js_of_ocaml.Js.Unsafe.fun_call (Js_of_ocaml.Js.Unsafe.js_expr "Math.random") [||]) in
      let random_val2 = Js_of_ocaml.Js.to_float (Js_of_ocaml.Js.Unsafe.fun_call (Js_of_ocaml.Js.Unsafe.js_expr "Math.random") [||]) in
      let random_val3 = Js_of_ocaml.Js.to_float (Js_of_ocaml.Js.Unsafe.fun_call (Js_of_ocaml.Js.Unsafe.js_expr "Math.random") [||]) in
      let random_val4 = Js_of_ocaml.Js.to_float (Js_of_ocaml.Js.Unsafe.fun_call (Js_of_ocaml.Js.Unsafe.js_expr "Math.random") [||]) in
      let random_val5 = Js_of_ocaml.Js.to_float (Js_of_ocaml.Js.Unsafe.fun_call (Js_of_ocaml.Js.Unsafe.js_expr "Math.random") [||]) in
      let x = random_val1 *. !game_width in
      let y = random_val2 *. !game_height in
      let width = 2.0 +. (random_val3 *. 4.0) in
      let height = 8.0 +. (random_val4 *. 12.0) in
      let color_index = int_of_float (random_val5 *. float_of_int (Array.length grass_colors)) in
      let color = grass_colors.(color_index) in
      grass_data.(i) <- (x, y, width, height, color);
    done;
    grass_pattern := grass_data;
    grass_generated := true;
  );
  
  (* Dessiner l'herbe avec les données précalculées *)
  Array.iter (fun (x, y, width, height, color) ->
    ctx##.fillStyle := Js_of_ocaml.Js.string color;
    ctx##fillRect x y width height;
  ) !grass_pattern;
  
  (* Dessiner les zones spéciales *)
  (* Zone de l'hôpital - fond rosé opaque *)
  ctx##.fillStyle := Js_of_ocaml.Js.string "#FFB6C1";
  ctx##fillRect 0.0 (!game_height -. hospital_height) !game_width hospital_height;
  
  (* Texte de l'hôpital *)
  ctx##.fillStyle := Js_of_ocaml.Js.string "#2E7D32";
  ctx##.font := Js_of_ocaml.Js.string "16px Arial";
  ctx##.textAlign := Js_of_ocaml.Js.string "center";
  ctx##fillText (Js_of_ocaml.Js.string "🏥 HÔPITAL - Déposez les creets malades ici !!!") (!game_width /. 2.0) (!game_height -. hospital_height /. 2.0);
  
  (* Zone de la rivière toxique - fond bleu opaque *)
  ctx##.fillStyle := Js_of_ocaml.Js.string "#2196F3";
  ctx##fillRect 0.0 0.0 !game_width river_height;
  
  (* Texte de la rivière *)
  ctx##.fillStyle := Js_of_ocaml.Js.string "#8b00ff";
  ctx##.font := Js_of_ocaml.Js.string "14px Arial";
  ctx##.textAlign := Js_of_ocaml.Js.string "center";
  ctx##fillText (Js_of_ocaml.Js.string "🌊 RIVIÈRE TOXIQUE - DANGER !!!") (!game_width /. 2.0) (river_height /. 2.0)

(* Boucle de jeu principale côté client *)
let%client start_game_loop _canvas ctx info_elem =
  let rec loop last_time =
    let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep 0.016 in (* ~60 FPS *)
    let current_time = Js_of_ocaml.Js.to_float (Js_of_ocaml.Js.Unsafe.fun_call (Js_of_ocaml.Js.Unsafe.js_expr "Date.now") [||]) /. 1000.0 in
    let dt = if last_time = 0.0 then 0.016 else current_time -. last_time in
    
    if !game_state.game_running then (
      (* Mise à jour de l'état du jeu *)
      update_game_state dt;
      
      (* Rendu *)
      draw_map_background ctx;
      
      (* Dessiner les zones spéciales *)
      (* Zone de l'hôpital - fond vert OPAQUE pour effacer l'ancien texte *)
      ctx##.fillStyle := Js_of_ocaml.Js.string "#C8E6C9";
      ctx##fillRect 0.0 (!game_height -. hospital_height) !game_width hospital_height;
      
      (* Texte de l'hôpital - dynamique selon l'urgence *)
      let infected_count = List.length (List.filter (fun c -> c.health <> Healthy) !game_state.creets) in
      let hospital_text = 
        if infected_count > 10 then
          "🏥 HÔPITAL - URGENCE !!! Déposez les malades ici !!!"
        else if infected_count > 5 then
          "🏥 HÔPITAL - Déposez les creets malades ici !!!"
        else
          "🏥 HÔPITAL - Centre de soins"
      in
      
      ctx##.fillStyle := Js_of_ocaml.Js.string "#2E7D32";
      ctx##.font := Js_of_ocaml.Js.string "bold 16px Arial";
      ctx##.textAlign := Js_of_ocaml.Js.string "center";
      ctx##fillText (Js_of_ocaml.Js.string hospital_text) (!game_width /. 2.0) (!game_height -. hospital_height /. 2.0);
      
      (* Zone de la rivière toxique - fond jaune OPAQUE pour effacer l'ancien texte *)
      ctx##.fillStyle := Js_of_ocaml.Js.string "#8b00ff";
      ctx##fillRect 0.0 0.0 !game_width river_height;
      
      (* Texte de la rivière - dynamique selon le niveau de panique *)
      let river_text = 
        if !game_state.panic_level > 1.5 then
          "☠️ RIVIÈRE TOXIQUE - DANGER EXTRÊME !!!!"
        else if !game_state.panic_level > 1.2 then
          "☠️ RIVIÈRE TOXIQUE - DANGER !!!!!"
        else
          "☠️ RIVIÈRE TOXIQUE - DANGER !!!"
      in
      
      ctx##.fillStyle := Js_of_ocaml.Js.string "#40E0D0";
      ctx##.font := Js_of_ocaml.Js.string "bold 16px Arial";
      ctx##.textAlign := Js_of_ocaml.Js.string "center";
      ctx##fillText (Js_of_ocaml.Js.string river_text) (!game_width /. 2.0) (river_height /. 2.0);
      
      (* Dessiner les creets *)
      List.iter (fun creet ->
        let size = creet.size in
        let x = creet.position.x in
        let y = creet.position.y in
        let r = size /. 2.0 in
        
        (* Couleur de base selon le type *)
        let base_color = match creet.health with
          | Healthy -> "#4da6ff"   (* Bleu *)
          | Infected -> "#ff8c00"  (* Orange *)
          | Berserk -> "#cc0000"   (* Rouge foncé *)
          | Evil -> "#8b00ff"      (* Violet *)
        in
        
        (* Dessiner le corps principal *)
        ctx##.fillStyle := Js_of_ocaml.Js.string base_color;
        ctx##.strokeStyle := Js_of_ocaml.Js.string "#333";
        ctx##.lineWidth := 2.0;
        ctx##beginPath ();
        ctx##arc x y r 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
        ctx##fill;
        ctx##stroke;
        
        (* Dessiner les yeux *)
        ctx##.fillStyle := Js_of_ocaml.Js.string "#000";
        let eye_size = r /. 10.0 in
        let eye_offset_x = r /. 4.0 in
        let eye_offset_y = r /. 8.0 in
        
        (* Œil gauche *)
        ctx##beginPath ();
        ctx##arc (x -. eye_offset_x) (y -. eye_offset_y) eye_size 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
        ctx##fill;
        
        (* Œil droit *)
        ctx##beginPath ();
        ctx##arc (x +. eye_offset_x) (y -. eye_offset_y) eye_size 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
        ctx##fill;
        
        (* Dessiner la bouche selon le type *)
        ctx##.strokeStyle := Js_of_ocaml.Js.string "#000";
        ctx##.lineWidth := 2.0;
        ctx##.fillStyle := Js_of_ocaml.Js.string "transparent";
        ctx##beginPath ();
        
        (match creet.health with
        | Healthy -> 
            (* Sourire *)
            let mouth_y = y +. r /. 3.0 in
            ctx##arc x mouth_y (r /. 3.0) 0.0 Js_of_ocaml.Js.math##._PI Js_of_ocaml.Js._false;
            
        | Infected -> 
            (* Bouche triste *)
            let mouth_y = y +. r /. 2.0 in
            ctx##arc x mouth_y (r /. 3.0) Js_of_ocaml.Js.math##._PI (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
            
        | Berserk -> 
            (* Bouche en colère *)
            let mouth_y = y +. r /. 2.0 in
            ctx##arc x mouth_y (r /. 3.0) Js_of_ocaml.Js.math##._PI (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
            
        | Evil -> 
            (* Bouche malicieuse *)
            let mouth_y = y +. r /. 2.0 in
            ctx##arc x mouth_y (r /. 3.0) Js_of_ocaml.Js.math##._PI (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
        );
        ctx##stroke;
        
        (* Ajouter des détails spécifiques selon le type *)
        (match creet.health with
        | Healthy -> () (* Rien de spécial *)
        | Infected -> 
            (* Taches d'infection *)
            ctx##.fillStyle := Js_of_ocaml.Js.string "#ff0000";
            ctx##beginPath ();
            ctx##arc (x -. r /. 2.0) (y -. r /. 2.0) (r /. 15.0) 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
            ctx##fill;
            ctx##beginPath ();
            ctx##arc (x +. r /. 2.0) (y -. r /. 3.0) (r /. 15.0) 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
            ctx##fill;
            
        | Berserk -> 
            (* Sourcils froncés *)
            ctx##.strokeStyle := Js_of_ocaml.Js.string "#000";
            ctx##.lineWidth := 2.0;
            ctx##beginPath ();
            ctx##moveTo (x -. r /. 2.0) (y -. r /. 2.0);
            ctx##lineTo (x -. r /. 6.0) (y -. r /. 4.0);
            ctx##lineTo (x -. r /. 2.0) (y -. r /. 8.0);
            ctx##stroke;
            ctx##beginPath ();
            ctx##moveTo (x +. r /. 2.0) (y -. r /. 2.0);
            ctx##lineTo (x +. r /. 6.0) (y -. r /. 4.0);
            ctx##lineTo (x +. r /. 2.0) (y -. r /. 8.0);
            ctx##stroke;
            
        | Evil -> 
            (* Petites cornes *)
            ctx##.strokeStyle := Js_of_ocaml.Js.string "#000";
            ctx##.lineWidth := 2.0;
            ctx##beginPath ();
            ctx##moveTo (x -. r /. 6.0) (y -. r /. 3.0);
            ctx##lineTo x (y -. r);
            ctx##lineTo (x +. r /. 6.0) (y -. r /. 3.0);
            ctx##stroke;
            (* Point rouge au centre *)
            ctx##.fillStyle := Js_of_ocaml.Js.string "#ff0000";
            ctx##beginPath ();
            ctx##arc x (y +. r /. 8.0) (r /. 20.0) 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
            ctx##fill;
        );
        
        (* Ajouter un contour pour les creets saisis *)
        if creet.is_grabbed then (
          ctx##.strokeStyle := Js_of_ocaml.Js.string "#2E7D32";
          ctx##.lineWidth := 3.0;
          ctx##beginPath ();
          ctx##arc x y r 0.0 (2.0 *. Js_of_ocaml.Js.math##._PI) Js_of_ocaml.Js._false;
          ctx##stroke
        )
      ) !game_state.creets;
      
      (* Mise à jour info *)
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
        
        (* Calculer le score final basé sur les performances *)
        let (final_score, _, _, _, _, _) = calculate_final_score !game_state.creets game_duration !game_state.panic_level in
        let final_score_message = Printf.sprintf "<br/>🏆 SCORE FINAL: %d points" final_score in
        
        info_elem##.innerHTML := Js_of_ocaml.Js.string (score_message ^ final_score_message);
        game_state := { !game_state with game_running = false }
      ) else (
        info_elem##.innerHTML := Js_of_ocaml.Js.string 
          (Printf.sprintf "🔵 Creets sains: %d | 🎯 Total: %d | ⚡ Panique: %.1fx" 
            healthy_count (List.length !game_state.creets) !game_state.panic_level)
      );
      
      loop current_time
    ) else (
      Lwt.return_unit
    )
  in
  loop 0.0

(* Démarrage du jeu côté client *)
let%client start_game () =
  Random.self_init ();
  let current_time = Js_of_ocaml.Js.to_float (Js_of_ocaml.Js.Unsafe.fun_call (Js_of_ocaml.Js.Unsafe.js_expr "Date.now") [||]) /. 1000.0 in
  let initial_creets = List.init 15 (fun _ -> create_creet current_time) in
  game_state := {
    creets = initial_creets;
    game_running = true;
    start_time = current_time;
    panic_level = 1.0;
  }
(* Services Eliom simplifiés - optionnels pour synchronisation *)
let%server ping_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["api"; "creets"; "ping"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

(* Références côté client *)
let%client ping_service = ~%ping_service

(* Handlers simplifiés *)
let%server () =
  Eliom_registration.String.register ~service:ping_service
    (fun () () ->
      Lwt.return ("text/plain", "pong")
    )

(* Logique côté client - version refactorisée *)
let%client init_game_client () =
  
  (* Récupérer les éléments DOM *)
  let canvas_opt = Js_of_ocaml.Dom_html.document##getElementById (Js_of_ocaml.Js.string "game-canvas") in
  let start_btn_opt = Js_of_ocaml.Dom_html.document##getElementById (Js_of_ocaml.Js.string "start-button") in
  let apply_btn_opt = Js_of_ocaml.Dom_html.document##getElementById (Js_of_ocaml.Js.string "apply-settings") in
  let info_elem_opt = Js_of_ocaml.Dom_html.document##getElementById (Js_of_ocaml.Js.string "game-info") in
  
  match (Js_of_ocaml.Js.Opt.to_option canvas_opt, 
         Js_of_ocaml.Js.Opt.to_option start_btn_opt,
         Js_of_ocaml.Js.Opt.to_option apply_btn_opt,
         Js_of_ocaml.Js.Opt.to_option info_elem_opt) with
  | (Some canvas_elem, Some start_btn_elem, Some apply_btn_elem, Some info_elem) ->
      let canvas = Js_of_ocaml.Js.Unsafe.coerce canvas_elem in
      let start_btn = Js_of_ocaml.Js.Unsafe.coerce start_btn_elem in
      let apply_btn = Js_of_ocaml.Js.Unsafe.coerce apply_btn_elem in
      let ctx = canvas##getContext (Js_of_ocaml.Dom_html._2d_) in
      
      (* Dessiner le fond initial de la carte *)
      draw_map_background ctx;
      
      (* Gestionnaire de clic sur le bouton *)
      start_btn##.onclick := Js_of_ocaml.Dom_html.handler (fun _ ->
        start_game ();
        info_elem##.innerHTML := Js_of_ocaml.Js.string "✅ Jeu démarré...";
        Lwt.async (fun () -> start_game_loop canvas ctx info_elem);
        Js_of_ocaml.Js._false
      );
      
      (* Gestionnaire de clic sur le bouton Appliquer *)
      apply_btn##.onclick := Js_of_ocaml.Dom_html.handler (fun _ ->
        apply_game_settings ();
        (* Mettre à jour la taille du canvas si nécessaire *)
        canvas##.width := int_of_float !game_width;
        canvas##.height := int_of_float !game_height;
        (* Redessiner le fond avec les nouvelles dimensions *)
        draw_map_background ctx;
        info_elem##.innerHTML := Js_of_ocaml.Js.string "✅ Paramètres appliqués !";
        Js_of_ocaml.Js._false
      );
      
      (* Gestionnaires d'événements de souris pour le drag & drop *)
      canvas##.onmousedown := Js_of_ocaml.Dom_html.handler (fun event ->
        let mouse_pos = get_mouse_pos canvas event in
        (match find_creet_at_position mouse_pos !game_state.creets with
        | Some creet ->
            dragging_creet := Some creet.id;
            mouse_offset := { 
              x = mouse_pos.x -. creet.position.x; 
              y = mouse_pos.y -. creet.position.y 
            };
            (* Marquer le creet comme saisi *)
            let updated_creets = List.map (fun c ->
              if c.id = creet.id then { c with is_grabbed = true }
              else c
            ) !game_state.creets in
            game_state := { !game_state with creets = updated_creets };
        | None -> ());
        Js_of_ocaml.Js._false
      );
      
      canvas##.onmousemove := Js_of_ocaml.Dom_html.handler (fun event ->
        (match !dragging_creet with
        | Some creet_id ->
            let mouse_pos = get_mouse_pos canvas event in
            let updated_creets = List.map (fun creet ->
              if creet.id = creet_id then
                update_creet_position_with_mouse creet mouse_pos
              else creet
            ) !game_state.creets in
            game_state := { !game_state with creets = updated_creets };
        | None -> ());
        Js_of_ocaml.Js._false
      );
      
      canvas##.onmouseup := Js_of_ocaml.Dom_html.handler (fun event ->
        (match !dragging_creet with
        | Some creet_id ->
            let mouse_pos = get_mouse_pos canvas event in
            (* Libérer le creet et lui donner une nouvelle vitesse aléatoire *)
            let updated_creets = List.map (fun creet ->
              if creet.id = creet_id then
                let released_creet = { creet with 
                  is_grabbed = false;
                  velocity = {
                    vx = random_float (-.(!base_speed)) (!base_speed);
                    vy = random_float (-.(!base_speed)) (!base_speed);
                  }
                } in
                (* Soigner le creet s'il est déposé dans l'hôpital *)
                if is_in_hospital mouse_pos && released_creet.health <> Healthy then
                  heal_creet released_creet
                else released_creet
              else creet
            ) !game_state.creets in
            game_state := { !game_state with creets = updated_creets };
            dragging_creet := None;
        | None -> ());
        Js_of_ocaml.Js._false
      );
      
      (* Gérer le cas où la souris sort du canvas *)
      canvas##.onmouseleave := Js_of_ocaml.Dom_html.handler (fun _ ->
        (match !dragging_creet with
        | Some creet_id ->
            let updated_creets = List.map (fun creet ->
              if creet.id = creet_id then
                let released_creet = { creet with 
                  is_grabbed = false;
                  velocity = {
                    vx = random_float (-.(!base_speed)) (!base_speed);
                    vy = random_float (-.(!base_speed)) (!base_speed);
                  }
                } in
                (* Soigner le creet s'il est dans l'hôpital *)
                if is_in_hospital creet.position && released_creet.health <> Healthy then
                  heal_creet released_creet
                else released_creet
              else creet
            ) !game_state.creets in
            game_state := { !game_state with creets = updated_creets };
            dragging_creet := None;
        | None -> ());
        Js_of_ocaml.Js._false
      );
      
      () (* Jeu initialisé avec succès *)
  | _ ->
      () (* Éléments DOM non trouvés *)

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