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
  transformation_checked: bool; (* Pour s'assurer qu'on ne vérifie la transformation qu'une fois *)
} [@@deriving json]

type game_state = {
  creets: creet list;
  game_running: bool;
  start_time: float;
  panic_level: float;
} [@@deriving json]

(* Constantes du jeu - uniquement les valeurs, pas les calculs *)
let game_width = 1000.0
let game_height = 700.0
let river_height = 50.0
let hospital_height = 50.0
let base_creet_size = 40.0
let base_speed = 50.0
]

(* État du jeu côté client *)
let%client game_state = ref {
  creets = [];
  game_running = false;
  start_time = 0.0;
  panic_level = 1.0;
}
let%client next_id = ref 1

(* État pour le glisser-déposer *)
let%client dragging_creet = ref None
let%client mouse_offset = ref { x = 0.0; y = 0.0 }

(* État pour l'herbe (généré une seule fois) *)
let%client grass_pattern = ref [||]
let%client grass_generated = ref false

(* Fonctions utilitaires côté client *)
let%client random_float min_val max_val = 
  min_val +. (Random.float (max_val -. min_val))

let%client distance p1 p2 = 
  sqrt ((p1.x -. p2.x) ** 2.0 +. (p1.y -. p2.y) ** 2.0)

let%client normalize_velocity v speed =
  let length = sqrt (v.vx ** 2.0 +. v.vy ** 2.0) in
  if length = 0.0 then v
  else { vx = v.vx *. speed /. length; vy = v.vy *. speed /. length }

(* Fonctions pour les interactions souris *)
let%client get_mouse_pos canvas event =
  let rect = canvas##getBoundingClientRect () in
  let x = (Js_of_ocaml.Js.to_float event##.clientX) -. (Js_of_ocaml.Js.to_float rect##.left) in
  let y = (Js_of_ocaml.Js.to_float event##.clientY) -. (Js_of_ocaml.Js.to_float rect##.top) in
  { x; y }

let%client find_creet_at_position pos creets =
  List.find_opt (fun creet ->
    let dist = distance pos creet.position in
    dist <= creet.size /. 2.0
  ) creets

let%client is_in_hospital pos =
  pos.y >= (game_height -. hospital_height)

let%client heal_creet creet =
  if creet.health <> Healthy then
    { creet with 
      health = Healthy; 
      size = base_creet_size; (* Remettre à la taille normale *)
      infection_time = None;
      transformation_checked = false; (* Réinitialiser pour une éventuelle réinfection *)
    }
  else creet

let%client update_creet_position_with_mouse creet mouse_pos =
  { creet with 
    position = { 
      x = mouse_pos.x -. !mouse_offset.x; 
      y = mouse_pos.y -. !mouse_offset.y 
    };
    velocity = { vx = 0.0; vy = 0.0 }; (* Arrêter le mouvement pendant le drag *)
  }

(* Création d'un nouveau creet côté client *)
let%client create_creet current_time =
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
    transformation_checked = false;
  } in
  Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string 
    (Printf.sprintf "🎯 Creet créé: ID=%d pos=(%.1f,%.1f) size=%.1f" 
      new_creet.id new_creet.position.x new_creet.position.y new_creet.size));
  new_creet

(* Logique de mouvement et collision côté client *)
let%client update_creet_position creet dt current_time =
  if creet.is_grabbed then creet
  else
    let speed_modifier = match creet.health with
      | Healthy -> 1.0
      | Infected -> 0.85 (* 15% plus lent *)
      | Berserk -> 1.0
      | Evil -> 1.3 (* plus rapide pour chasser *)
    in
    
    (* Changement de direction aléatoire ou poursuite pour les Evil *)
    let velocity = 
      if creet.health = Evil then
        (* Les creets Evil chassent les creets sains *)
        let healthy_creets = List.filter (fun c -> c.health = Healthy && not c.is_grabbed) !game_state.creets in
        match healthy_creets with
        | [] -> creet.velocity (* Pas de cible, garde la direction actuelle *)
        | targets ->
            (* Trouve le creet sain le plus proche *)
            let closest_target = List.fold_left (fun acc target ->
              let dist_acc = distance creet.position acc.position in
              let dist_target = distance creet.position target.position in
              if dist_target < dist_acc then target else acc
            ) (List.hd targets) (List.tl targets) in
            (* Direction vers la cible *)
            let dx = closest_target.position.x -. creet.position.x in
            let dy = closest_target.position.y -. creet.position.y in
            let norm = sqrt (dx *. dx +. dy *. dy) in
            if norm > 0.0 then
              { vx = dx /. norm *. base_speed; vy = dy /. norm *. base_speed }
            else creet.velocity
      else if current_time -. creet.last_direction_change > 2.0 && Random.float 1.0 < 0.1 then
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
        (* Collision avec la rivière - infection automatique pour les creets sains *)
        let infected = if creet.health = Healthy then Infected else creet.health in
        (river_height +. creet.size /. 2.0, abs_float velocity.vy, infected)
      else if new_y >= game_height -. hospital_height -. creet.size /. 2.0 then 
        (* Collision avec l'hôpital - PAS de guérison automatique *)
        (game_height -. hospital_height -. creet.size /. 2.0, -.abs_float velocity.vy, creet.health)
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
        let new_size = min (base_creet_size *. 4.0) (creet.size *. 1.02) in
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

(* Fonction pour dessiner le fond de la carte *)
let%client draw_map_background ctx =
  (* Effacer le canvas *)
  ctx##clearRect 0.0 0.0 game_width game_height;
  
  (* Dessiner le fond de pelouse avec des nuances de vert *)
  let grass_base_color = "#4CAF50" in
  ctx##.fillStyle := Js_of_ocaml.Js.string grass_base_color;
  ctx##fillRect 0.0 0.0 game_width game_height;
  
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
      let x = random_val1 *. game_width in
      let y = random_val2 *. game_height in
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
  ctx##fillRect 0.0 (game_height -. hospital_height) game_width hospital_height;
  
  (* Texte de l'hôpital *)
  ctx##.fillStyle := Js_of_ocaml.Js.string "#C2185B";
  ctx##.font := Js_of_ocaml.Js.string "16px Arial";
  ctx##.textAlign := Js_of_ocaml.Js.string "center";
  ctx##fillText (Js_of_ocaml.Js.string "🏥 HÔPITAL - Déposez les creets malades ici") (game_width /. 2.0) (game_height -. hospital_height /. 2.0);
  
  (* Zone de la rivière toxique - fond bleu opaque *)
  ctx##.fillStyle := Js_of_ocaml.Js.string "#2196F3";
  ctx##fillRect 0.0 0.0 game_width river_height;
  
  (* Texte de la rivière *)
  ctx##.fillStyle := Js_of_ocaml.Js.string "#1565C0";
  ctx##.font := Js_of_ocaml.Js.string "14px Arial";
  ctx##.textAlign := Js_of_ocaml.Js.string "center";
  ctx##fillText (Js_of_ocaml.Js.string "🌊 RIVIÈRE TOXIQUE - DANGER!") (game_width /. 2.0) (river_height /. 2.0)

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
      (* Zone de l'hôpital - fond vert clair *)
      ctx##.fillStyle := Js_of_ocaml.Js.string "rgba(76, 175, 80, 0.3)";
      ctx##fillRect 0.0 (game_height -. hospital_height) game_width hospital_height;
      
      (* Texte de l'hôpital *)
      ctx##.fillStyle := Js_of_ocaml.Js.string "#2E7D32";
      ctx##.font := Js_of_ocaml.Js.string "16px Arial";
      ctx##.textAlign := Js_of_ocaml.Js.string "center";
      ctx##fillText (Js_of_ocaml.Js.string "🏥 HÔPITAL - Déposez les creets malades ici") (game_width /. 2.0) (game_height -. hospital_height /. 2.0);
      
      (* Zone de la rivière toxique - fond bleu-vert *)
      ctx##.fillStyle := Js_of_ocaml.Js.string "rgba(255, 193, 7, 0.3)";
      ctx##fillRect 0.0 0.0 game_width river_height;
      
      (* Texte de la rivière *)
      ctx##.fillStyle := Js_of_ocaml.Js.string "#FF8F00";
      ctx##.font := Js_of_ocaml.Js.string "14px Arial";
      ctx##.textAlign := Js_of_ocaml.Js.string "center";
      ctx##fillText (Js_of_ocaml.Js.string "☠️ RIVIÈRE TOXIQUE - DANGER!") (game_width /. 2.0) (river_height /. 2.0);
      
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
        info_elem##.innerHTML := Js_of_ocaml.Js.string "💀 GAME OVER";
        game_state := { !game_state with game_running = false }
      ) else (
        info_elem##.innerHTML := Js_of_ocaml.Js.string 
          (Printf.sprintf "🟢 Creets sains: %d | 🎯 Total: %d | ⚡ Panique: %.1fx" 
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
  };
  Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string 
    (Printf.sprintf "🎮 Jeu démarré avec %d creets" (List.length initial_creets)))
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

(* Interface utilisateur du jeu *)
let%shared creets_interface () =
  div ~a:[a_class ["creets-game"]]
    [ div ~a:[a_class ["game-controls"]]
        [ button ~a:[a_id "start-button"; a_class ["btn"; "btn-primary"]] [txt "Démarrer le Jeu"]
        ]
    ; div ~a:[a_id "game-info"; a_class ["game-info"]] []
    ; div ~a:[a_class ["canvas-container"]]
        [ canvas ~a:[
            a_id "game-canvas";
            a_class ["game-canvas"];
            a_width (int_of_float game_width);
            a_height (int_of_float game_height);
            a_style "background: linear-gradient(to bottom, #87CEEB 0%, #87CEEB 8%,rgb(255, 255, 255) 8%,rgb(255, 255, 255) 92%, #FFB6C1 92%, #FFB6C1 100%); cursor: pointer;"
          ] []
        ]
    ]

(* Logique côté client - version refactorisée *)
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
      
      (* Dessiner le fond initial de la carte *)
      draw_map_background ctx;
      
      (* Gestionnaire de clic sur le bouton *)
      start_btn##.onclick := Js_of_ocaml.Dom_html.handler (fun _ ->
        Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string "🚀 Bouton cliqué - Démarrage du jeu...");
        start_game ();
        info_elem##.innerHTML := Js_of_ocaml.Js.string "✅ Jeu démarré...";
        Lwt.async (fun () -> start_game_loop canvas ctx info_elem);
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
            Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string 
              (Printf.sprintf "🖱️ Creet %d saisi" creet.id));
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
                    vx = random_float (-.base_speed) base_speed;
                    vy = random_float (-.base_speed) base_speed;
                  }
                } in
                (* Soigner le creet s'il est déposé dans l'hôpital *)
                if is_in_hospital mouse_pos && released_creet.health <> Healthy then (
                  Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string 
                    (Printf.sprintf "🏥 Creet %d soigné à l'hôpital!" creet.id));
                  heal_creet released_creet
                ) else released_creet
              else creet
            ) !game_state.creets in
            game_state := { !game_state with creets = updated_creets };
            Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string 
              (Printf.sprintf "🖱️ Creet %d libéré" creet_id));
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
                    vx = random_float (-.base_speed) base_speed;
                    vy = random_float (-.base_speed) base_speed;
                  }
                } in
                (* Soigner le creet s'il est dans l'hôpital *)
                if is_in_hospital creet.position && released_creet.health <> Healthy then (
                  Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string 
                    (Printf.sprintf "🏥 Creet %d soigné à l'hôpital!" creet.id));
                  heal_creet released_creet
                ) else released_creet
              else creet
            ) !game_state.creets in
            game_state := { !game_state with creets = updated_creets };
            dragging_creet := None;
        | None -> ());
        Js_of_ocaml.Js._false
      );
      
      Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string "✅ Jeu initialisé avec succès")
  | _ ->
      Js_of_ocaml.Firebug.console##log (Js_of_ocaml.Js.string "❌ Éléments DOM non trouvés")

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
