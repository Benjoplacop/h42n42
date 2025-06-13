(* Interface pour le jeu des Creets *)

[%%shared.start]

type creet_health = 
  | Healthy
  | Infected
  | Berserk
  | Evil

type position = {
  x: float;
  y: float;
}

type velocity = {
  vx: float;
  vy: float;
}

type creet = {
  id: int;
  position: position;
  velocity: velocity;
  health: creet_health;
  size: float;
  is_grabbed: bool;
  last_direction_change: float;
  infection_time: float option;
}

type game_state = {
  creets: creet list;
  game_running: bool;
  start_time: float;
  panic_level: float;
}

val game_width : float
val game_height : float
val river_height : float
val hospital_height : float
val creets_interface : unit -> [> Html_types.div] Eliom_content.Html.elt

[%%server.start]

val start_game : unit -> unit Lwt.t
val get_game_state : unit -> game_state Lwt.t
val move_creet : int -> float -> float -> bool -> unit Lwt.t
