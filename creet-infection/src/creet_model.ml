module CreetState = struct
  type state = Healthy | Sick | Berserk | Mean

  type position = { x: float; y: float }
  type direction = { dx: float; dy: float }
  
  type creet = {
    mutable state: state;
    mutable position: position;
    mutable direction: direction;
    mutable speed: float;
    mutable size: float;
    mutable color: string;
    mutable special_state: string option;
  }

  let create_creet ~state ~position ~direction ~speed ~size ~color =
    { state; position; direction; speed; size; color; special_state = None }

  let move creet =
    creet.position.x <- creet.position.x +. creet.direction.dx *. creet.speed;
    creet.position.y <- creet.position.y +. creet.direction.dy *. creet.speed;

  let check_boundary creet width height =
    if creet.position.x < 0. || creet.position.x > width then
      creet.direction.dx <- -. creet.direction.dx;
    if creet.position.y < 0. || creet.position.y > height then
      creet.direction.dy <- -. creet.direction.dy

  let update_state creet =
    match creet.state with
    | Sick ->
        creet.color <- "red"; (* Change color to indicate sickness *)
        creet.speed <- creet.speed *. 0.85; (* Slow down by 15% *)
        if Random.float 1.0 < 0.1 then
          creet.state <- Berserk
        else if Random.float 1.0 < 0.1 then
          creet.state <- Mean
    | _ -> ()

  let reproduce creets =
    List.filter (fun c -> c.state = Healthy) creets
    |> List.length
    |> fun count -> if count > 0 then Some (create_creet ~state:Healthy ~position:{x=0.0; y=0.0} ~direction:{dx=1.0; dy=1.0} ~speed:1.0 ~size:1.0 ~color:"green") else None
end