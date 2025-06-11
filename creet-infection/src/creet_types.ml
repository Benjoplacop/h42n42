type state = Healthy | Sick | Berserk | Mean

type position = { x: float; y: float }

type direction = Up | Down | Left | Right

type creet = {
  state: state;
  position: position;
  direction: direction;
  speed: float;
  size: float;
  color: string;
  special_states: string list;
}