open Base

module GameState = struct
  type t = {
    variables : (string, string) Map.Poly.t;
    current_scene : string option;
    player_name : string;
  }

  let create player_name =
    { variables = Map.Poly.empty; current_scene = None; player_name }

  let set_variable t name value =
    { t with variables = Map.Poly.set t.variables ~key:name ~data:value }

  let get_variable t name = Map.Poly.find t.variables name

  let set_scene t scene = { t with current_scene = Some scene }
end

module Events = struct
  type event =
    | SceneChange of string
    | VariableSet of string * string
    | PlayerChoice of int
    | GameStart
    | Game_end
  [@@deriving sexp, compare]

  type t = event list

  let create () = []

  let add_event events event = event :: events

  let get_events events = List.rev events
end

module Engine = struct
  type t = {
    state : GameState.t;
    events : Events.t;
  }

  let create player_name =
    { state = GameState.create player_name; events = Events.create () }

  let process_event t event =
    let events = Events.add_event t.events event in
    let state =
      match event with
      | Events.SceneChange scene -> GameState.set_scene t.state scene
      | Events.VariableSet (name, value) -> GameState.set_variable t.state name value
      | _ -> t.state
    in
    { state; events }

  let get_current_scene t = t.state.current_scene

  let get_player_name t = t.state.player_name

  let get_variable t name = GameState.get_variable t.state name
end
