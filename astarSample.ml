type field = O | X
let maze = [|
  [|O;O;O;O;O;O;O;O;O;O;O;O;O;O;O;O;O;X;O;O;O|];
  [|O;O;O;X;X;O;O;X;X;X;X;O;O;O;O;O;O;X;O;O;O|];
  [|O;X;O;O;X;O;O;X;O;O;X;O;X;X;X;X;X;X;O;O;O|];
  [|O;X;X;X;X;O;O;O;O;O;X;O;O;O;O;O;O;O;O;O;O|];
  [|O;O;O;O;O;O;O;O;O;O;X;O;O;O;O;O;X;X;X;X;O|];
  [|O;O;O;X;X;X;O;X;O;O;X;O;O;O;O;O;O;O;O;X;O|];
  [|O;O;O;O;O;X;O;X;X;X;X;O;O;O;O;O;O;O;O;X;O|];
  [|O;X;O;O;O;X;O;O;O;O;O;O;O;O;O;X;X;X;X;X;O|];
  [|O;X;X;X;X;X;O;O;O;X;X;X;X;X;O;O;O;O;O;O;O|];
  [|O;O;O;O;O;O;O;O;O;O;O;O;O;X;O;O;X;X;X;X;X|];
  [|O;O;O;O;O;O;O;O;O;O;O;O;O;X;O;O;O;O;O;O;O|];
  |]
type pos_type = {x:int; y:int}
type cost_type = float
let len_x = Array.length maze.(0)
let len_y = Array.length maze

let string_of_pos pos = Printf.sprintf "(%d, %d)" pos.x pos.y
let string_of_cost = string_of_float

module AstarSample = Astar.Make(
  struct
    type pos = pos_type
    type cost = cost_type
    let start = {x=0; y=0}
    let goal = {x=len_x - 1; y=len_y - 1}

    let heuristic pos = 
      sqrt (
        (float_of_int (goal.x - pos.x)) ** 2.0 +.
        (float_of_int (goal.y - pos.y)) ** 2.0
      )

    let init_cost = 0.0
    let add_cost a b = a +. b
    let cost_to_move prev_pos now_pos = 1.0
    let compare_cost a b = compare a b

    let next_routes current =
      List.fold_left
        (fun next_points (dx, dy) ->
          let (x, y) = (current.x - dx, current.y - dy) in
          if x >= 0 && y >= 0 && x < len_x && y < len_y && maze.(y).(x) = O
          then {x=x; y=y}::next_points
          else next_points
        )
        [] [(0, -1); (0, 1); (-1, 0); (1, 0)]
  end
)

let _ =
  let resultset = AstarSample.run () in
  List.iter
    (fun (pos, prev, cost) ->
      Printf.printf "pos=%s, prev=%s, cost=%s\n"
        (string_of_pos pos)
        (match prev with None -> "----" | Some x -> string_of_pos x)
        (string_of_cost cost)
    ) resultset

