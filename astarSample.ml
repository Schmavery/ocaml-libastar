let maze = [
  "S                #   ";
  "   ##  ####      # # ";
  " #  #  #  # ###### # ";
  " ####     #        # ";
  "     #     #    #### ";
  " ##### ########    # ";
  "              ##   # ";
  " ###########   ##### ";
  " #           ##      ";
  "## ###########  #####";
  "                    G";
  ]
let elm_in_maze x y = String.get (List.nth maze y) x

type position = {x:int; y:int}
let len_x = String.length (List.nth maze 0)
let len_y = List.length maze
let start = {x=0; y=0}
let goal = {x=len_x - 1; y=len_y - 1}

let string_of_pos pos = Printf.sprintf "(%d, %d)" pos.x pos.y
let string_of_cost = string_of_float

module MazeAstar= Astar.Make(
  struct
    type pos = position
    type cost = float

    let heuristic pos = 
      sqrt (
        (float_of_int (goal.x - pos.x)) ** 2.0 +.
        (float_of_int (goal.y - pos.y)) ** 2.0
      )

    let add_cost a b = a +. b
    let cost_to_move prev_pos now_pos = 1.0
    let compare_cost a b = compare a b

    let next_routes current =
      List.fold_left
        (fun next_points (dx, dy) ->
          let (x, y) = (current.x - dx, current.y - dy) in
          if x >= 0 && y >= 0 && x < len_x && y < len_y && 
            elm_in_maze x y != '#'
          then {x=x; y=y}::next_points
          else next_points
        )
        [] [(0, -1); (0, 1); (-1, 0); (1, 0)]
  end
)

let _ =
  let resultset = MazeAstar.run start goal 0.0 in
  List.iter
    (fun ({x=x; y=y}, prev, cost) ->
      (*
      Printf.printf "pos=%s, prev=%s, cost=%s\n"
      (string_of_pos pos)
      (match prev with None -> "----" | Some x -> string_of_pos x)
      (string_of_cost cost)
      *)
      String.set (List.nth maze y) x '.';
    ) resultset;
  List.iter (fun row -> print_endline row) maze

