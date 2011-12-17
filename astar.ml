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
type pos = {x:int; y:int}
let len_x = Array.length maze.(0)
let len_y = Array.length maze
let start = {x=0; y=0}
let goal = {x=len_x - 1; y=len_y - 1}
let plus a b = a +. b
let cost_to_move prev_pos now_pos = 1.0
let compare_cost = compare
let string_of_cost = string_of_float

let h pos = 
  sqrt (
    (float_of_int (goal.x - pos.x)) ** 2.0 +.
    (float_of_int (goal.y - pos.y)) ** 2.0
  )
let string_of_pos pos = 
  Printf.sprintf "(%d, %d)" pos.x pos.y

let next_routes current =
  List.fold_left
    (fun next_points (dx, dy) ->
      let (x, y) = (current.x - dx, current.y - dy) in
      if x >= 0 && y >= 0 && x < len_x && y < len_y && maze.(y).(x) = O
      then {x=x; y=y}::next_points
      else next_points
    )
    [] [(0, -1); (0, 1); (-1, 0); (1, 0)]

type cost = float
type node = {pos:pos; cost:cost; score:cost; prev:pos option}


let score prev now_pos =
  (plus (plus prev.cost (cost_to_move prev.pos now_pos)) (h now_pos))

let remove_minimum_score_node nodeset =
  let (minimum_opt, resultset) =
    List.fold_left
      (fun (candidate_opt, resultset) node ->
        match candidate_opt with
        | None -> (Some node, resultset)
        | Some candidate when compare_cost candidate.score node.score > 0 ->
               (Some node, candidate::resultset)
        | _ -> (candidate_opt, node::resultset)
      ) (None, []) nodeset
  in
  match minimum_opt with
  | None -> failwith "empty nodeset?"
  | Some minimum -> (minimum, resultset)

let find_same_pos nodeset pos =
  List.fold_left
    (fun (found_node_opt, resultset) node ->
      if node.pos = pos then (Some node, resultset)
      else (found_node_opt, node::resultset)
    ) (None, []) nodeset

let create_node pos score prev_node =
  { pos=pos;
    cost=plus prev_node.cost (cost_to_move prev_node.pos pos);
    score=score;
    prev=Some prev_node.pos }

let string_of_node node =
  Printf.sprintf "pos=%s, cost=%s, score=%s, prev=%s"
    (string_of_pos node.pos)
    (string_of_cost node.cost)
    (string_of_cost node.score)
    (match node.prev with None -> "(----)" | Some prev -> string_of_pos prev)

let print_nodeset nodeset =
  List.iter (fun node -> print_endline (string_of_node node)) nodeset

let sort closeset =
  let rec _sort prev sorted = 
    let node = List.find (fun node -> node.pos = prev) closeset in
    let sorted = node::sorted in
    match node.prev with
    | Some prev -> _sort prev sorted
    | None -> sorted
  in
  _sort goal []

let run () =
  let rec _run openset closeset =
    (*
    print_endline "========================================";
    print_endline "  openset                               ";
    print_endline "----------------------------------------";
    print_nodeset openset;
    print_endline "========================================";
    print_endline "  closeset                              ";
    print_endline "----------------------------------------";
    print_nodeset closeset;
    *)
    let (node, openset) = remove_minimum_score_node openset in
    let closeset = node::closeset in
    if node.pos = goal then (sort closeset)
    else (
      let openset =
        List.fold_left
          (fun openset next_pos ->
            let score_of_next_pos = score node next_pos in
            (*
            print_endline "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
            Printf.printf "node:%s, next_pos:%s, score_of_next_pos:%s\n"
            (string_of_node node)
            (string_of_pos next_pos)
            (string_of_score score_of_next_pos);
            *)
            match find_same_pos openset next_pos with
            (* check the same pos in openset *)
            | (None, openset) -> (
              (* if none, check the same pos in closeset *)
              match find_same_pos closeset next_pos with
              | (None, closeset) -> 
                  (create_node next_pos score_of_next_pos node)::openset
              | (Some same_pos_node, closeset) when same_pos_node.score > score_of_next_pos ->
                    (create_node next_pos score_of_next_pos node)::openset
              | (_, closeset) -> openset
            )
            | (Some same_pos_node, openset) when same_pos_node.score > score_of_next_pos ->
                  (create_node next_pos score_of_next_pos node)::openset
            | (_, openset) -> openset
          ) openset (next_routes node.pos)
      in
      _run openset closeset
    )
  in
  let start_node = {pos=start; cost=0.0; score=h start; prev=None} in
  _run [start_node] []

let _ = 
  let closeset = run () in
  print_nodeset closeset
