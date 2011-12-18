type t = S | A | B | C | D | E | G

let routes =
  [
    (S, A, 3);
    (S, B, 10);
    (S, C, 12);
    (A, D, 10);
    (A, B, 2);
    (B, C, 3);
    (B, E, 7);
    (C, E, 3);
    (D, E, 1);
    (D, G, 3);
    (E, D, 1);
    (E, G, 5);
  ]

let string_of_point = function
    | S -> "S"
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"
    | E -> "E"
    | G -> "G"

module GraphAstar =
  Astar.Make(
    struct
      type pos = t
      type cost = int

      let heuristic pos = 0

      let add_cost a b = a + b
      let cost_to_move prev_pos now_pos =
        let cost_opt =
          List.fold_left
            (fun candidate (src, dst, cost) -> 
              if src = prev_pos && dst = now_pos
              then Some cost
              else candidate
            ) None routes in
        match cost_opt with None -> failwith "not found" | Some cost -> cost

      let compare_cost a b = compare a b

        let next_routes current =
          List.fold_left
            (fun next_points (src, dst, cost) ->
              if src = current
              then dst::next_points
              else next_points
            ) [] routes
    end
  )

let _ =
  let resultset = GraphAstar.run S G 0 in
  List.iter
    (fun (src, _, cost) ->
      Printf.printf "src=%s, cost=%s\n"
        (string_of_point src)
        (string_of_int cost)
    ) resultset

