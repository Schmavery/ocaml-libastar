module type RouteType = sig
  type pos
  type cost
  val start : pos
  val goal : pos
  val add_cost: cost -> cost -> cost
  val init_cost : cost
  val cost_to_move : pos -> pos -> cost
  val compare_cost : cost -> cost -> int
  val heuristic : pos -> cost
  val next_routes : pos -> pos list
end

module Make(Route:RouteType) : sig
  val run : unit -> (Route.pos * Route.pos option * Route.cost) list
end = struct
  type node = {pos:Route.pos; cost:Route.cost; score:Route.cost; prev:Route.pos option}

  let score prev now_pos =
    (Route.add_cost
      (Route.add_cost prev.cost (Route.cost_to_move prev.pos now_pos))
      (Route.heuristic now_pos)
    )

  let remove_minimum_score_node nodeset =
    let (minimum_opt, resultset) =
      List.fold_left
        (fun (candidate_opt, resultset) node ->
          match candidate_opt with
          | None -> (Some node, resultset)
          | Some candidate when Route.compare_cost candidate.score node.score > 0 ->
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
      cost=Route.add_cost prev_node.cost (Route.cost_to_move prev_node.pos pos);
      score=score;
      prev=Some prev_node.pos }

  (*
  let string_of_node node =
    Printf.sprintf "pos=%s, cost=%s, score=%s, prev=%s"
      (Route.string_of_pos node.pos)
      (Route.string_of_cost node.cost)
      (Route.string_of_cost node.score)
      (match node.prev with None -> "(----)" | Some prev -> Route.string_of_pos prev)

  let print_nodeset nodeset =
    List.iter (fun node -> print_endline (string_of_node node)) nodeset
  *)

  let sort closeset =
    let rec _sort prev sorted = 
      let node = List.find (fun node -> node.pos = prev) closeset in
      let sorted = node::sorted in
      match node.prev with
      | Some prev -> _sort prev sorted
      | None -> sorted
    in
    _sort Route.goal []

  let result_of_node xs =
    List.map (fun x -> (x.pos, x.prev, x.cost)) xs

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
      if node.pos = Route.goal then result_of_node (sort closeset)
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
            ) openset (Route.next_routes node.pos)
        in
        _run openset closeset
      )
    in
    let start_node = {pos=Route.start; cost=Route.init_cost; score=Route.heuristic Route.start; prev=None} in
    _run [start_node] []
  end
