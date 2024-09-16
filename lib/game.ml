let _ = Random.self_init ()

let text_size = 20

module Game = struct
  type cell =
    | Dead
    | Alive

  type world = {
    size : int;
    living_cells : int;
    generation : int;
    cell_size : int;
    cells : cell array array
  }

  let init_world size : world =
    { size; living_cells = 0; generation = 0; cell_size = 10; cells = Array.make_matrix size size Dead }
  ;;

  let randomize_world w : world =
    let random_cells =
      w.cells
        |> Array.map (fun line ->
          line |> Array.map (fun _ -> if Random.int 10 > 2 then Alive else Dead))
    in
    { w with cells = random_cells }
  ;;

  let is_neighbor w_size i j x y : bool =
    if (i <> x || j <> y) && i >= 0 && j >= 0 && i < w_size && j < w_size
    then true
    else false

  let count_living_neighbors w x y : int =
    let count = ref 0 in
    for i = x - 1 to x + 1 do
      for j = y - 1 to y + 1 do
        if is_neighbor w.size i j x y
        then if w.cells.(i).(j) = Alive then count := !count + 1
      done
    done;
    !count
  ;;

  let update_cell w x y : cell =
    let living_neighbors = count_living_neighbors w x y in
    match w.cells.(x).(y) with
    | Alive when living_neighbors < 2 -> Dead
    | Alive when living_neighbors > 3 -> Dead
    | Dead when living_neighbors = 3 -> Alive
    | cell -> cell
  ;;

  let update w : world =
    let new_cells = Array.make_matrix w.size w.size Dead in
    let living_neighbors = ref 0 in
    for i = 0 to w.size - 1 do
      for j = 0 to w.size - 1 do
        new_cells.(i).(j) <- update_cell w i j;
        if new_cells.(i).(j) = Alive then living_neighbors := !living_neighbors + 1
      done
    done;
    { w with cells = new_cells; generation = w.generation + 1; living_cells = !living_neighbors }
  ;;

  let rec main_loop (current_world : world) : unit =
    match Raylib.window_should_close () with
    | true -> Raylib.close_window ()
    | false ->
      Raylib.begin_drawing ();
      Raylib.clear_background Raylib.Color.white;

      for i = 0 to current_world.size - 1 do
        for j = 0 to current_world.size - 1 do
          if current_world.cells.(i).(j) = Alive then Raylib.draw_rectangle (i * current_world.cell_size) (j * current_world.cell_size) current_world.cell_size current_world.cell_size Raylib.Color.black
        done;
        Raylib.draw_text (string_of_int current_world.generation) 10 10 text_size Raylib.Color.green;
        Raylib.draw_text (string_of_int current_world.living_cells) 10 30 text_size Raylib.Color.green;
      done;
      Raylib.end_drawing ();
      current_world |> update |> main_loop
  ;;
end

