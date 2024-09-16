open Gameoflife.Game

let grid_size = 60
let screen_size = 600

let randomized_world () : Game.world =
  (* Open window *)
  Raylib.init_window screen_size screen_size "Game of Life";
  Raylib.set_target_fps 4;
  Raylib.set_exit_key(Raylib.Key.Q);
  (* Intialize an empty world and randomize it *)
  grid_size |> Game.init_world |> Game.randomize_world
;;

let () = randomized_world () |> Game.main_loop
