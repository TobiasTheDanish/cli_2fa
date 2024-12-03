let fps = 30.0

let loop (initial:'a) (render:('a -> float -> unit)) (update: ('a -> float -> 'a)) =
  let state = ref initial in
  let delta = Float.div 1.0 fps in
  let last_t = ref (Unix.gettimeofday ()) in

  while true do 
    let cur_t = Unix.gettimeofday () in
    let frame_delta = Float.sub cur_t !last_t in
    if frame_delta >= delta then (
      render !state frame_delta;
      Stdlib.flush Stdlib.stdout;
      state := update !state frame_delta;
      last_t := cur_t
    )
  done
