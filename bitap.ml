exception Found

let match_Threshold = ref 0.2
let match_Distance = ref 1000


(* find_from without exception *)
let find_from str ofs sub = 
  let sublen = String.length sub in
    if sublen = 0 then ofs
    else
      let len = String.length str in
	if len = 0 then -1 else
	if 0 > ofs || ofs >= len then raise (Invalid_argument "index out of bounds")
	else begin
		let found = ref false in
		let result = ref (-1) in
	    for i = ofs to len - sublen do
	      let j = ref 0 in
	      while not !found && String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
		    incr j;
		    if !j = sublen then begin
		    	result := i;
		    	found := true
		    end
	      done;
	    done;
	    !result
	end

let exists str sub = find_from str 0 sub >= 0

(* rfind_from without exception *)
let rfind_from str suf sub = 
  let sublen = String.length sub 
  and len    = String.length str in
    if sublen = 0 then len
    else
      if len = 0 then -1 else
	if 0 > suf || suf >= len then raise (Invalid_argument "index out of bounds")
	else begin
	  let result = ref (-1) in
	  let found = ref false in
  	  for i = suf - sublen + 1 downto 0 do
	    let j = ref 0 in
	      while not !found && String.unsafe_get str ( i + !j ) = String.unsafe_get sub !j do
		    incr j;
		    if !j = sublen then begin
		    	result := i;
		    	found := true
		    end
	      done;
	  done;
	  !result
	end

let match_bitapScore e x loc pattern =
  let accuracy = float_of_int e /. float_of_int (String.length pattern) in
  let proximity = abs (loc - x) in
  if !match_Distance = 0 then
    if proximity = 0 then accuracy else 1.0
  else
    accuracy +. float_of_int proximity /. float_of_int !match_Distance

let match_alphabet pattern =
	let s = Hashtbl.create 32 in
	for i = 0 to String.length pattern - 1 do
	    let c = pattern.[i] in
	    let value = (try Hashtbl.find s c with Not_found -> 0)
	                lor (1 lsl (String.length pattern - i - 1))
	    in
	    Hashtbl.replace s c value
    done;
    s

let match_bitap text pattern loc =
      let s = match_alphabet pattern in
      let score_threshold = ref !match_Threshold in
      let best_loc = ref (find_from text loc pattern) in
      if !best_loc <> -1 then begin
      	score_threshold := min (match_bitapScore 0 !best_loc loc pattern) !score_threshold;
      	best_loc := rfind_from text (min (loc + String.length pattern) (String.length text)) pattern;
        if !best_loc <> -1 then
          score_threshold := min (match_bitapScore 0 !best_loc loc pattern) !score_threshold
      end;
      let matchmask = 1 lsl (String.length pattern - 1) in
      best_loc := -1;
      let bin_min = ref 0 in
      let bin_mid = ref 0 in
      let bin_max = ref (String.length pattern + String.length text) in
      let last_rd = ref (Array.make 0 0) in
      (try
      for d = 0 to String.length pattern - 1 do
        bin_min := 0;
        bin_mid := !bin_max;
        while !bin_min < !bin_mid do
          if match_bitapScore d (loc + !bin_mid) loc pattern <= !score_threshold then
            bin_min := !bin_mid
          else
            bin_max := !bin_mid;
          bin_mid := (!bin_max - !bin_min) / 2 + !bin_min;
        done;
        bin_max := !bin_mid;
        let start = ref (max 1 (loc - !bin_mid + 1)) in
        let finish = min (loc + !bin_mid) (String.length text) + String.length pattern in
        let rd = Array.make (finish+2) 0 in
        rd.(finish + 1) <- (1 lsl d) - 1;
        (try
          for j = finish downto !start do
            let charMatch =
              if String.length text <= j - 1 || not (Hashtbl.mem s text.[j - 1]) then
                0
             else 
              Hashtbl.find s text.[j-1]
            in
            if d = 0 then
              rd.(j) <- ((rd.(j + 1) lsl 1) lor 1) land charMatch
            else
              rd.(j) <- ((rd.(j + 1) lsl 1) lor 1) land charMatch lor ((((!last_rd.(j + 1) lor !last_rd.(j)) lsl 1) lor 1) lor !last_rd.(j + 1));
            if rd.(j) land matchmask <> 0 then begin
              let score = match_bitapScore d (j - 1) loc pattern in
              if score <= !score_threshold then begin
                score_threshold := score;
                best_loc := j - 1;
                if !best_loc > loc then
                  start := max 1 (2 * loc - !best_loc)
                else
                  raise Found
              end
            end
          done
        with
        | Found -> ());
        if (match_bitapScore (d + 1) loc loc pattern) > !score_threshold then
          raise Found;
        last_rd := rd
      done
      with
      | Found -> ());
      !best_loc

let bitap_exists text pattern = (match_bitap text pattern 0) >= 0

let test () =
  match_Distance := 100;
  match_Threshold := 0.5;
  Printf.printf "%d = 5\n" (match_bitap "abcdefghijk" "fgh" 5);
  Printf.printf "%d = 5\n" (match_bitap "abcdefghijk" "fgh" 0);
  Printf.printf "%d = 4\n" (match_bitap "abcdefghijk" "efxhi" 0);
  Printf.printf "%d = 2\n" (match_bitap "abcdefghijk" "cdefxyhijk" 5);
  Printf.printf "%d = -1\n" (match_bitap "abcdefghijk" "bxy" 1);
  Printf.printf "%d = 2\n" (match_bitap "123456789xx0" "3456789x0" 2);
  Printf.printf "%d = 0\n" (match_bitap "abcdef" "xxabc" 4);
  Printf.printf "%d = 3\n" (match_bitap "abcdef" "defyy" 4);
  Printf.printf "%d = 0\n" (match_bitap "abcdef" "xabcdefy" 0);
  match_Threshold := 0.4;
  Printf.printf "%d = 4\n" (match_bitap "abcdefghijk" "efxyhi" 1);
  match_Threshold := 0.3;
  Printf.printf "%d = -1\n" (match_bitap "abcdefghijk" "efxyhi" 1);
  match_Threshold := 0.0;
  Printf.printf "%d = 1\n" (match_bitap "abcdefghijk" "bcdef" 1);
  match_Threshold := 0.5;
  Printf.printf "%d = 0\n" (match_bitap "abcdexyzabcde" "abccde" 3);
  Printf.printf "%d = 8\n" (match_bitap "abcdexyzabcde" "abccde" 5);
  match_Distance := 10;
  Printf.printf "%d = -1\n" (match_bitap "abcdefghijklmnopqrstuvwxyz" "abcdefg" 24);
  Printf.printf "%d = 0\n" (match_bitap "abcdefghijklmnopqrstuvwxyz" "abcdxxefg" 1);
  match_Distance := 1000;
  Printf.printf "%d = 0\n" (match_bitap "abcdefghijklmnopqrstuvwxyz" "abcdefg" 24)