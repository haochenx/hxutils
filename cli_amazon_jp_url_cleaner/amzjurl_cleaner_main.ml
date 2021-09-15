let output =
  let raw =
    if (ArgOptions.has_flag "-n")
    then print_string
    else print_endline
  in
  if (ArgOptions.has_flag "-i")
  then (fun str -> info "cleaned: %s" str; raw str)
  else raw

let () =
  let orig = slurp_stdin() in
  Uri.of_string orig
  |> (fun u ->
    let comps =
      Uri.path u
      |> String.split_on_char '/'
    in
    let rec fa = function
      | (_, "dp" :: rest) -> fa (`dp_found, rest)
      | (`dp_found, dp :: _) -> [ "dp" ; dp]
      | (_, _ :: rest) -> fa (`init, rest)
      | (`dp_found, []) | (`init, []) -> comps in
    let comps =
      match fa (`init, comps) with
      | ("" :: _) as comps -> comps
      | rest -> "" :: rest
    in
    Uri.with_path u
      (String.concat "/" comps)
  )
  |> Fn.flip Uri.with_query []
  |> Uri.to_string
  |> output
