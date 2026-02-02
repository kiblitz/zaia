open! Core

let machinize ?(lowercase = false) s =
  let s =
    s
    |> String.substr_replace_all ~pattern:"..." ~with_:""
    |> String.substr_replace_all ~pattern:"." ~with_:"_"
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
    |> String.substr_replace_all ~pattern:" " ~with_:"_"
    |> String.filter ~f:(fun c -> Char.is_alphanum c || [%equal: char] c '_')
    |> String.lowercase
  in
  let s = if lowercase then s else String.capitalize s in
  let s = if Char.is_digit (String.nget s 0) then [%string "V%{s}"] else s in
  match s with
  | "initializer" | "function" | "object" | "type" -> [%string "%{s}_"]
  | s -> s
;;
