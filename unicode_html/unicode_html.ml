open Sedlex_ppx.Unicode
open Kxclib
open Printf
(* open ArgOptions *)


let generate_html lst =
  foldr (fun (name, cset) acc1 ->
      sprintf
        "<table><thead><tr><th>%s</th></tr></thead><tbody>%s</tbody></table>%s"
        name
        (foldr (fun (k, v) acc2 ->
             sprintf "<tr><td>0x%x</td><td>&#%d;</td><td>0x%x</td><td>&#%d;</td></tr>%s"
               k k v v acc2)
            cset "")
        acc1)
    lst ""

let extract_unicode unicode_list args =
  List.filter_map
    (fun arg -> match List.assoc_opt arg unicode_list with
       | None -> None
       | Some l -> Some (arg, l))
    args

let () =
  let categories_list =
    (match ArgOptions.(get_option (StringOption "-categories")) with
     | None -> Categories.list
     | Some args -> extract_unicode
                      Categories.list
                      (String.split_on_char ',' args))
    |> generate_html in
  let properties_list =
    (match ArgOptions.(get_option (StringOption "-properties")) with
     | None -> Properties.list
     | Some args -> extract_unicode
                      Properties.list
                      (String.split_on_char ',' args))
    |> generate_html in
  if categories_list = "" then ()
  else
    printf "<h1>Categories.list</h1>%s" categories_list;
  if properties_list = "" then ()
  else
    printf "<h1>Properties.list</h1>%s" properties_list
