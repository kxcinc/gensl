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

let unicode_list lst =
  List.filter (fun (k, _) -> ArgOptions.has_flag ("-" ^ k)) lst

let () =
  let categories_list = (generate_html (unicode_list Categories.list)) in
  let proparties_list = (generate_html (unicode_list Properties.list)) in
  if categories_list = "" then ()
  else
    printf "<h1>Categories.list</h1>%s"
      (generate_html (unicode_list Categories.list));
  if proparties_list = "" then ()
  else
    printf "<h1>Properties.list</h1>%s"
      (generate_html (unicode_list Properties.list))
