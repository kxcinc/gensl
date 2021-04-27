open Sedlex_ppx.Unicode
open Kxclib
open Printf


let generate_categories_html () =
  foldr (fun (name, cset) acc1 ->
      sprintf
        "<table><thead><tr><th>%s</th></tr></thead><tbody>%s</tbody></table>%s"
        name
        (foldr (fun (k, v) acc2 ->
             sprintf "<tr><td>&#%d;</td><td>&#%d;</td></tr>%s"
               k v acc2)
            cset "")
        acc1)
    Categories.list ""

let generate_properties_html () =
  foldr (fun (name, cset) acc1 ->
      sprintf
        "<table><thead><tr><th>%s</th></tr></thead><tbody>%s</tbody></table>%s"
        name
        (foldr (fun (k, v) acc2 ->
             sprintf "<tr><td>&#%d;</td><td>&#%d;</td></tr>%s"
               k v acc2)
            cset "")
        acc1)
    Properties.list ""


let () =
  printf "<h1>Categories.list</h1>%s<h1>Properties.list</h1>%s"
    (generate_categories_html ())
    (generate_properties_html ())
