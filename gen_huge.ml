(*
©  Clément Joly, 2017

leo@wzukw.eu.org

This software is a computer program whose purpose is to use Pomodoro method.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-B
license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info".

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-B license and that you accept its terms.

*)

open Core.Std;;

let gen_task i : Sexp.t =
  let open Sexp.O in
  let each n f = Option.some_if ( i mod n = 0 ) (f i) in
  let pseudo_random_date i =
    let open Date in
    add_days (Date.today ~zone:Core.Zone.utc) i
    |> to_string
  in
  let raw_list =
    [ ( "name", Some (sprintf "Task %i" i) )
    ; ( "description", (each 11 ( sprintf "desc %i" )) )
    ; ( "done_at", (each 23 pseudo_random_date) )
    ; ( "done_with", (each 7 Int.to_string) )
    ; ( "estimation", (each 14 Int.to_string) )
    ; ( "short_interruption", (each 2 Int.to_string) )
    ; ( "long_interruption", (each 5 Int.to_string) )
    ; ( "day", (each 3 pseudo_random_date) )
    ]
  in
  (* eprintf "%i\n%!" i; *)
  List (List.filter_map raw_list
          ~f:(fun (field, val_option) -> Option.map val_option
                 ~f:(fun atm -> List [ Atom field ; Atom atm])
             ))
;;

let () =
  let tasks =
    Sexp.List (List.init 50_000 ~f:gen_task)
  in
  Sexp.O.(List [
      List [ Atom "settings" ; List [] ]
    ; List [ Atom "tasks" ; tasks ]
    ])
  |> Sexp.to_string_mach
  |> print_endline
;;
