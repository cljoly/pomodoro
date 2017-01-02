(*
©  Clément Joly, 2016-2017

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

class ['a] t ?(compare=compare) read_value = object(s)
  val log : 'a = read_value
  val mutable actual = read_value
  method get_log = log
  method private get_actual = actual
  (* Shorthand *)
  method get = s#get_actual
  (* Update actual value, but keep log as-is, giving a new object if we try to
   * change it *)
  method update_log new_log_value = {< log = new_log_value >}
  method private update_actual updated_value =
    actual <- updated_value
  method set = s#update_actual
  (* Turn the actual state to the log one *)
  method turn2log = s#update_actual s#get_log
  (* Gives both values if they differs *)
  method both =
    Option.some_if ((compare log actual) <> 0) (log, actual)

  (* Returning string with all relevant values *)
  method print_both f =
    (* f converts to string *)
    match s#both with
    | None -> f s#get
    | Some (log_value, actual_value) ->
      sprintf "%s (log: %s)" (f actual_value) (f log_value)
end

