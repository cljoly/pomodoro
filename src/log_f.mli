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

type sort_of_timer =
  | Pomodoro
  | Short_break
  | Long_break
;;

type timer_sexp = {
  sort : sort_of_timer;
  duration : float;
  (* Command to play sound while pomodoro is running *)
  ticking_command : string;
  (* Command to a play sound when pomodoro is finished *)
  ringing_command : string;
  (* Maximum time the ringing command is allowed to run *)
  max_ring_duration : float;
}
type settings_sexp = {
  timer_cycle : timer_sexp sexp_list
}

type internal_read_log

(* Self refreshing log file *)
class read_log : string -> object
    val mutable internal_read_log : internal_read_log
    method fname : string
    method private irl : internal_read_log
    method ptasks : Tasks.ptask list
    method settings : settings_sexp
  end
;;
