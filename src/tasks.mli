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

Inspired by example of lambda-term library, (c) 2011, Jeremie Dimino <jeremie@dimino.org>

*)

open Core.Std;;

module T = Time;;
module Ts = Time.Span;;

type status = Active | Done
type of_timer = Pomodoro | Short_break | Long_break
class ['a] avl :
  'a ->
  object ('b)
    val mutable actual : 'a
    val log : 'a
    method both : ('a * 'a) option
    method print_both : ('a -> string) -> string
    method get : 'a
    method private get_actual : 'a
    method get_log : 'a
    method set : 'a -> unit
    method turn2log : unit
    method private update_actual : 'a -> unit
    method update_log : 'a -> 'b
  end
class timer :
  float ->
  of_timer ->
  on_finish:('a -> unit) ->
  string ->
  string ->
  string ->
  object ('a)
    val duration : Ts.t
    val mutable marked_finished : bool
    val name : string
    val of_type : of_timer
    val mutable running_meanwhile : Lwt_process.process_none
    val running_when_done : string
    val start_time : T.t
    method private call_on_finish_once : unit
    method cancel : unit
    method is_finished : bool
    method name : string
    method of_type : of_timer
    method remaining : Ts.t option
    method run_done : unit
    method update_running_meanwhile : unit
  end
class ptask :
  ?num:int ->
  name:string ->
  description:string ->
  ?done_at:string ->
  ?number_of_pomodoro:int ->
  ?estimation:int ->
  ?interruption:int ->
  of_timer list ->
  (of_timer -> timer) ->
  object ('a)
    method current_timer : timer
    method description : string avl
    method done_at : string option avl
    method estimation : int option avl
    method interruption : int option avl
    method id : int

    (* XXX Confusing *)
    method record_interruption : unit
    method interrupt : unit

    method is_done : bool
    method long_summary : string
    method mark_done : unit
    method name : string avl
    method num : int option avl
    method number_of_pomodoro : int option avl
    method short_summary : string
    method status : status avl
    method private summary : long:bool -> string
    method update_with : 'a -> 'a
  end
val time_remaining : timer:< remaining : Ts.t option; .. > -> string
val get_pending : ptask list -> ptask option
val on_finish : timer -> unit
