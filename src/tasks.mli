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

type status = Active | Done

class ptask :
  ?num:int ->
  name:string ->
  description:string ->
  ?done_at:string ->
  ?number_of_pomodoro:int ->
  ?estimation:int ->
  ?interruption:int ->
  ?day:Date.t ->
  Timer.of_timer list ->
  (Timer.of_timer -> Timer.timer) ->
  object ('a)
    method current_timer : unit -> Timer.timer option
    method description : string Avl.t
    method done_at : string option Avl.t
    method estimation : int option Avl.t
    method interruption : int option Avl.t
    method day : Date.t option Avl.t
    method id : int

    method record_interruption : long:bool -> unit
    method remove_timer : unit
    method attach_timer : unit

    method is_done : bool
    method long_summary : string
    method mark_done : unit
    method name : string Avl.t
    method num : int option Avl.t
    method number_of_pomodoro : int option Avl.t
    method short_summary : string
    method status : status Avl.t
    method private summary : long:bool -> string
    method update_with : 'a -> 'a
  end

val get_pending : ptask list -> ptask option
