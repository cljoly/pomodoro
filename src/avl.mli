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

(* Represents actual states and states read from log file
 * (avl stands for actual_vs_log) *)

(* Create an avl around value. The [cmp_sexp] couple of function maybe be
 * provided as a custom comparator and converter to s-expression,
 * respectively. *)
class ['a] t :
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

