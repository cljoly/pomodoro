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

module T = Time;;

(* Task plumbery *)

(* Some type to describe states of ptasks *)
type status = Active | Done;;

(* A task (written ptask to avoid conflict with lwt), like "Learn OCaml". Cycle
 * sets the number and order of timers *)
class ptask
    ?num (* Position in log file, useful to order tasks *)
    ?done_at
    ?number_of_pomodoro
    ?estimation
    ?short_interruption
    ?long_interruption
    ?day
    ~name
    ~description
  =
  object(s:'s)
    val name : string Avl.t = new Avl.t name
    val description : string Avl.t = new Avl.t description
    method name = name
    method description = description
    (* Way to identify a task uniquely, XXX based on its name for now *)
    method id = String.hash s#name#get

    val status =
      new Avl.t (match done_at with Some _ -> Done | None -> Active)
    val done_at = new Avl.t done_at
    method done_at = done_at
    method mark_done =
      done_at#set T.(now () |> to_string |> Option.some);
      status#set Done
    method status = status
    method is_done =
      status#get = Done

    val num : int option Avl.t = new Avl.t num
    method num = num

    val number_of_pomodoro : int option Avl.t = new Avl.t number_of_pomodoro
    (* Record one more pomodoro *)
    method record_pomodoro =
      number_of_pomodoro#set
        (Option.value ~default:0 number_of_pomodoro#get
         |> (fun nop -> nop + 1)
         |> Option.some);

    val short_interruption : int option Avl.t = new Avl.t short_interruption
    method short_interruption = short_interruption
    (* Record a short interruption *)
    method record_short_interruption =
      short_interruption#set
        (Some (Option.value_map ~default:(0+1) ~f:succ short_interruption#get));


    val long_interruption : int option Avl.t = new Avl.t long_interruption
    method long_interruption = long_interruption
    (* Record an interruption. Timer should be reset since it is a long
    interruption *)
    method record_long_interruption =
      long_interruption#set
        (Some (Option.value_map ~default:(0+1) ~f:succ long_interruption#get));

    val day : Date.t option Avl.t = new Avl.t day
    method day = day

    val estimation : int option Avl.t = new Avl.t estimation
    method estimation = estimation

    method number_of_pomodoro = number_of_pomodoro

    (* Returns a summary of the task, short or with more details *)
    method private summary ~long =
      let short_summary = sprintf "%s: \"%s\""
          (name#print_both String.of_string)
          (description#print_both String.of_string)
      in
      let done_at =
        done_at#print_both (Option.value_map~default:"(no done date)"
                              ~f: (fun date -> sprintf "(done at %s)" date))
      in
      let nb =
        sprintf "with %s pomodoro" (number_of_pomodoro#print_both
                                      (Option.value_map ~f:Int.to_string ~default:"0"))
      in
      let interruptions =
        sprintf "short interruption: %s; long interruption: %s"
          (short_interruption#print_both
             (Option.value_map ~default:"0" ~f:Int.to_string))
          (long_interruption#print_both
             (Option.value_map ~default:"0" ~f:Int.to_string))
      in
      let estimation =
        sprintf "estimation: %s"
          (estimation#print_both
             (Option.value_map ~default:"0" ~f:Int.to_string))
      in
      (* Display only what is needed *)
      Option.[
        Some short_summary
      ; (some_if long done_at)
      ; (some_if long nb)
      ; (some_if long interruptions)
      ; (some_if long estimation)
      ] |> List.filter_map ~f:(fun a -> a)
      |> String.concat ~sep:", "
    method short_summary = s#summary ~long:false
    method long_summary = s#summary ~long:true


    (* Update a task with data of an other, provided they have the same ids.
     * Keeps timer running, since they are kept as-is. Updates states when it
     * makes sens *)
    method update_with (another:'s) =
      let update_actual avl =
        avl#turn2log;
        avl
      in
      let clever_status_update =
        match status#get, another#status#get with
        | Done, Active -> status#update_log another#status#get
        | Active, Done | Done, Done | Active, Active ->
          (* Make sure current state is the log one *)
          status#update_log another#status#get |> update_actual
      in
      assert (another#id = s#id);
      {<
        status = clever_status_update;
        name = name#update_log another#name#get |> update_actual;
        description = description#update_log another#description#get |> update_actual;
        num = num#update_log another#num#get |> update_actual;
        number_of_pomodoro = number_of_pomodoro#update_log another#number_of_pomodoro#get;
        done_at = done_at#update_log another#done_at#get;
        short_interruption = short_interruption#update_log another#short_interruption#get;
        long_interruption = long_interruption#update_log another#long_interruption#get;
        day = day#update_log another#day#get |> update_actual;
        estimation = estimation#update_log another#estimation#get;
      >}
  end

(* Get first ptask not marked as done *)
let rec get_pending = function
  | hd :: tl ->
    if hd#is_done
    then get_pending tl
    else Some hd
  | [] -> None
;;

