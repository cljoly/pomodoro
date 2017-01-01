(*
©  Clément Joly, 2016

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
module Ts = Time.Span;;

(* Task plumbery *)

(* Some type to describe states of ptasks *)
type status = Active | Done;;
(* Type of timer *)
type of_timer =
    Pomodoro | Short_break | Long_break
;;

(* Represents actual states and states read from log file
 * (avl stands for actual_vs_log) *)
class ['a] avl read_value = object(s)
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
    Option.some_if (log <> actual) (log, actual)

  (* Returning string with all relevant values *)
  method print_both f =
    (* f converts to string *)
    match s#both with
    | None -> f s#get
    | Some (log_value, actual_value) ->
      sprintf "%s (log: %s)" (f actual_value) (f log_value)
end

(* Create a timer of duration (in minute). The on_exit function is called the
 * first time the timer is finished *)
class timer duration of_type ~on_finish name running_meanwhile running_when_done =
  let run_meanwhile () =
    Lwt_process.shell running_meanwhile
    |> Lwt_process.open_process_none
  in
  object(s)
    val name : string = name
    method name = name
    val duration = Ts.of_min duration
    val start_time = T.now ()
    val mutable marked_finished = false

    val of_type : of_timer = of_type
    method of_type = of_type

    method private call_on_finish_once =
      if not marked_finished
      then begin
        on_finish s;
        marked_finished <- true
      end

    method remaining =
      let now = T.now () in
      let eleapsed_time = T.diff now start_time in
      let remaining_time = Ts.(duration - eleapsed_time) in
      if Ts.(eleapsed_time < duration)
      then Some remaining_time (* Time remaining *)
      else begin
        s#call_on_finish_once;
        None
      end
    method is_finished = Option.is_none s#remaining
    method cancel =
      if not marked_finished then marked_finished <- true

    (* Command running as long as the timer is not finished, launched at
     * instanciation *)
    val mutable running_meanwhile = run_meanwhile ()
    (* Stop and keep running when necessary *)
    method update_running_meanwhile =
      let make_sure_its_running () =
        running_meanwhile#state
        |> function | Lwt_process.Running -> ()
                    | Lwt_process.Exited _ -> running_meanwhile <- run_meanwhile ()
      in
      if s#is_finished
      then running_meanwhile#terminate
      else make_sure_its_running ()

    (* Command to run when finish *)
    val running_when_done = running_when_done
    method run_done =
      Lwt_process.shell running_when_done
      |> Lwt_process.exec ~timeout:4. (* TODO Configure it *)
      |> ignore
  end

let empty_timer () =
  new timer 0. Short_break ~on_finish:(fun _ -> ()) "" "" ""
;;

(* A task (written ptask to void conflict with lwt), like "Learn OCaml". Cycle
 * sets the number and order of timers *)
class ptask
    ?num (* Position in log file, useful to order tasks *)
    name
    description
    cycle
    (simple_timer:(of_timer -> timer))
    ?done_at
    number_of_pomodoro
  =
  let cycle_length = List.length cycle in
  object(s:'s)
    val name : string avl = new avl name
    val description : string avl = new avl description
    method name = name#get
    method description = description#get
    (* Way to identify a task uniquely, XXX based on its name for now *)
    method id = String.hash s#name

    val status =
      new avl (match done_at with Some _ -> Done | None -> Active)
    val done_at = new avl done_at
    method done_at = done_at#get
    method mark_done = status#set Done
    method status = status#get
    method is_done =
      status#get = Done

    val num : int option avl = new avl num
    method num = num#get

    val cycle : of_timer list avl = new avl cycle
    val cycle_length = cycle_length
    (* Position in the cycle, lead to problem if cycle is empty *)
    val mutable position = -1
    val mutable current_timer = empty_timer ()
    val number_of_pomodoro = new avl number_of_pomodoro
    method number_of_pomodoro = number_of_pomodoro#get
    (* Return current timer. Cycles through timers, as one finishes *)
    method current_timer =
      if
        (status#get = Active)
        && current_timer#is_finished
      then begin
        if current_timer#of_type = Pomodoro then
          number_of_pomodoro#set (number_of_pomodoro#get + 1);
        (* Circle through positions *)
        position <- (position + 1) mod cycle_length;
        current_timer <- simple_timer (List.nth_exn cycle#get position);
      end;
      current_timer
    (* Allow to interrupt a task *)
    method interrupt = current_timer#cancel

    (* Returns a summary of the task, short or with more details *)
    method private summary ~long =
      let short_summary = sprintf "%s: \"%s\""
          (name#print_both String.of_string)
          (description#print_both String.of_string)
      in
      let done_at =
        Option.map done_at#get ~f:(fun _ ->
            (done_at#print_both
              (function
                | None -> "(no done date)"
                | Some date -> sprintf "(done at %s)" date)
            ))
      in
      let nb =
        sprintf "%s pomodoro" (number_of_pomodoro#print_both Int.to_string)
      in
      let interruption =
        None (* TODO Implement this *)
      in
      let estimation =
        None (* TODO Implement this *)
      in
      let append_pipe_if_some =
        Option.map ~f:(fun a -> a ^ " |")
      in
      (* Display only what is needed *)
      Option.[
        some_if long "|"
      ; Some short_summary
      ; bind done_at (some_if long)
      ; some_if long "with"
      ; (some_if long nb |> append_pipe_if_some)
      ; (some_if long interruption |> join |> append_pipe_if_some)
      ; (some_if long estimation |> join |> append_pipe_if_some) (* TODO Remove join once implemented *)
      ] |> List.filter_map ~f:(fun a -> a)
      |> String.concat ~sep:" "
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
      assert (another#id = s#id);
      {<
        name = name#update_log another#name |> update_actual;
        description = description#update_log another#description |> update_actual;
        num = num#update_log another#num |> update_actual;
        number_of_pomodoro = number_of_pomodoro#update_log another#number_of_pomodoro;
        (* Updating status, but keeping potentially different date *)
        done_at = done_at#update_log another#done_at;
        status = status#update_log another#status |> update_actual
      >}
  end

(* Pretty printing of remaining time *)
let time_remaining ~timer =
  timer#remaining |> Option.value ~default:(Ts.create ())
  (* XXX Manual pretty printing *)
  |> Ts.to_parts |> fun { Ts.Parts.hr ; min; sec ; _ } ->
  hr |> function
  | 0 -> sprintf "%i:%i" min sec
  | _ -> sprintf "%i:%i:%i" hr min sec
;;

(* Get first ptask not marked as done *)
let rec get_pending = function
  | hd :: tl ->
    if hd#is_done
    then get_pending tl
    else Some hd
  | [] -> None
;;

(* When a timer is finished, notify *)
let on_finish timer =
  sprintf "notify-send '%s ended.'" timer#name
  |> Sys.command
  |> ignore;
  timer#run_done
;;

