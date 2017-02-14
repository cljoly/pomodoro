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
module Ts = Time.Span;;

(* Timer related code *)

(* Type of timer *)
type of_timer =
    Pomodoro | Short_break | Long_break
;;
(* Return durations associated in log file for each type of timer *)
let durations_and_name log =
  function
    | Pomodoro -> (log.Log_f.settings.pomodoro_duration, "Pomodoro")
    | Short_break -> (log.Log_f.settings.short_break_duration, "Short break")
    | Long_break -> (log.Log_f.settings.long_break_duration, "Long break")
;;

(* When a timer is finished, notify *)
let on_finish timer =
  sprintf "notify-send '%s ended.'" timer#name
  |> Sys.command
  |> ignore;
  timer#run_done
;;

(* A timer of duration (in minute). The on_exit function is called the
 * first time the timer is finished *)
class timer duration of_type ~on_finish name running_meanwhile ?max_done_duration running_when_done =
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
    (* Pretty printing of remaining time *)
    method remaining_str =
      s#remaining |> Option.value ~default:(Ts.create ())
      (* XXX Manual pretty printing *)
      |> Ts.to_parts |> fun { Ts.Parts.hr ; min; sec ; _ } ->
      hr |> function
      | 0 -> sprintf "%i:%i" min sec
      | _ -> sprintf "%i:%i:%i" hr min sec

    method is_finished = Option.is_none s#remaining
    method mark_finished =
      if not marked_finished then marked_finished <- true
    (* Do as if the timer would have start right now *)
    method reset =
      (* Only a Pomodoro timer may be interrupted *)
      if s#of_type = Pomodoro
      then
        {< start_time = T.now () >}
      else
        s

    (* Command running as long as the timer is not finished, launched at
     * instanciation *)
    val mutable running_meanwhile = run_meanwhile ()
    (* Stop and keep running when necessary *)
    method ensure_consistency =
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
      let timeout =
        Option.value ~default:0.4 max_done_duration
      in
      Lwt_process.shell running_when_done
      |> Lwt_process.exec ~timeout
      |> ignore
  end

(* TODO Create a special object for timer that are elleapsed and that do nothing *)
let empty_timer () =
  new timer 0. Short_break ~on_finish:(fun _ -> ()) "Empty" "" ""
;;

class cycling ?cycle ~log =
  (* Simplified instanciation of class timer *)
  let simple_timer of_timer =
    let ticking_command = !log.Log_f.settings.ticking_command in
    let ringing_command = !log.Log_f.settings.ringing_command in
    let (duration, name) = durations_and_name !log of_timer in
    new timer duration of_timer
      ~on_finish:on_finish
      name
      ticking_command
      ringing_command
      ?max_done_duration:!log.settings.max_ring_duration
  in
  (* We do 4 pomodoroes, with a short break between each, before taking a long
   * break *)
  let cycle = (* TODO Allow to configure this *)
    Option.value cycle
      ~default:[ Pomodoro ; Short_break
               ; Pomodoro ; Short_break
               ; Pomodoro ; Short_break
               ; Pomodoro ; Long_break
               ]
  in
  (* lead to problem if cycle is empty, for instance with `position` instance variable *)
  let _ = assert (cycle <> []) in
  let cycle_length = List.length cycle in
  object (s:'s)
    val cycle : of_timer list Avl.t = new Avl.t cycle
    val cycle_length = cycle_length
    val mutable position = -1
    val mutable current_timer = empty_timer ()
    (* Circle through positions *)
    method private next_position =
      position <- (position + 1) mod cycle_length;
      current_timer <- simple_timer (List.nth_exn cycle#get position)
    method get final_call =
      if current_timer#is_finished then begin
        final_call current_timer;
        s#next_position
      end;
      current_timer#ensure_consistency;
      current_timer
    method map_current_timer ~f =
      current_timer <- f current_timer;
      current_timer#ensure_consistency;
  end
;;
