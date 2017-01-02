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

(* Tools with log file *)

(* Simple log of pomodoros & tasks, with settings *)
type settings = {
  (* Defaults from pomodoro guide *)
  pomodoro_duration : float;
  short_break_duration : float;
  long_break_duration : float;
  (* Command to play sound while pomodoro is running *)
  ticking_command : string;
  (* Command to play sound when pomodoro is finished *)
  ringing_command : string;
} [@@deriving sexp]
type task_sexp = {
  name : string;
  description : string;
  done_at : string sexp_option; (* date and time iso8601 like 2016-09-10T14:57:25 *)
  done_with : int sexp_option; (* Number of pomodoro used *)
  (* Write down an estimation of the number of needed pomodoro *)
  estimation : int sexp_option;
  interruption : int sexp_option; (* Track interruptions *)
} [@@deriving sexp]
type log = {
  settings : settings;
  tasks : task_sexp list
} [@@deriving sexp]

(* fname stands for filename *)
type read_log = { fname : string ; log : Tasks.ptask list }

(* Read log containing tasks and settings, tries multiple times since user may
 * edit file and lead to temporal removal *)
let read_log filename =
  let rec read_log tries =
    try Sexp.load_sexp_conv_exn filename log_of_sexp
    with exn ->
      Unix.sleep (Int.of_float Param.tick);
      if tries > 0
      then read_log (pred tries)
      else raise exn
  in
  let log = read_log 30 in
  let durations = [
    ( Tasks.Pomodoro, (log.settings.pomodoro_duration, "Pomodoro") );
    ( Tasks.Short_break, (log.settings.short_break_duration, "Short break") );
    ( Tasks.Long_break, (log.settings.long_break_duration, "Long break") )
  ] in
  let simple_timer of_timer = (* Simplified instanciation of class timer *)
    let ticking_command = log.settings.ticking_command in
    let ringing_command = log.settings.ringing_command in
    let (duration, name) = List.Assoc.find_exn durations of_timer in
    new Tasks.timer duration of_timer ~on_finish:Tasks.on_finish name ticking_command ringing_command
  in
  (* We do 4 pomodoroes, with a short break between each, before taking a long
   * break *)
  let cycle = (* TODO Allow to configure this *)
    [ Tasks.Pomodoro ; Tasks.Short_break
    ; Tasks.Pomodoro ; Tasks.Short_break
    ; Tasks.Pomodoro ; Tasks.Short_break
    ; Tasks.Pomodoro ; Tasks.Long_break
    ]
  in
  {
    fname = filename;
    log = List.mapi log.tasks
        ~f:(fun task_position (task_sexp:task_sexp) ->
            new Tasks.ptask
              ~num:task_position
              ~name:task_sexp.name
              ~description:task_sexp.description
              ?done_at:task_sexp.done_at
              ?number_of_pomodoro:task_sexp.done_with
              ?estimation:task_sexp.estimation
              ?interruption:task_sexp.interruption
              cycle
              simple_timer
          );
  }

(* Update entries, dropping all tasks in old log file if they are not in the new
 * one and adding those in the new log file, even if they were not in the new
 * one. Makes sure we stop timers of task going deeper in the list *)
let reread_log r_log =
  (* Disable timer from tasks other than the first one *)
  let disable_all_but_first =
    function
    | [] -> []
    | (_ :: interrupted_tasks) as log ->
      List.iter ~f:(fun timer -> timer#interrupt) interrupted_tasks;
      log
  in
  let fname = r_log.fname in (* Name is common to both logs *)
  let old_log = r_log.log in
  let new_log = (read_log fname).log in
  let log = (* Merge current state and log file *)
    List.map new_log
      ~f:(fun new_task ->
          List.find_map old_log
            ~f:(fun old_task ->
                if new_task#id = old_task#id
                then Some (old_task#update_with new_task)
                else None
              )
          |> Option.value ~default:new_task
        )
    |> disable_all_but_first
  in
  { fname ; log }
;;
