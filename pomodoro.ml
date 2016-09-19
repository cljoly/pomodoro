(*
 * clock.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>,
 *                 2016, Leo Wzukw <leowzukw@vmail.me>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)
open Core.Std;;

open Lwt_react;;
open Lwt;;
open LTerm_widget;;

module T = Time;;
module Ts = Time.Span;;

(* Simple log of pomodoros & tasks, with settings *)
type settings = {
  (* Defaults from pomodoro guide *)
  pomodoro_duration : float;
  short_break_duration : float;
  long_break_duration : float
} [@@deriving sexp]
type task_sexp = {
  name : string;
  description : string;
  done_at : string sexp_option; (* date and time iso8601 like 2016-09-10T14:57:25 *)
  done_with : int sexp_option (* Number of pomodoro used *)
} [@@deriving sexp]
type log = {
  settings : settings;
  tasks : task_sexp list
} [@@deriving sexp]

(* Interval used by lwt_engine timer *)
let ticking = 0.5;;

(* Some type to describe states of ptasks *)
type status = Active | Done;;
(* Type of timer *)
type of_timer =
    Pomodoro | Short_break | Long_break
;;

(* Create a timer of duration (in minute). The on_exit function is called the
 * first time the timer is finished *)
class timer duration of_type ~on_finish name = object(s)
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
    if Ts.(compare eleapsed_time duration) < 0
    then Some remaining_time (* Time remaining *)
    else begin
      s#call_on_finish_once;
      None
    end
  method is_finished = Option.is_none s#remaining
end;;

let empty_timer () =
  new timer 0. Short_break ~on_finish:(fun _ -> ()) ""
;;

(* A task (written ptask to void conflict with lwt), like "Learn OCaml". Cycle
 * sets the number and order of timers *)
class ptask
    name
    description
    cycle
    (simple_timer:(of_timer -> timer))
    ?done_at
    number_of_pomodoro
  =
  let cycle_length = List.length cycle in
  object(s)
    val name : string = name
    val description : string = description
    method name = name
    method description = description

    val mutable status = match done_at with Some _ -> Done | None -> Active
    val done_at = Option.value ~default:"" done_at
    method mark_done = status <- Done
    method is_done = status = Done

    val cycle : of_timer list = cycle
    val cycle_length = cycle_length
    (* Posiition in the cycle, lead to problem if cycle is empty *)
    val mutable position = -1
    val mutable current_timer = empty_timer ()
    val mutable number_of_pomodoro = number_of_pomodoro
    (* Return current timer. Cycles through timers, as one finishes *)
    method current_timer =
      if
        (status = Active)
        && current_timer#is_finished
      then begin
        if current_timer#of_type = Pomodoro
        then
          number_of_pomodoro <- number_of_pomodoro + 1;
        (* Circle through positions *)
        position <- (position + 1) mod cycle_length;
        current_timer <- simple_timer (List.nth_exn cycle position);
      end;
      current_timer

    method summary = sprintf "%s: %s\nPomodoro: %i" name description
        number_of_pomodoro
  end;;

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
  |> ignore
;;

(* Read log containg tasks and settings *)
let read_log filename =
  let log = Sexp.load_sexp_conv_exn filename log_of_sexp in
  let durations = [
    ( Pomodoro, (log.settings.pomodoro_duration, "Pomodoro") );
    ( Short_break, (log.settings.short_break_duration, "Short break") );
    ( Long_break, (log.settings.long_break_duration, "Long break") )
  ] in
  let simple_timer of_timer = (* Simplified instanciation of class timer *)
    let (duration, name) = List.Assoc.find_exn durations of_timer in
    new timer duration of_timer ~on_finish name
  in
  (* We do 4 pomodoroes, with a short break between each, before taking a long
   * break *)
  let cycle = (* TODO Allow to configure this *)
    [ Pomodoro ; Short_break
    ; Pomodoro ; Short_break
    ; Pomodoro ; Short_break
    ; Pomodoro ; Long_break
    ] in
  List.map log.tasks ~f:(fun (task_sexp:task_sexp) ->
      new ptask
        task_sexp.name
        task_sexp.description
        cycle
        simple_timer
        ?done_at:task_sexp.done_at
        (Option.value ~default:0 task_sexp.done_with)
    )
;;

(* A view with both task and pomodoro timers *)
let task_timer ~ptasks () =
  let waiter, wakener = wait () in

  let current_task ~default f =
    get_pending ptasks
    |> Option.value_map ~f ~default
  in
  (* Allow to get remainging time for current ptask, if any one is yet active *)
  let remaining_time () =
    current_task ~default:"Finished"
      (fun ptask ->
         let timer = ptask#current_timer in
         String.concat [ (time_remaining ~timer) ; "\n" ; timer#name ])
  in
  let task_summary () =
    current_task ~default:"" (fun ptask -> ptask#summary)
  in

  let vbox = new vbox in
  let clock = new label (remaining_time ()) in
  let ptask = new label "" in
  let done_btn = new button "Done" in
  let exit_btn = new button "Exit" in
  vbox#add clock;
  vbox#add ptask;
  vbox#add done_btn;
  vbox#add exit_btn;

  (* Update the time every second *)
  (Lwt_engine.on_timer ticking true
     (fun _ ->
        clock#set_text (remaining_time ());
        ptask#set_text (task_summary ())
     ))
  |> ignore;

  (* Mark task as finished when done button is pressed *)
  done_btn#on_click
    (fun () -> current_task ~default:() (fun t -> t#mark_done));
  (* Quit when the exit button is clicked *)
  exit_btn#on_click (wakeup wakener);

  (* Run in the standard terminal *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter
;;

(* A view listing tasks *)
let listing ~ptasks () =
  let waiter, wakener = wait () in

  let vbox = new vbox in

  (* List tasks *)
  List.iter ptasks ~f:(fun ptask ->
    if not ptask#is_done then
    let task = new label  ptask#summary in
    (* TODO Add scroller, improve summary *)
    vbox#add task
  );

  (* Add buttons *)
  let pomodoro_btn = new button "Pomodoro" in
  let exit_btn = new button "Exit" in
  vbox#add pomodoro_btn;
  vbox#add exit_btn;

  (* Go to pomodoro view *)
  pomodoro_btn#on_click (fun () ->
    task_timer ~ptasks () |> ignore);

  (* Quit when the exit button is clicked *)
  exit_btn#on_click (wakeup wakener);

  (* Run in the standard terminal *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter
;;

let () =
  (* Get timers with command line arguments *)
  let ptasks =
    Sys.argv |> function
    | [| _ ; name |] -> read_log name
    | _ -> failwith "Needs exactly one argument, filename of your log file."
  in

  Lwt_main.run (listing ~ptasks ())

;;
