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
end;;

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
  done_with : int sexp_option (* Number of pomodoro used *)
} [@@deriving sexp]
type log = {
  settings : settings;
  tasks : task_sexp list
} [@@deriving sexp]

(* Interval used by lwt_engine timer *)
let tick = 0.5;;
let log_tick = 3. *. tick;;

(* Some type to describe states of ptasks *)
type status = Active | Done;;
(* Type of timer *)
type of_timer =
    Pomodoro | Short_break | Long_break
;;

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
    if Ts.(compare eleapsed_time duration) < 0
    then Some remaining_time (* Time remaining *)
    else begin
      s#call_on_finish_once;
      None
    end
  method is_finished = Option.is_none s#remaining

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
end;;

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

    (* Returns a summary of the task, short or with more details *)
    method private summary ~long =
      (* f converts to string *)
      let print_both avl ~f = match avl#both with
        | None -> f avl#get
        | Some (log_value, actual_value) ->
            sprintf "%s (log: %s)" (f actual_value) (f log_value)
      in
      (* Identity *) let id = fun a -> a in
      let short_summary = sprintf "%s: %s"
        (print_both ~f:id name)
        (print_both ~f:id description)
      in
      if long
      then
      (* Display only what is needed *)
      [ Some short_summary
      ; Option.map done_at#get
        ~f:(fun _ -> "Done at" ^ (print_both
          ~f:(function None -> "None" | Some date -> date) done_at))
      ; "With " ^ (print_both number_of_pomodoro ~f:Int.to_string) ^ " pomodoro"
        |> Option.some
      ] |> List.filter_map ~f:id
      |> String.concat ~sep:"\n"
      else short_summary

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
  |> ignore;
  timer#run_done
;;

type read_log = { name : string ; log : ptask list };;

(* Read log containing tasks and settings, tries multiple times since user may
 * edit file and lead to temporal removal *)
let read_log filename =
  let rec read_log tries =
    try Sexp.load_sexp_conv_exn filename log_of_sexp
    with exn ->
      Unix.sleep (Int.of_float tick);
      if tries > 0
      then read_log (pred tries)
      else raise exn
  in
  let log = read_log 30 in
  let durations = [
    ( Pomodoro, (log.settings.pomodoro_duration, "Pomodoro") );
    ( Short_break, (log.settings.short_break_duration, "Short break") );
    ( Long_break, (log.settings.long_break_duration, "Long break") )
  ] in
  let simple_timer of_timer = (* Simplified instanciation of class timer *)
    let ticking_command = log.settings.ticking_command in
    let ringing_command = log.settings.ringing_command in
    let (duration, name) = List.Assoc.find_exn durations of_timer in
    new timer duration of_timer ~on_finish name ticking_command ringing_command
  in
  (* We do 4 pomodoroes, with a short break between each, before taking a long
   * break *)
  let cycle = (* TODO Allow to configure this *)
    [ Pomodoro ; Short_break
    ; Pomodoro ; Short_break
    ; Pomodoro ; Short_break
    ; Pomodoro ; Long_break
    ]
  in
  {
    name = filename;
    log = List.mapi log.tasks
        ~f:(fun task_position (task_sexp:task_sexp) ->
            new ptask
              ~num:task_position
              task_sexp.name
              task_sexp.description
              cycle
              simple_timer
              ?done_at:task_sexp.done_at
              (Option.value ~default:0 task_sexp.done_with)
          );
  }
;;

(* Update entries, dropping all tasks in old log file if they are not in the new
 * one and adding those in the new log file, even if they were not in the new one *)
let reread_log r_log =
  let name = r_log.name in (* Name is common to both logs *)
  let old_log = r_log.log in
  let new_log = (read_log name).log in
  let log =
    List.map new_log
      ~f:(fun new_task ->
          List.find_map old_log ~f:(fun old_task ->
              if new_task#id = old_task#id
              then Some (old_task#update_with new_task)
              else None
            )
          |> Option.value ~default:new_task
        )
  in
  { name ; log }
;;

(* A view with both task and pomodoro timers *)
let task_timer ~ptasks (main_frame:frame) () =
  let current_task ~default f =
    get_pending !ptasks.log
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
    current_task ~default:"" (fun ptask -> ptask#long_summary)
  in

  let vbox = new vbox in
  let clock = new label (remaining_time ()) in
  let ptask = new label "" in
  let done_btn = new button "Done" in
  vbox#add clock;
  vbox#add ptask;
  vbox#add done_btn;
  main_frame#set (vbox :> t);

  (* Update the time every second *)
  (Lwt_engine.on_timer tick true
     (fun _ ->
        (* Make sure timer is in a consistent state *)
        current_task ~default:() (fun ct -> ct#current_timer#update_running_meanwhile);
        (* Update display *)
        clock#set_text (remaining_time ());
        ptask#set_text (task_summary ());
        (* XXX Quite heavy *)
        main_frame#set (vbox :> t);
     ))
  |> ignore;

  (* Mark task as finished when done button is pressed *)
  done_btn#on_click
    (fun () -> current_task ~default:() (fun t -> t#mark_done));
;;

(* A view listing tasks *)
let listing ~ptasks () =
  let waiter, wakener = wait () in

  let vbox = new vbox in
  let main_frame = new frame in

  let display_done_task = ref false in

  let list_task () =
    let to_add = new vbox in
    List.iter !ptasks.log ~f:(fun ptask ->
      if !display_done_task || not ptask#is_done then
      let task = new label  ptask#short_summary in
      (* TODO Add scroller, improve summary *)
      to_add#add task
    );
    main_frame#set to_add;
  in
  let event_task_list =
    (Lwt_engine.on_timer log_tick true
       (fun _ -> list_task ()))
  in
  vbox#add main_frame;

  (* Add buttons *)
  let pomodoro_btn = new button "Pomodoro" in
  let toggle_done_btn = new button "Toggle done" in
  let exit_btn = new button "Exit" in
  vbox#add pomodoro_btn;
  vbox#add toggle_done_btn;
  vbox#add exit_btn;

  (* Go to pomodoro view *)
  pomodoro_btn#on_click (fun () ->
    Lwt_engine.stop_event event_task_list;
    task_timer ~ptasks main_frame ();
  );

  (* Show don task or not *)
  toggle_done_btn#on_click (fun () ->
    display_done_task := not !display_done_task;
    list_task ());

  (* Quit when the exit button is clicked *)
  exit_btn#on_click (wakeup wakener);

  (* Run in the standard terminal *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter
;;

let () =
  (* Get timers with command line arguments, mutable to allow easier update *)
  let ptasks = ref (
    Sys.argv |> function
    | [| _ ; name |] -> read_log name
    | _ -> failwith "Needs exactly one argument, filename of your log file."
  ) in
  (* Update log file *)
  (Lwt_engine.on_timer log_tick true
     (fun _ ->
        ptasks := reread_log !ptasks))
  |> ignore;


  Lwt_main.run (listing ~ptasks ())

;;
