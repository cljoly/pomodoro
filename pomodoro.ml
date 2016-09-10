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

(* Interval used by lwt_engine timer *)
let ticking = 1.0;;

(* Create a timer of duration (in minute). The on_exit function is called the
 * first time the timer is finished *)
class timer duration ~name ~description ~on_finish = object(s)
  val name : string = name
  val description : string = description
  method name = name
  method description = description
  val duration = Ts.of_min duration
  val start_time = T.now ()
  val mutable marked_finished = ref false

  method private call_on_finish_once =
    if not !marked_finished
    then begin
      on_finish s;
      marked_finished := true
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
  method finished = Option.is_none s#remaining
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

(* Get first timer not marked as finished *)
let rec get_pending = function
  | hd :: tl ->
    if hd#finished
    then get_pending tl
    else Some hd
  | [] -> None
;;

(* When a timer is finished, notify *)
let on_finish timer =
  sprintf "notify-send '%s ended. \\n%s'" timer#name timer#description
  |> Sys.command
  |> ignore
;;

(* Function to treat one timer after the other, giving remaning time to show *)
let handle_timers timers =
  get_pending timers
  |> Option.map ~f:(fun timer ->
  if timer#finished
  then begin
    "Finished"
  end else time_remaining ~timer)
;;

let main ~timers () =
  let waiter, wakener = wait () in

  (* Allow to get remainging time for current timer, if any one is yet active *)
  let remaining_time () =
    handle_timers timers
    |> Option.value ~default:"Finished"
  in

  let vbox = new vbox in
  let clock = new label (remaining_time ()) in
  let button = new button "exit" in
  vbox#add clock;
  vbox#add button;

  (* Update the time every second *)
  (Lwt_engine.on_timer ticking true
     (fun _ -> clock#set_text (remaining_time ())))
  |> ignore;

  (* Quit when the exit button is clicked *)
  button#on_click (wakeup wakener);

  (* Run in the standard terminal *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter

let () =
  (* Get timers with command line arguments *)
  let timers =
    Sys.argv |> Array.to_list
    |> fun (_ :: tl) ->
      List.map ~f:Float.of_string tl
      |> List.map ~f:(new timer ~name:"" ~description:"" ~on_finish)
  in

  Lwt_main.run (main ~timers ())

;;
