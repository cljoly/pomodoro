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

open Lwt_react
open Lwt
open LTerm_widget

module T = Time;;
module Ts = Time.Span;;


(* Create a timer of duration (in minute) *)
class timer duration = object
  val duration = Ts.of_min duration
  val start_time = T.now ()

  method remaining =
    let now = T.now () in
    let eleapsed_time = T.diff now start_time in
    let remaining_time = Ts.(duration - eleapsed_time) in
    if Ts.(compare eleapsed_time duration) < 0
      then Some remaining_time (* Time remaining *)
      else None
end;;

let time_remaining ~timer =
  timer#remaining |> Option.value ~default:(Ts.create ())
  (* XXX Manual pretty printing *)
  |> Ts.to_parts |> fun { Ts.Parts.hr ; min; sec } ->
      hr |> function
        | 0 -> sprintf "%i:%i" min sec
        | _ -> sprintf "%i:%i:%i" hr min sec
;;

(*
let get_time () =
  let localtime = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02u:%02u:%02u"
    localtime.Unix.tm_hour
    localtime.Unix.tm_min
    localtime.Unix.tm_sec
*)

let main ~timer () =
  let waiter, wakener = wait () in

  let vbox = new vbox in
  let clock = new label (time_remaining ~timer) in
  let button = new button "exit" in
  vbox#add clock;
  vbox#add button;

  (* Update the time every second. *)
    (Lwt_engine.on_timer 1.0 true
      (fun _ -> clock#set_text (
        match timer#remaining with
        None ->
          Sys.command  "notify-send \"Pomodoro ended, take a break\"" |> ignore;
          "Finished"
        | Some _ -> time_remaining ~timer))) |> ignore;

  (* Quit when the exit button is clicked. *)
  button#on_click (wakeup wakener);

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter

let () =
  (* Get timers with command line arguments *)
  let timers =
    Sys.argv |> Array.to_list
    |> fun (_ :: tl) ->
    List.map ~f:Float.of_string tl
    |> List.map ~f:(new timer)
  in

  (* TODO Use other arguments *)
  let timer::_ = timers in

  Lwt_main.run (main ~timer ())

