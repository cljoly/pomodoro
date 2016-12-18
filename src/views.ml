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

Inspired by example of lambda-term library, (c) 2011, Jeremie Dimino <jeremie@dimino.org>

*)

open Core.Std;;

open Lwt;;
open LTerm_widget;;

(* A view with both task and pomodoro timers *)

let task_timer ~ptasks (main_frame:frame) () =
  let current_task ~default f =
    Tasks.get_pending !ptasks.Log_f.log
    |> Option.value_map ~f ~default
  in
  (* Allow to get remainging time for current ptask, if any one is yet active *)
  let remaining_time () =
    current_task ~default:"Finished"
      (fun ptask ->
         let timer = ptask#current_timer in
         String.concat [ (Tasks.time_remaining ~timer) ; "\n" ; timer#name ])
  in
  let task_summary () =
    current_task ~default:"" (fun ptask -> ptask#long_summary)
  in

  let vbox = new vbox in
  let clock = new label (remaining_time ()) in
  let ptask = new label "" in
  let done_btn = new button "Done" in
  vbox#add ~expand:true clock;
  vbox#add ~expand:true ptask;
  vbox#add ~expand:true done_btn;
  main_frame#set (vbox :> t);

  (* Update the time every second *)
  (Lwt_engine.on_timer Param.tick true
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
    List.iter !ptasks.Log_f.log ~f:(fun ptask ->
        if !display_done_task || not ptask#is_done then
          let task = new label  ptask#short_summary in
          (* TODO Add scroller, improve summary *)
          to_add#add ~expand:true task
      );
    main_frame#set to_add;
  in
  let event_task_list =
    (Lwt_engine.on_timer Param.log_tick true
       (fun _ -> list_task ()))
  in
  vbox#add ~expand:true main_frame;

  (* Add buttons, in a flat box *)
  let hbox = new hbox in
  let pomodoro_btn = new button "Pomodoro" in
  let toggle_done_btn = new button "Toggle done" in
  let exit_btn = new button "Exit" in
  hbox#add ~expand:true pomodoro_btn;
  hbox#add ~expand:true toggle_done_btn;
  hbox#add ~expand:true exit_btn;
  vbox#add ~expand:false hbox;

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

