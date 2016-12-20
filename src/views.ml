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
open LTerm_geom;;

(* A view with both task and pomodoro timers *)

(* Scrollable list of tasks *)
class scrollable_task_list ~ptasks (scroll : scrollable) =
  let log () = !ptasks.Log_f.log in
  object
  inherit t "task_list"

  initializer scroll#set_range (List.length (log ()))

  method! can_focus = false

  method! draw ctx _ =
    let log = log () in
    let offset = scroll#offset in
    let { rows ; _ } = LTerm_draw.size ctx in
    let draw_nth_task ~n =
      (* Should not be out of range since offset is set to length of task list *)
      List.nth log (n+offset)
      |> (function
        | Some task -> task#short_summary
        | None -> "" (* Out of range, print blank line *))
      |> LTerm_draw.draw_string ctx n 0
    in
    for row=0 to rows-1 do
      draw_nth_task ~n:row
    done
end;;

(* Place scrollable task list *)
let  add_scroll_task_list ~ptasks (box : box) =
  let adj = new scrollable in
  let scroll = new vscrollbar adj in
  let task_list = new scrollable_task_list ~ptasks adj in
  box#add ~expand:true task_list;
  box#add ~expand:false scroll;
  adj#on_offset_change (fun _ -> scroll#queue_draw);
  (task_list, scroll, adj)
;;

(* Place pomodoro timer *)
let add_pomodoro_timer ~ptasks (box:box) =
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
  box#add ~expand:false vbox;

  (* Update the time on every tick *)
  (Lwt_engine.on_timer Param.tick true
     (fun _ ->
        (* Make sure timer is in a consistent state *)
        current_task ~default:() (fun ct -> ct#current_timer#update_running_meanwhile);
        (* Update display *)
        clock#set_text (remaining_time ());
        ptask#set_text (task_summary ());
     ))
  |> ignore;

  (* Mark task as finished when done button is pressed *)
  done_btn#on_click
    (fun () -> current_task ~default:() (fun t -> t#mark_done));
;;

(* Add buttons, in an horizontal box *)
let add_bottom_btn ~(main:vbox) ~(adj:scrollable) ~wakener display_done_task =
  let hbox = new hbox in
  (* Scroll down *)
  let down_btn = new button "Down" in
  down_btn#on_click (fun () -> adj#set_offset (adj#offset+1););
  (* Scroll up *)
  let up_btn = new button "Up" in
  up_btn#on_click (fun () -> adj#set_offset (adj#offset-1););
  (* Show done task or not *)
  let toggle_done_btn = new button "Toggle done" in
  toggle_done_btn#on_click (fun () ->
      display_done_task := not !display_done_task;);
  let exit_btn = new button "Exit" in
  exit_btn#on_click (fun () -> wakeup wakener ());

  hbox#add ~expand:true down_btn;
  hbox#add ~expand:true up_btn;
  hbox#add ~expand:true toggle_done_btn;
  hbox#add ~expand:true exit_btn;
  main#add ~expand:false hbox;
;;

(* Main view *)
let mainv ~ptasks () =
  let waiter, wakener = wait () in
  let main = new vbox in
  let display_done_task = ref false in

  add_pomodoro_timer ~ptasks main;
  let ( _, _, adj) = add_scroll_task_list ~ptasks main in
  add_bottom_btn ~main ~adj ~wakener display_done_task;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
    Lwt.finalize
    (fun () -> run term main waiter)
    (fun () -> LTerm.disable_mouse term )
;;

