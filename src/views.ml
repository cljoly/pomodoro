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

Inspired by example of lambda-term library, (c) 2011, Jeremie Dimino <jeremie@dimino.org>

*)

open Core.Std;;

open Lwt;;
open LTerm_widget;;
open LTerm_geom;;

(* A view with both task and pomodoro timers *)

(* Scrollable list of tasks *)
class scrollable_task_list ~log (scroll : scrollable) display_task =
  let ptasks () = !log.Log_f.ptasks in
  object
    inherit t "task_list"

    initializer scroll#set_range (List.length (ptasks ()))

    method! can_focus = false

    method! draw ctx _ =
      let { rows ; cols } = LTerm_draw.size ctx in
      (* Return elements of list [l] between [top] (of the screen) and (top+row)
       * (corresponding to the last line of the screen) *)
      let select_task top l =
        let ll = List.length l in
        (* Reset offset to last line and redraw *)
        let reset_offset_redraw () =
          scroll#set_offset ~trigger_callback:true (ll-1);
          List.last l |> Option.to_list
        in
        let top = Int.max 0 top in
        let bottom = Int.min (top + rows) ll in
        match List.slice l top bottom with
        (* Appends when we are at the end of the list l *)
        | exception (Invalid_argument _) -> reset_offset_redraw ()
        | [] ->  reset_offset_redraw ()
        (* Not at the end *)
        | l -> l
      in
      (* Return a string creating a table of tasks. Return a message for user if
         the windows to small *)
      let draw_table_of_task (task_list: Tasks.ptask list) =
        let open Textutils.Ascii_table in
        (* String of option *)
        let soo f = Option.value_map ~default:"" ~f in
        try
          to_string ~bars:`Unicode ~display:Display.line ~limit_width_to:cols
            Column.[
              create "Summary" ~align:Align.Right
                (fun t -> t#short_summary)
            ; create "Done" ~align:Align.Center
                (fun t ->
                   t#status#print_both
                     (function Tasks.Done -> "X" | Tasks.Active -> " "))
            ; create "with" ~align:Align.Center
                (fun t -> t#number_of_pomodoro#print_both (soo Int.to_string))
            ; create "at" ~align:Align.Center
                (fun t -> t#done_at#print_both (soo String.to_string))
            ; create "Short interruption" ~align:Align.Center
                (fun t -> t#short_interruption#print_both (soo Int.to_string))
            ; create "Long interruption" ~align:Align.Center
                (fun t -> t#long_interruption#print_both (soo Int.to_string))
            ; create "Estimation" ~align:Align.Center
                (fun t -> t#estimation#print_both (soo Int.to_string))
            ; create "Day" ~align:Align.Center
                (fun t -> t#day#print_both (soo Date.to_string))
            ]
            task_list
        with
          exn ->
          sprintf "Window is likely to be too small.\n%s" (Exn.to_string exn)
      in
      let offset = scroll#offset in
      ptasks ()
      |> List.filter ~f:display_task
      |> select_task offset
      |> draw_table_of_task
      |> LTerm_draw.draw_string ctx 0 0
  end;;

(* Place scrollable task list *)
let add_scroll_task_list ~log (box : box) display_task =
  let adj = new scrollable in
  let scroll = new vscrollbar adj in
  let task_list = new scrollable_task_list ~log adj display_task in
  box#add ~expand:true task_list;
  box#add ~expand:false scroll;
  adj#on_offset_change (fun _ -> scroll#queue_draw);
  (task_list, scroll, adj)
;;

(* Place the pomodoro timer *)
let add_pomodoro_timer ~log (box:box) =
  let current_task ~default f =
    Tasks.get_pending !log.Log_f.ptasks
    |> Option.value_map ~f ~default
  in
  let timer_cycle : Timer.cycling =
    new Timer.cycling ~log
      ?cycle:None
  in
  let mark_pomodoro_done = (fun timer ->
      match timer#of_type with
      | Timer.Pomodoro -> current_task ~default:()
                            (fun task -> task#record_pomodoro)
      | _ -> ()
    )
  in
  (* Allow to get remaining time *)
  let remaining_time () =
    let timer =
      timer_cycle#get mark_pomodoro_done
    in
    String.concat [ timer#name ; "\n" ; timer#remaining_str ]
  in
  let task_summary () =
    current_task ~default:"" (fun ptask -> ptask#long_summary)
  in

  let vbox = new vbox in
  let clock = new label (remaining_time ()) in
  let ptask = new label "" in
  let btn_hbox = new hbox in
  let linter_btn = new button "Long interruption" in
  let sinter_btn = new button "Short interruption" in
  let done_btn = new button "Done" in

  (* Update displayed time on every tick *)
  (Lwt_engine.on_timer Param.tick true
     (fun _ ->
        clock#set_text (remaining_time ());
        ptask#set_text (task_summary ());
     ))
  |> ignore;

  (* Record interruptions *)
  linter_btn#on_click
    (fun () -> current_task ~default:()
        (fun t ->
           t#record_long_interruption;
           timer_cycle#map_current_timer ~f:(fun timer -> timer#reset)));
  sinter_btn#on_click
    (fun () -> current_task ~default:() (fun t -> t#record_short_interruption));

  (* Mark task as finished when done button is pressed *)
  done_btn#on_click
    (fun () -> current_task ~default:() (fun t -> t#mark_done));

  btn_hbox#add ~expand:true linter_btn;
  btn_hbox#add ~expand:true sinter_btn;
  btn_hbox#add ~expand:true done_btn;
  vbox#add ~expand:true clock;
  vbox#add ~expand:true ptask;
  vbox#add btn_hbox ~expand:true;
  box#add ~expand:false vbox;
;;

(* Add buttons, in an horizontal box *)
let add_bottom_btn
    ~(main:vbox)
    ~(adj:scrollable)
    ~wakener
    ~log
    display_done_task
    display_only_day
    current_day
  =
  let day_hbox = new hbox in
  let current_day_label = new label "" in
  let set_day_to target_date =
    current_day := target_date;
    Date.to_string !current_day
    |> sprintf "Current day: %s"
    |> current_day_label#set_text
  in
  set_day_to !current_day;
  (* Show only todays task for the current day *)
  let toggle_day_btn = new button "Filter by day" in
  toggle_day_btn#on_click (fun () -> display_only_day := not !display_only_day);
  let last_day_btn = new button "Last day" in
  last_day_btn#on_click (fun () ->
      Date.add_days !current_day (-1) |> set_day_to
    );
  let next_day_btn = new button "Next day" in
  next_day_btn#on_click (fun () ->
      Date.add_days !current_day 1 |> set_day_to
    );
  (* Get the date in log file which is the narrowest before or after
  current_day, if any *)
  let get_narrowest before_or_after =
    let right_order a b c_option =
      let open Date in
      match before_or_after with
      | `After -> (* We want a < b < c *)
        a < b
        && (Option.is_none c_option
            || Option.value_exn c_option |> (fun c -> b < c))
      | `Before -> (* We want c < b < a *)
        a > b
        && (Option.is_none c_option
            || Option.value_exn c_option |> (fun c -> b > c))
    in
    List.filter_map !log.Log_f.ptasks ~f:(fun ptask -> ptask#day#get)
    |> List.fold ~init:None ~f:(fun best_found candidate ->
        if right_order !current_day candidate best_found
        then Some candidate
        else best_found
      )
  in
  let last_day_in_list () =
    get_narrowest `Before
    |> Option.iter ~f:set_day_to
  in
  let next_day_in_list () =
    get_narrowest `After
    |> Option.iter ~f:set_day_to
  in
  (* Last day present in a task of the log file *)
  let last_log_day_btn = new button "Last log day" in
  last_log_day_btn#on_click last_day_in_list;
  (* Next day present in a task of the log file *)
  let next_log_day_btn = new button "Next log day" in
  next_log_day_btn#on_click next_day_in_list;

  day_hbox#add ~expand:true current_day_label;
  day_hbox#add ~expand:true toggle_day_btn;
  day_hbox#add ~expand:true last_day_btn;
  day_hbox#add ~expand:true next_day_btn;
  day_hbox#add ~expand:true last_log_day_btn;
  day_hbox#add ~expand:true next_log_day_btn;
  main#add ~expand:false day_hbox;

  let hbox = new hbox in
  (* Scroll down *)
  let down_btn = new button "Down" in
  down_btn#on_click (fun () -> adj#set_offset (adj#offset+1));
  (* Scroll up *)
  let up_btn = new button "Up" in
  up_btn#on_click (fun () -> adj#set_offset (adj#offset-1));
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
let mainv ~log () =
  let waiter, wakener = wait () in
  let main = new vbox in
  let display_done_task = ref false in
  let display_only_day = ref false in
  (* Day for which tasks are considered *)
  let current_day = ref (Date.today ~zone:Core.Zone.local) in

  (* Whether a task should be displayed or not, on done and date criteria *)
  let display_task task =
    (!display_done_task || not task#is_done)
    &&
    (not !display_only_day ||
     Option.value_map ~default:false task#day#get
       ~f:(fun day ->
           Date.equal !current_day day)
    )
  in

  add_pomodoro_timer ~log main;
  main#add ~expand:false (new hline);
  let ( _, _, adj) = add_scroll_task_list ~log main display_task in
  main#add ~expand:false (new hline);
  add_bottom_btn ~main ~adj ~wakener ~log display_done_task display_only_day current_day;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> run term main waiter)
    (fun () -> LTerm.disable_mouse term )
;;

