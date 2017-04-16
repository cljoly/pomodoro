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

(* Combinaison to filter as few element as possible in case of slicing. May use
 * length of the list if given. A bit picky about choice of start and stop *)
let filter_slice_exn l ?length ~f start stop =
  let ll =
    Option.value_map length
      ~default:(lazy ( List.length l ))
      ~f:(fun i -> Lazy.return i)
  in
  (* TODO Treat these cases by returning [] or a given element of l *)
  assert ( 0 <= start );
  assert ( stop <= (Lazy.force ll) );
  assert ( start < stop );
  (* i is the indice of the current element if the list l had been filtered.
   * Caution, acc is in reverse order for performance reasons *)
  let rec fs acc i l =
    if i < stop
    then
      let keep_if_in_slice x =
        if start <= i (* i < stop verified before *)
        then x :: acc
        else acc
      in
      begin (* Filter like operation *)
        match l with
        | hd :: tl ->
          if f hd
          then fs
              (keep_if_in_slice hd)
              (succ i) tl
          else fs acc i tl
        | [] -> acc
      end
    else acc
  in
  fs [] 0 l
  |> List.rev
;;

(* Scrollable list of tasks *)
class scrollable_task_list ~log (scroll : scrollable) display_task =
  let ptasks () = log#ptasks in
  object
    inherit t "task_list"

    initializer scroll#set_range (List.length (ptasks ()))

    method! can_focus = false

    method! draw ctx _ =
      let { rows ; cols } = LTerm_draw.size ctx in
      (* Return elements of a task list [l] between [top] (of the screen) and
         (top+row) (corresponding to the last line of the screen) *)
      let select_task top free_space_to_keep l =
        let ll = List.length l in
        (* Reset offset to last line and redraw *)
        let reset_offset_redraw () =
          scroll#set_offset ~trigger_callback:true (ll-1);
          List.last l |> Option.to_list
        in
        let top = Int.max 0 top in
        let bottom = Int.min (top + rows - free_space_to_keep) ll in
        match filter_slice_exn l ~length:ll ~f:display_task top bottom with
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
        let non_empty_columns =
          Column.[
            (create "Summary" ~align:Align.Right,
             (fun t -> t#short_summary))
          ; (create "Done" ~align:Align.Center,
             (fun t ->
                t#status#print_both
                  (function Tasks.Done -> "X" | Tasks.Active -> " ")))
          ; (create "with" ~align:Align.Center,
             (fun t -> t#number_of_pomodoro#print_both (soo Int.to_string)))
          ; (create "at" ~align:Align.Center,
             (fun t -> t#done_at#print_both (soo String.to_string)))
          ; (create "Short interruption" ~align:Align.Center,
             (fun t -> t#short_interruption#print_both (soo Int.to_string)))
          ; (create "Long interruption" ~align:Align.Center,
             (fun t -> t#long_interruption#print_both (soo Int.to_string)))
          ; (create "Estimation" ~align:Align.Center,
             (fun t -> t#estimation#print_both (soo Int.to_string)))
          ; (create "Day" ~align:Align.Center,
             (fun t -> t#day#print_both (soo Date.to_string)))
          ]
          |> List.filter_map
            ~f:(fun (creation_fun, content_generator) ->
                let not_empty =
                  List.exists ~f:(fun task -> content_generator task <> "")
                in
                Option.some_if (not_empty task_list)
                  (creation_fun content_generator)
              )
        in
        try
          to_string ~bars:`Unicode ~display:Display.line ~limit_width_to:cols
            non_empty_columns
            task_list
        with
          exn ->
          sprintf "Window is likely to be too small.\n%s" (Exn.to_string exn)
      in
      let offset = scroll#offset in
      (* Total size (in line) the table adds around data *)
      let tab_burden = 4 in
      ptasks ()
      (* |> List.filter ~f:display_task *)
      |> select_task offset tab_burden
      |> draw_table_of_task
      |> LTerm_draw.draw_string ctx 0 0
  end
;;

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
    Tasks.get_pending log#ptasks
    |> Option.value_map ~f ~default
  in
  let timer_cycle : Timer.cycling = new Timer.cycling ~log in
  let mark_pomodoro_done = (fun timer ->
      match timer#of_type with
      | Log_f.Pomodoro -> current_task ~default:()
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
  (Lwt_engine.on_timer log#settings.tick true
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
    current_day
    no_day_filter
  =
  let day_hbox = new hbox in
  let no_dfilter_btn = new button "" in
  let update_on_dfilter_btn () =
    match !no_day_filter with
    | true -> no_dfilter_btn#set_label "No active day filter"
    | false -> no_dfilter_btn#set_label "Filtering task on day basis"
  in
  update_on_dfilter_btn ();
  no_dfilter_btn#on_click (fun () ->
      no_day_filter := not !no_day_filter;
      update_on_dfilter_btn ();
    );
  (* Cycle throw view of current_day task or view of task without day parameter *)
  let current_day_btn = new button "Filter by day" in
  let set_day_to target_date =
    current_day := target_date;
    match !current_day with
    | Some date ->
      Date.to_string date
      |> sprintf "Current day: %s"
      |> current_day_btn#set_label
    | None -> current_day_btn#set_label "No current day"
  in
  set_day_to !current_day;
  let current_day_or_today () =
    Option.value ~default:(Date.today ~zone:Core.Zone.local) !current_day
  in
  current_day_btn#on_click (fun () -> (* Cycle through views *)
      match !current_day with
      | Some _ -> set_day_to None
      | None -> set_day_to (Some (current_day_or_today ()))
    );
  let move_current_day by =
    current_day_or_today ()
    |> (fun current_day ->
        Date.add_days current_day by |> Option.some |> set_day_to)
  in
  let last_day_btn = new button "Last day" in
  last_day_btn#on_click (fun () -> move_current_day (-1));
  let next_day_btn = new button "Next day" in
  next_day_btn#on_click (fun () -> move_current_day 1);
  let ordered_day_in_log =
    let current_day = current_day_or_today () in
    let get_date_and_not_current (ptask:Tasks.ptask) =
      let current_day = current_day_or_today () in
      Option.bind ptask#day#get
        (fun date ->
           Option.some_if (not (Date.equal current_day date)) date)
    in
    let before_after_current_day =
      lazy ( List.filter_map log#ptasks ~f:get_date_and_not_current
      |> List.dedup ~compare:Date.compare
      |> List.partition_tf ~f:Date.(fun date -> date < current_day) )
    in
    let lazy_sort l = lazy (List.sort ~cmp:Date.compare l) in
    Lazy.map before_after_current_day
      ~f:(fun (before_current_day, after_current_day) ->
          ( lazy_sort before_current_day, lazy_sort after_current_day )
        )
  in
  let set_date_in_log f =
    Lazy.force ordered_day_in_log
    |> f
    |> Option.iter ~f:(fun date -> set_day_to (Some date))
  in
  (* Last day present in a task of the log file *)
  let last_log_day_btn = new button "Last log day" in
  last_log_day_btn#on_click
    (fun () ->
       set_date_in_log (fun (before_current_day, _) ->
           Lazy.force before_current_day |> List.last)
    );
  (* Next day present in a task of the log file *)
  let next_log_day_btn = new button "Next log day" in
  next_log_day_btn#on_click
    (fun () ->
       set_date_in_log (fun (_, after_current_day) -> Lazy.force after_current_day |> List.hd)
    );

  day_hbox#add ~expand:true no_dfilter_btn;
  day_hbox#add ~expand:true current_day_btn;
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
  let no_day_filter = ref true in
  (* Day for which tasks are considered *)
  let current_day = ref (Some (Date.today ~zone:Core.Zone.local)) in

  (* Whether a task should be displayed or not, on done and date criteria *)
  let display_task task =
    let is_coherent_with_current_day () =
      match !current_day with
      | Some day ->
        Option.value_map ~default:false task#day#get ~f:(Date.equal day)
      | None -> Option.is_none task#day#get
    in
    (!display_done_task || not task#is_done)
    &&
    (!no_day_filter || is_coherent_with_current_day ())
  in

  add_pomodoro_timer ~log main;
  let ( _, _, adj) = add_scroll_task_list ~log main display_task in
  add_bottom_btn ~main ~adj ~wakener ~log display_done_task current_day no_day_filter;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> run term main waiter)
    (fun () -> LTerm.disable_mouse term )
;;

