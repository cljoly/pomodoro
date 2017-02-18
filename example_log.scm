#|
Simple example file, just to show you how things works.
Contains a model for your task and a mini-tutorial to get started.
|#
(
 (settings
  (
   (pomodoro_duration 25)
   (short_break_duration 5)
   (long_break_duration 20)
   (ticking_command "")
   (ringing_command "")
   )
  )
 (tasks
  (

   #|
   The following task is a model. Feel free to copy-paste it for your own task!
   As you may know, the #; comment the first s-expression that follow. You could
   use it to comment a task easily.
   |#
   #;
   ((name "Task name") (description "Task description")
    (done_at "2016-09-10T15:42:38") (done_with 5)
    (estimation 3) (short_interruption 2) (long_interruption 4)
    (day "2018/06/06")
    )

   ((name "Welcome to your new productivity assistant") (description "it helps you to be more efficient.") (day "1/1/3000"))
   ((name "It uses the well known Pomodoro technique, the light way") (description "let's take a quick tour!") (day "1/1/3000"))
   ;; For those reading this task list in the program, be cautious
   ((name "You should read the log file") (description "some line may be cut in the programm.") (day "1/1/3000"))
   ((name "(Psst, look at the day field") (description "we are at the beginning of a new era!)") (day "1/1/3000"))

   ((name "You just complete a log file, like this one, with all your task")
    (description "run the tool.") (day "1/2/3000"))
   ((name "Give the name of your log file as first argument")
    (description "if you edit the file while the tool is running...") (day "1/2/3000"))
   ((name "...everything is refreshed automatically") (description "nice!") (day "1/2/3000"))
   ((name "The tool never ever writes to the log file")
    (description "it is your own property!") (day "1/2/3000"))
   ((name "However, if, once a few pomodoro passed, you record that number in your log file, \
           the tool understand it and refreshes its state cleverly") ;; Note the multi-line name
    (description awesome!) (day "1/2/3000"))

   ((name "Here is some example") (description "you are ready to add your own task.") (day "1/1/3000"))
   ((name "Just before you go") (description "did you notice the order of your tasks is preserved too?"))
   ((name "Name and description") (description "only requiered fields") (day "1/3/3000"))
   ((name Tip) (description "Your editor should read this file as a scheme one") (day "1/3/3000"))
   ((name "A finished task") (description "has both done_at and done_with fields.") (done_at "2016-09-10T16:06:41") (done_with 5) (day "1/3/3000"))
   ((name OCaml) (description "Learn it!") (done_at "2016-09-10T16:06:41") (done_with 5) (day "1/3/3000"))
   ((name "Long strings need quotes") (description "") (done_at "2016-09-10T16:06:41") (done_with 5) (day "1/3/3000"))
   ((name Partial) (description "Some fields are optional") (day "1/3/3000"))
   ))
 )

