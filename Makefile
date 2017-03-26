all:
	corebuild -pkg textutils -pkg lambda-term -pkg lwt -pkg inotify.lwt src/pomodoro.native
