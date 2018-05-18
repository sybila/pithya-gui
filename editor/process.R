source("config.R")          # global configuration
source("tooltips.R")        # texts

# process = reactiveValue(running = config, port = num, observer = observer, onSuccess = function, onError = funciton, onKill = function)
# config = list(command, args, stdout, stderr)

safeDestroyObserver <- function(observer) {
	if (!is.null(observer)) {
		debug("[safeDestroyObserver] destroy observer")
		observer$destroy()
	} else {
		debug("[safeDestroyObserver] observer already destroyed")
	}
}

killRemoteProcess <- function(session, process) {
	config <- process$running
	if (!is.null(config)) {		
		debug(paste0("[killRemoteProcess] killing process ", config$command))
		tryCatch({
			# Try to send the kill command
			socket <- make.socket("localhost", process$port)
			write.socket(socket, "kill\n")
			close.socket(socket)
		}, error = function(err) {
			debug("[killRemoteProcess] kill command failed")			

			showModal(modalDialog(title = "Process is not responding!",
				footer = modalButton("Ok"),
				paste0("Pithya could not kill process ", config$command, ". The process might have died unexpectedly. In case it is still running, please try to kill it manually. Normal functionality will now resume.")
			), session = session$shiny)
		}, finally = {
			process$running <- NULL
			safeDestroyObserver(process$observer)
			process$observer <- NULL	
			# If successful, call the kill callback
			process$onKill()	
		})
	} else {
		debug("[killRemoteProcess] no process is currently running")
	}
}

startRemoteProcess <- function(session, process, config) {
	debug("start")
	if (!is.null(process$running)) {
		# There is already another running process that needs to be stopped first.
		# Note: This should not happen. Make sure you disable the process start button while process is running.
		killRemoteProcess(session, process)
	}

	debug("prepare")

	debug(config)

	debug(paste0("[startRemoteProcess] starting ", config$command))

	process$running <- config

	# TODO we don't delete notification files anywhere
	notificationFile <- tempfile(pattern = "notificationFile_", fileext = ".txt", tmpdir = session$pithya$sessionDir)	
	file.create(notificationFile)

	process$observer <- myReactiveFileReader(intervalMillis = 100, session$shiny, notificationFile, function(notification) {
		if (length(notification) > 0) {
			debug(paste0("[notificationObserver] notification received: ", notification[1]))

			process$running <- NULL
			safeDestroyObserver(process$observer)
			process$observer <- NULL
			process$notification <- NULL
			if (grepl("^Success$", notification[1])) {
				process$onSuccess()					
			} else if (grepl("^Killed$", notification[1])) {
				# do nothing, handled by kill function
			} else if (grepl("^Error$", notification[1])) {
				process$onError(notification[length(notification)])
			}	
		}
	})	

	processArgs <- c(process$port, paste0("\"", notificationFile, "\""), config$args)	
	debug(paste(c("[startRemoteProcess] execute: ", "core/bin/", config$command, processArgs), collapse = " "))	

	system2(
		command = paste0("core/bin/", config$command), 
		args = processArgs, 
		stdout = config$stdout, 
		stdin = config$stdin,
		wait = FALSE
	)

	debug(paste(c("[startRemoteProcess] process started")))
}