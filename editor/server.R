source("config.R")          # global configuration
source("tooltips.R")        # texts
source("editor/process.R")	# remote process inerface

# Setup all reactive dependencies in the editor tab.

editorServer <- function(input, session, output) {

	sessionDir <- session$pithya$sessionDir
	progressFile <- tempfile(pattern = "progress", fileext = ".txt", tmpdir = sessionDir)
	file.create(progressFile)
	progressReader <- reactiveFileReader(intervalMillis = 100, session$shiny, progressFile, readLines)

	printProgress <- function(line) {
		print(line)
		write(paste0(line, "\n"), progressFile, append = T)
	}


	approximationProcess <- reactiveValues(
		running = NULL,
		port = 9999,
		observer = NULL,
		resultFile = NULL,
		inputFile = NULL,
		notificationID = NULL,		
		onSuccess = function() {
			debug("[approximationProcess] success")
			output <- approximationProcess$resultFile
			if (file.exists(output) && length(readLines(output)) > 0) {
        		# Approximation success
        		session$pithya$approximatedModel$file <- output
        		session$pithya$approximatedModel$outdated <- FALSE	        		        	
        		printProgress(Approximation_finished)
        		approximationProcess$finalize(TRUE)
        		showNotification(Approximation_finished)
        	} else {
        		printProgress(Approximation_error)
        		approximationProcess$onError("Missing result")
        	}        	
		},
		onError = function(e) {
			debug(paste0("[approximationProcess] error ", e))
			approximationProcess$finalize(FALSE)
			showModal(modalDialog(title = Approximation_error, e))
		},
		onKill = function() {
			debug("[approximationProcess] killed")
			approximationProcess$finalize(FALSE)
		},
		finalize = function(success) {
			# remove input file
			if (!is.null(approximationProcess$inputFile)) {
				file.remove(approximationProcess$inputFile)
				approximationProcess$inputFile <- NULL
			}
			# hide progress notification			
			if (!is.null(approximationProcess$notificationID)) {
				removeNotification(approximationProcess$notificationID)
				approximationProcess$notificationID <- NULL
			}
			# remove result if not successfule
			if (!success && !is.null(approximationProcess$resultFile)) {
				file.remove(approximationProcess$resultFile)			
				approximationProcess$resultFile <- NULL
			}						
		}
	)

	synthesisProcess <- reactiveValues(
		running = NULL,
		port = 9998,
		observer = NULL,
		propertyFile = NULL,
		resultFile = NULL,
		notificationID = NULL,
		missingThresholds = NULL,
		onSuccess = function() {
			debug("[synthesisProcess] success")	

			session$pithya$synthesisResult$file <- synthesisProcess$resultFile
			session$pithya$synthesisResult$outdated <- FALSE			
			synthesisProcess$finalize(TRUE)
			showNotification("Parameter synthesis finished")
		},
		onError = function(e) {
			debug(paste0("[synhtesisProcess] error ", e))
			synthesisProcess$finalize(FALSE)
			if (grepl("Missing thresholds: .*", e)) {
				# Note: This message has a fixed syntax and therefore this matching should not fail!
				thresholdList <- sub("Missing thresholds: ", "", e)
				thresholds <- strsplit(thresholdList, split = "; ", fixed = TRUE)
				thresholds <- lapply(thresholds, function(t) {
					list(name = sub(": .+", "", t), thresholds = strsplit(sub(".+: ", "", t), split = ", ", fixed = FALSE))
				})
				synthesisProcess$missingThresholds <- thresholds[[1]]
				# We have missing thresholds!
				showModal(modalDialog(title = "Missing thresholds!",
					paste0("Variable thresholds ", thresholdList, " are missing in the model. Click `Add` to add them to the model and recompute approximation."), 
					footer = tagList(
						modalButton("Cancel"),
						actionButton("add_thresholds", "Add")
					)
				))
			} else {
				showModal(modalDialog(title = "Snyhesis error!", e))
			}
		},
		onKill = function() {
			debug("[synthesisProcess] killed")
			synthesisProcess$finalize(FALSE)
		},
		finalize = function(success) {
			# remove property file
			if (!is.null(synthesisProcess$propertyFile)) {
				file.remove(synthesisProcess$propertyFile)
				synthesisProcess$propertyFile <- NULL
			}
			# hide progress notification
			if (!is.null(synthesisProcess$notificationID)) {
				removeNotification(synthesisProcess$notificationID)
				approximationProcess$notificationID <- NULL
			}
			# remove result if not successful
			if (!success && !is.null(synthesisProcess$resultFile)) {
				file.remove(synthesisProcess$resultFile)
				synthesisProcess$resultFile <- NULL
			}
		}
	)

	## Basic reactive interactions between the elements

	## These two observers have to be set up BEFORE the file upload observers, so that the 
	## default examples are properly observed 

	# Enable approximation button when process is not running and result is outdated
	observeEvent(c(session$pithya$approximatedModel$outdated, approximationProcess$running), {
		enabled <- is.null(approximationProcess$running) && session$pithya$approximatedModel$outdated
		updateButton(session$shiny, "generate_abstraction", style = "success", disabled = !enabled)
	})
	
	# Enable synthesis button when process is not running and model is ready and result is outdated
	observeEvent(c(input$prop_input_area, session$pithya$synthesisResult$outdated, session$pithya$approximatedModel$outdated), {
		# TODO check if the properties are identical to the last synthesised one
		enabled <- is.null(synthesisProcess$running) && session$pithya$synthesisResult$outdated	&& !session$pithya$approximatedModel$outdated
		updateButton(session$shiny, "process_run", style = "success", disabled = !enabled)
	})

	# Invalidate approximated model when input changes
	observeEvent(input$model_input_area, {
		# TODO check if the model is identical to the last approximated one		
		session$pithya$approximatedModel$outdated = TRUE
		session$pithya$synthesisResult$outdated = TRUE
	})

	# Invalidate synthesis result when property changes
	observeEvent(input$prop_input_area, {
		session$pithya$synthesisResult$outdated = TRUE	
	})

	# Load model file into the text editor after upload or reset
	observeEvent(c(input$model_file, input$reset_model), {
		if (is.null(input$model_file) || is.null(input$model_file$datapath)) {
			# load example model TODO: remove in final version
			printProgress(Starting_advice)
			data <- readLines(paste0(session$pithya$examplesDir, defaultModel))
		} else {
			data <- readLines(input$model_file$datapath)
		}
		updateAceEditor(session$shiny, "model_input_area", value = paste(data, collapse = "\n"))    	
	})

	# Load property file into the text editor after upload or reset
	observeEvent(c(input$prop_file, input$reset_prop), {
		if (is.null(input$prop_file) || is.null(input$prop_file$datapath)) {
			# load example properties TODO: remove in final version
			data <- readLines(paste0(session$pithya$examplesDir, defaultProperty))
		} else {
			data <- readLines(input$prop_file$datapath)
		}
		updateAceEditor(session$shiny, "prop_input_area", value = paste(data, collapse = "\n"))
	})

	# Model file download
	output$save_model_file <- downloadHandler(
		filename = "model.bio",
		content = function(file) {
			content <- isolate(input$model_input_area)
			writeLines(content, file)
		}
	)

	# Property file download
	output$save_prop_file <- downloadHandler(
    	filename = "property.ctl",
    	content = function(file) {
    		content <- isolate(input$prop_input_area)
    		writeLines(content, file)       
    	}
	)

	## Model approximation runner

	# TODO enable full progress indicator
	# TODO make sure tractor prints nothing to the stdout when error occurs
	# TODO dont forget to enable model explorer after this

	# We need this as an extra function because it's called after thresholds are generated
	generateApproximation <- function(modelData) {
		debug("[generateApproximation] start")
		printProgress(Approximation_started)

		if (!is.null(approximationProcess$running)) {
			# Should not happen, but just to be sure...
			killRemoteProcess(session, approximationProcess)
		}

		approximationProcess$inputFile <- tempfile(pattern = "approximationInput", fileext = ".bio", tmpdir = sessionDir)
		approximationProcess$resultFile <- tempfile(pattern = "approximationOutput", fileext = ".bio", tmpdir = sessionDir)
		writeLines(modelData, approximationProcess$inputFile)

		approximationProcess$notificationID <- showNotification(
			tagList("Approximation running", actionButton("approximation_kill", "Cancel")), 
			duration = NULL, closeButton = FALSE
		)

		startRemoteProcess(session, approximationProcess, list(
			command = paste0(corePath, "tractor"), 
			args = c(approximationProcess$inputFile, ifelse(input$fast_approximation,"true","false"), ifelse(input$thresholds_cut,"true","false")),
			stdout = approximationProcess$resultFile,
			stderr = progressFile
		))		
	}
		
	observeEvent(input$generate_abstraction, {
		generateApproximation(input$model_input_area)
	})

	observeEvent(input$approximation_kill, {
		debug("[killApproximation] kill requested")	
		killRemoteProcess(session, approximationProcess)
	})

	observeEvent(input$synthesis_kill, {
		debug("[killSynthesis] kill requested")	
		killRemoteProcess(session, synthesisProcess)
	})

	observeEvent(input$add_thresholds, {
		tryCatch({
			removeModal()
			t <- synthesisProcess$missingThresholds			
			if (!is.null(t)) {				
				debug("[addThresholds] adding:", paste0(t, collapse = " "))
				model <- input$model_input_area
				# current model without comments (we can't match anyhting in the comments)
				modelClean <- str_replace_all(model, "(#|//).*\n", "\n")
				debug(modelClean)
				for (i in 1:length(t$name)) {
					name <- t$name[i]
					newThresholds <- t$thresholds[i]
					headerPattern <- paste0("THRES:[ \t]+", name,":[ \t]+")
					linePattern <- paste0(headerPattern, "([-0-9,. \t]+)")
					debug("[addThresholds] line pattern: ", linePattern)
					match <- str_match(modelClean, linePattern)
					if (length(match) >= 2) {
						# trim ensures that whitespace around the match is preserved after we replace it
						originalMatch <- str_trim(match[1])
						originalThresholdString <- match[2]
						originalThresholds <- strsplit(originalThresholdString, ",", fixed = TRUE)
						debug("[addThresholds] extracted original thresholds: ", paste0(originalThresholds, collapse = ", "))
						combined <- unique(sort(as.numeric(unlist(c(newThresholds, originalThresholds)))))
						debug("[addThresholds] final thresholds: ", paste0(combined, collapse = ", "))
						model <- sub(originalMatch, paste0("THRES: ", name, ": ", paste(combined, collapse = ", ")), model)
					}												
				}
				updateAceEditor(session$shiny, "model_input_area", value = model)
				showNotification("Thresholds added successfully")
				# We need to pass model directly because the editor is not updated yet
				generateApproximation(model)
			}
			synthesisProcess$missingThresholds <- NULL
		}, error = function(e) {
			debug("[addThresholds] error: ", e)	
			showModal(modalDialog(title = "Error while adding thresholds",
				"Pithya could not add missing thresholds to the model file. Make sure there are not syntax errors and try again or add the thresholds manually."
			))
		})
	})

	## Parameter synthesis runner

	observeEvent(input$process_run, {
		debug("[performSynthesis] start")	
		printProgress(Parameter_synthesis_started)

		if (!is.null(synthesisProcess$running)) {
			killRemoteProcess(session, synthesisProcess)
		}

		# TODO some safety checks about model file, would ya?

		# Run combine to check syntax and semantics
		synthesisProcess$propertyFile <- tempfile(pattern = "property", fileext = ".ctl", tmpdir = sessionDir)
		synthesisProcess$resultFile <- tempfile(pattern = "synthesisResult", fileext = ".ctl", tmpdir = sessionDir)
		writeLines(input$prop_input_area, synthesisProcess$propertyFile)

		synthesisProcess$notificationID = showNotification(
			tagList("Parameter synthesis running", actionButton("synthesis_kill", "Cancel")), 
			duration = NULL, closeButton = FALSE)

		# TODO thread count
		startRemoteProcess(session, synthesisProcess, list(
			command = paste0(corePath, "biodivine-ctl"),
			args = c(session$pithya$approximatedModel$file, synthesisProcess$propertyFile),
			stdout = synthesisProcess$resultFile,
			stderr = progressFile
		))

	})

}