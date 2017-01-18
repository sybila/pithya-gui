source("config.R")          # global configuration
source("tooltips.R")        # texts

# Setup all reactive dependencies in the editor tab.

editorServer <- function(input, session, output) {

	sessionDir <- session$pithya$sessionDir
	progressFile <- tempfile(pattern = "progress", fileext = ".txt", tmpdir = sessionDir)
	progressReader <- reactiveFileReader(intervalMillis = 100, session$shiny, progressFile, readLines)

	printProgress <- function(line) {
		print(line)
		writeLines(line, progressFile)
	}

	## Basic reactive interactions between the elements


	## These two observers have to be set up BEFORE the file upload observers, so that the 
	## default examples are properly observed 

	# Enable approximation button after model text area changes
	observeEvent(input$model_input_area, {
		# TODO check if the model is identical to the last approximated one
		updateButton(session$shiny, "generate_abstraction", style = "success", disabled = FALSE)
		session$pithya$approximatedModel$outdated = TRUE
		session$pithya$synthesisResult$outdated = TRUE
	})
	
	# Update synthesis button status when property file or approximated model changes
	observeEvent(c(input$prop_input_area, session$pithya$approximatedModel$outdated), {
		# TODO check if the properties are identical to the last synthesised one
		model <- session$pithya$approximatedModel
		# Enable synthesis button assuming the approximated model is computed (not null) and not outdated
		updateButton(session$shiny, "process_run", style = "success", disabled = is.null(model$file) || model$outdated)
		session$pithya$synthesisResult$outdated = TRUE
	})

	# Load model file into the text editor after upload or reset
	observeEvent(c(input$model_file, input$reset_model), {
		if (is.null(input$model_file) || is.null(input$model_file$datapath)) {
			# load example model TODO: remove in final version
			cat(Starting_advice, file = progressFile)
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

	observeEvent(input$generate_abstraction, {
		printProgress(Approximation_started)
		updateButton(session$shiny, "generate_abstraction", style = "success", disabled = TRUE)

		# TODO enable full progress indicator
		# TODO make tractor work with standard input instead of argument
		# TODO make sure tractor prints nothing to the stdout when error occurs
		# TODO dont forget to enable model explorer after this
		withProgress(message = Approximation_running, value = 0.5, {
			inputModel <- tempfile(pattern = "approximationInput", fileext = ".bio", tmpdir = sessionDir)
			outputModel <- tempfile(pattern = "approximationOutput", fileext = ".bio", tmpdir = sessionDir)
			writeLines(input$model_input_area, inputModel)

			system2(
				command = paste0(corePath, "tractor"),
				args = c(inputModel, ifelse(input$fast_approximation,"true","false"), ifelse(input$thresholds_cut,"true","false")),
                stdout = outputModel,
                stderr = progressFile, 
            	wait = TRUE
        	)

	        file.remove(inputModel)
	        if (file.exists(outputModel) && length(readLines(outputModel)) > 0) {
	        	# Approximation success
	        	session$pithya$approximatedModel$file <- outputModel
	        	session$pithya$approximatedModel$outdated <- FALSE	        		        	
	        	printProgress(Approximation_finished)
	        } else {
	        	printProgress(Approximation_error)
	        }
		})
	})

	## Parameter synthesis runner

	synthesisProcess <- reactiveValues(pid = NULL, result = NULL)

	observeEvent(input$process_run, {
		file.remove(progressFile)	#clear progress
		printProgress(Parameter_synthesis_started)
		updateButton(session$shiny, "process_run", style = "success", disabled = TRUE)

		# TODO some safety checks about model file, would ya?

		# Run combine to check syntax and semantics
		model <- session$pithya$approximatedModel$file
		property <- tempfile(pattern = "property", fileext = ".ctl", tmpdir = sessionDir)
		config <- tempfile(pattern = "config", fileext = ".json", tmpdir = sessionDir)
		writeLines(input$prop_input_area, property)

    	system2(
    		command = paste0(corePath, "combine"),
    		args = c(model, property), 
    		stdout = config, 
    		stderr = progressFile, 
    		wait = T
		)

    	file.remove(property)
    	
    	if(file.exists(config) && length(readLines(config)) > 0) {
    		printProgress(Experiment_config_file_created)

    		result <- tempfile(pattern = "synthesisResult", fileext = ".json", tmpdir = sessionDir)
    		synthesisProcess$result <- result

        	system2(
        		command = paste0(corePath, "biodivine-ctl"), 
        		args = c(config),
        		stdout = result,
        		stderr = progressFile,
        		wait = T
    		)

  			# Here, we don't know if the execution was successful, only that it is finished

  			synthesisProcess$pid <- NULL

  			# TODO update process run button to correct state. Somehow.
  			# TODO delete result file if process failed
        
    	} else {
    		printProgress(Combine_error)
    	}
	})

	# TODO Verify that errors won't cause problems
	# TODO What if I rerun the process?

	# Update process ID when progress changes
	observe({
		#Check PID
		pid <- gsub("PID: ","",grep("^PID: [0-9]+$", progressReader(), value = TRUE))
		if (length(pid) > 0 && !is.null(synthesisProcess$result)) {
			synthesisProcess$pid <- pid
		} else {
			synthesisProcess$pid <- NULL
		}
	})

	# Publish successful results
	observe({		
		#Check Done
		if(T %in% grepl("^!!DONE!!$", progressReader()) && !is.null(synthesisProcess$result)) {
			session$pithya$synthesisResult$file <- synthesisProcess$result
			session$pithya$synthesisResult$outdated <- FALSE
			synthesisProcess$result <- NULL
			synthesisProcess$pid <- NULL

			# TODO add this to texts

			js_string <- paste0('alert("Parameter synthesis done!");')
	        session$shiny$sendCustomMessage(type='paramSynthEnd', list(value = js_string))
	    }
	})

	# Update process kill buntton when PID changes
	observeEvent(synthesisProcess$pid, {
		updateButton(session$shiny, "process_stop", style = "danger", disabled = is.null(synthesisProcess$pid))
	})

	# Kill synthesis process and remove result file (other clean up is performed elsewhere)
	observeEvent(input$process_stop, {
		pid <- synthesisProcess$pid
		result <- synthesisProcess$result
		if (!is.null(pid)) {
			synthesisProcess$result <- NULL
			printProgress(pid)
			command <- ifelse(.Platform$OS.type=="windows", paste0("taskkill /f /pid ",pid), paste0("kill -9 ",pid))
			system(command,wait = TRUE)
			printProgress(Parameter_synthesis_stopped)
			if (!is.null(result)) {
				file.remove(result)				
			}
		}
	})

}