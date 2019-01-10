source("config.R")          # global configuration
source("tooltips.R")        # texts
source("result/plotRow.R")
source("result.R")


resultServer <- function(input, session, output) {

	plotRows <- reactiveValues()
	
	# Load synthesis results as current one
	observeEvent(input$load_synth_results, {
	  session$pithya$currentResult$file   <- session$pithya$synthesisResult$file
	  session$pithya$currentResult$result <- session$pithya$synthesisResult$result
	  session$pithya$currentResult$loaded <- Result_synthResults_tag
	  updateButton(session$shiny, "load_synth_results", style = "default")
	})
	# Load TC analysis results as current one
	observeEvent(input$load_tca_results, {
	  session$pithya$currentResult$file   <- session$pithya$TCanalysisResult$file
	  session$pithya$currentResult$result <- session$pithya$TCanalysisResult$result
	  session$pithya$currentResult$loaded <- Result_TCAResults_tag
	  updateButton(session$shiny, "load_tca_results", style = "default")
	})
	# Load imported results as current one
	observeEvent(input$load_imported_results, {
	  session$pithya$currentResult$file   <- session$pithya$importedResult$file
	  session$pithya$currentResult$result <- session$pithya$importedResult$result
	  session$pithya$currentResult$loaded <- Result_importedResults_tag
	  updateButton(session$shiny, "load_imported_results", style = "default")
	})
	
	# Activates and/or turns button green after new result is ready
	observeEvent(session$pithya$synthesisResult$result, {
	  enabled <- !is.null(session$pithya$synthesisResult$result)
	  debug("[result] synthesis results changed. result tab enabled: ", enabled)
	  
	  if(enabled) {
	    if(is.null(session$pithya$currentResult$result)) {
	      # if no previous results were loaded then automaticaly load this one
	      session$pithya$currentResult$file   <- session$pithya$synthesisResult$file
	      session$pithya$currentResult$result <- session$pithya$synthesisResult$result
	      session$pithya$currentResult$loaded <- Result_synthResults_tag
	      updateButton(session$shiny, "load_synth_results", disabled = !enabled, style = "default")
	    } else {
	      # if laoded results come from this source, it's important to inform user about being out of date now
	      if(session$pithya$currentResult$loaded == Result_synthResults_tag) {
	        output$error_message <- renderUI({
	          tags$h3(style = "text-align: center; margin: 15px; color: red;", "Warning: Shown PS results are out of date.")
	        })
	      }
	      updateButton(session$shiny, "load_synth_results", disabled = !enabled, style = "success")
	    }
	  } else {
	    updateButton(session$shiny, "load_synth_results", disabled = !enabled, style = "default")
	  }
	}, ignoreNULL = FALSE, ignoreInit = TRUE)
	# Activates and/or turns button green after new result is ready
	observeEvent(session$pithya$TCanalysisResult$result, {
	  enabled <- !is.null(session$pithya$TCanalysisResult$result)
	  debug("[result] Attractor analysis results changed. result tab enabled: ", enabled)
	  
	  if(enabled) {
	    if(is.null(session$pithya$currentResult$result)) {
	      # if no previous results were loaded then automaticaly load this one
	      session$pithya$currentResult$file   <- session$pithya$TCanalysisResult$file
	      session$pithya$currentResult$result <- session$pithya$TCanalysisResult$result
	      session$pithya$currentResult$loaded <- Result_TCAResults_tag
	      updateButton(session$shiny, "load_tca_results", disabled = !enabled, style = "default")
	    } else {
	      # if laoded results come from this source, it's important to inform user about being out of date now
	      if(session$pithya$currentResult$loaded == Result_TCAResults_tag) {
	        output$error_message <- renderUI({
	          tags$h3(style = "text-align: center; margin: 15px; color: red;", "Warning: Shown AA results are out of date.")
	        })
	      }
	      updateButton(session$shiny, "load_tca_results", disabled = !enabled, style = "success")
	    }
	  } else {
	    updateButton(session$shiny, "load_tca_results", disabled = !enabled, style = "default")
	  }
	}, ignoreNULL = FALSE, ignoreInit = TRUE)
	# Activates and/or turns button green after new result is ready
	observeEvent(session$pithya$importedResult$result, {
	  enabled <- !is.null(session$pithya$importedResult$result)
	  debug("[result] imported results changed. result tab enabled: ", enabled)
	  
	  if(enabled) {
	    if(is.null(session$pithya$currentResult$result)) {
	      # if no previous results were loaded then automaticaly load this one
	      session$pithya$currentResult$file   <- session$pithya$importedResult$file
	      session$pithya$currentResult$result <- session$pithya$importedResult$result
	      session$pithya$currentResult$loaded <- Result_importedResults_tag
	      updateButton(session$shiny, "load_imported_results", disabled = !enabled, style = "default")
	    } else {
	      # if laoded results come from this source, it's important to inform user about being out of date now
	      if(session$pithya$currentResult$loaded == Result_importedResults_tag) {
	        output$error_message <- renderUI({
	          tags$h3(style = "text-align: center; margin: 15px; color: red;", "Warning: Shown imported results are out of date.")
	        })
	      }
	      updateButton(session$shiny, "load_imported_results", disabled = !enabled, style = "success")
	    }
	  }
	})

	# Remove plots when synthesis result changes and enable button
	observeEvent(session$pithya$currentResult$result, {		
		enabled <- !is.null(session$pithya$currentResult$result)
		debug("[result] current results changed. result tab enabled: ", enabled)
		# remove graphs
		lapply(isolate(reactiveValuesToList(plotRows)), function(row) {			
			if (!is.null(row)) {	# TODO this seems to be happenning with the very last removed plot
				row$destroy()
				plotRows[[row$outRow]] <- NULL	
			}
		})
		# enable button
		updateButton(session$shiny, "add_param_plot", disabled = !enabled)
		# reset out of sync information (if any)
		if(enabled) {
		  #closeAlert(session$shiny,"result_notification")
		  if(session$pithya$currentResult$loaded %in% c(Result_synthResults_tag, Result_importedResults_tag, Result_TCAResults_tag)) {
  		  output$error_message <- renderUI({
    		  tags$h3(style = "text-align: center; margin: 15px; color: black;", 
    		          paste0("Loaded results come from ",
    		                 ifelse(session$pithya$currentResult$loaded == Result_synthResults_tag, "parameter synthesis procedure",
    		                        ifelse(session$pithya$currentResult$loaded == Result_importedResults_tag, "imported file",
    		                               ifelse(session$pithya$currentResult$loaded == Result_TCAResults_tag, "attractor analysis"))),
    		                 "."))
    		})
		  }
		  # prepare first row of plots
		  prepareOneResultPlotRow(session, input, output, plotRows)
		}
	}, ignoreNULL = FALSE, ignoreInit = TRUE)

	output$result_notification <- renderUI({
		if (!is.null(session$pithya$synthesisResult$result) && !is.null(session$pithya$currentResult$loaded) && 
		    session$pithya$currentResult$loaded == Result_synthResults_tag && session$pithya$synthesisResult$outdated) {
		  tags$h3(style = "text-align: center; margin: 15px; color: red", "Warning: Shown PS results are out of sync with the current contents of the model or property editor.")
		} else if (!is.null(session$pithya$TCanalysisResult$result) && !is.null(session$pithya$currentResult$loaded) && 
		           session$pithya$currentResult$loaded == Result_TCAResults_tag && session$pithya$TCanalysisResult$outdated) {
		  tags$h3(style = "text-align: center; margin: 15px; color: red", "Warning: Shown AA results are out of sync with the current content of the model editor.")
	  } else if (!is.null(session$pithya$importedResult$result) && !is.null(session$pithya$currentResult$loaded) && 
	             session$pithya$currentResult$loaded == Result_importedResults_tag && session$pithya$importedResult$outdated) {
	    tags$h3(style = "text-align: center; margin: 15px; color: red", "Warning: Shown imported results don't have to be related to the current model and/or property.")
	  }
	})
	
	# Add plot row on button click 
	observeEvent(input$add_param_plot, {
	  # prepare another row of plots
	  prepareOneResultPlotRow(session, input, output, plotRows)
	})	

	output$error_message <- renderUI({
		if (is.null(session$pithya$currentResult$result)) {
			titlePanel("No results loaded. Compute parameter synthesis, attractor analysis or import saved results.")
		} else ""
	})

	# Render plot rows
	output$param_plots <- renderUI({
		debug("[result] render plots")
		# We have to sort them by numeric ID
		sortedIds <- sort(unlist(lapply(reactiveValuesToList(plotRows), function(x) x$id)))
		tagList(
			lapply(sortedIds, function(id) {
				uiOutput(paste0("row_output_", id))
			})
        )
	})

	# TODO this is not working
	#observeEvent(session$pithya$synthesisResult$file, {
	#	updateButton(session$shiny, "save_result_file", style = "success", disabled = is.null(session$pithya$synthesisResult$file))	
	#})

	# Import result file of user choice from local destination
	observeEvent(input$ps_file, {
		if (is.null(input$ps_file) || is.null(input$ps_file$datapath)) {
			showNotification("Invalid result file to import")			
		} else {
			tryCatch({
			  session$pithya$importedResult$file <- input$ps_file$datapath
				result <- parseResultFile(input$ps_file$datapath)
				session$pithya$importedResult$result <- result
				session$pithya$importedResult$outdated <- TRUE
				debug("result imported successfully")
			}, error = function(e) {
				showNotification(paste0("Invalid result file to import: ", e))
			})
		}
	})

	# Download synthesis result file (if available)
	output$save_result_file <- downloadHandler(
		filename = function(loaded = session$pithya$currentResult$loaded) {
		  if(loaded == Result_synthResults_tag) return("PS_results.json")
		  if(loaded == Result_importedResults_tag) return("imported results.json")
		  if(loaded == Result_TCAResults_tag) return("AA_results.json")
		  else return ("results.json")
	  },
		content = function(file) {
			content <- isolate(session$pithya$currentResult$file)
			if(!is.null(content))
			  writeLines(readLines(content), file)
		}
	)

}