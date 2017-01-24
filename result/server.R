source("config.R")          # global configuration
source("tooltips.R")        # texts
source("result/plotRow.R")
source("result.R")


resultServer <- function(input, session, output) {

	plotRows <- reactiveValues()

	# Remove plots when synthesis result changes and enable button
	observeEvent(session$pithya$synthesisResult$result, {		
		enabled <- !is.null(session$pithya$synthesisResult$result)
		debug("[result] results changed. result tab enabled: ", enabled)
		# remove graphs
		lapply(isolate(reactiveValuesToList(plotRows)), function(row) {			
			if (!is.null(row)) {	# TODO this seems to be happenning with the very last removed plot
				row$destroy()
				plotRows[[row$outRow]] <- NULL	
			}
		})
		updateButton(session$shiny, "add_param_plot", disabled = !enabled)
	})

	# Add plot row on button click 
	observeEvent(input$add_param_plot, {
		debug("[result] new plot row")
		let(session$pithya$synthesisResult$result, function(result) {
			row <- createResultPlotRow(session$pithya$nextId(), result, input, session, output, 
				onRemove = function(row) {
					row$destroy()
					plotRows[[row$outRow]] <- NULL
				}
			)			
			plotRows[[row$outRow]] <- row			
		})			
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

	# Load result file into the text editor after upload or reset
	observeEvent(input$ps_file, {
		if (is.null(input$ps_file) || is.null(input$ps_file$datapath)) {
			showNotification("Invalid result file")			
		} else {
			tryCatch({
				result <- parseResultFile(input$ps_file$datapath)
				session$pithya$synthesisResult$result <- result
				# Always outdates, because it is loaded from file
				session$pithya$synthesisResult$outdated <- TRUE
				debug("result loaded successfully")
			}, error = function(e) {
				showNotification(paste0("Invalid result file: ", e))
			})
		}
	})

	# Download result file (if available)
	output$save_result_file <- downloadHandler(
		filename = "results.json",
		content = function(file) {
			content <- isolate(session$pithya$synthesisResult$file)
			writeLines(readLines(content), file)
		}
	)

}