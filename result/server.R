source("config.R")          # global configuration
source("tooltips.R")        # texts
source("result/plotRow.R")
source("result.R")


resultServer <- function(input, session, output) {

	plotRows <- reactiveValues()

	# Compute coverage config
	observeEvent(c(session$pithya$synthesisResult$result, input$coverage_check, input$density_coeficient), {
		enabled <- input$coverage_check
		density <- input$density_coeficient
		result <- session$pithya$synthesisResult$result
		debug("[coverage config] changed")
		if (is.null(result) || (result$type == "rectangular" && !enabled)) {
			session$pithya$synthesisResult$coverageConfig <- NULL
		} else {
			session$pithya$synthesisResult$coverageConfig <- list(
				result = result,
				thresholds = lapply(result$paramRanges, function(range) {
					seq(range$min, range$max, length.out = density)	
				}),
				count = result$paramCount,
				thresholdSized = rep(density, result$paramCount),
				dimensionSizes = rep(density - 1, result$paramCount)
			)
		}
	})

	# Compute parameter coverage when result changes and is SMT or coverage is enabled
	observeEvent(session$pithya$synthesisResult$coverageConfig, {
		config <- session$pithya$synthesisResult$coverageConfig
		if (is.null(config)) {
			session$pithya$synthesisResult$coverage <- NULL
		} else {
			result <- config$result
			withProgress(message = "Computing parameter coverage...", 
				min = 0, max = length(result$paramValues), value = 0,
			expr = {				
				thresholds <- config$thresholds
				dimensionSizes <- config$dimensionSizes
				centers <- lapply(thresholds, function(t) (t[-1] + t[-length(t)]) / 2)
				one <- array(1, dimensionSizes)
				params <- lapply(1:config$count, function(d) {
					explodeArray(centers[[d]], d, dimensionSizes)	
				})

				coverage <- lapply(result$paramValues, function(p) {
					incProgress(1)
					if (result$type == "smt") { p(params) } else {
						# TODO this can be optimized to compute this for thresholds first and then explode them
						Reduce(function(a,b) a | b, lapply(p, function(rect) rectangleContainsPoints(rect, params)))
					}	
				})

				config$data <- coverage
				session$pithya$synthesisResult$coverage <- config
			})
		}
	})

	# Update coverage after it has been computed
	observeEvent(session$pithya$synthesisResult$coverage, {
		for (row in isolate(reactiveValuesToList(plotRows))) {
			row$params$state$coverage <- session$pithya$synthesisResult$coverage
		}
	})

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