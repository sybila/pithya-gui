source("config.R")          # global configuration
source("tooltips.R")        # texts
source("explorer/plotRow.R")
source("bio.R")

explorerServer <- function(input, session, output) {	

	plotRows <- reactiveValues()

	# Current params value list
	params <- function(model) {
		if (length(model$paramNames) == 0) {
			list()
		} else {
			lapply(1:length(model$paramNames), function(p) {
				unwrapOr(isolate(input[[paste0("param_slider_", p)]]), model$paramRanges[[p]]$min)
			})
		}		
	}

	# Update the parametrised models based on parameter values
	paramsChanged <- function() {
		let(session$pithya$approximatedModel$model, function(model) {
			params <- params(model)
			# Notify plots
			for (row in isolate(reactiveValuesToList(plotRows))) {
				row$vector$state$params <- params
				row$state$state$params <- params
			}
		})
	}

	# Show parameter numeric inputs when model is loaded
	output$param_sliders_bio <- renderUI({
		model <- session$pithya$approximatedModel$model
		if (!is.null(model)) {
		if (length(model$paramNames) == 0) {
			"No parameters"
		} else {
			lapply(1:length(model$paramNames), function(pIndex) {
				debug("[param_sliders_bio] render bio slider")
				min <- model$paramRanges[[pIndex]]$min
				max <- model$paramRanges[[pIndex]]$max
				id <- paste0("param_slider_", pIndex)
				observeEvent(input[[id]], {
					# TODO clean up
					paramsChanged()	
				})
				tooltip(tooltip = Explorer_parameter_tooltip,
					numericInput(
						inputId = id,
						label = paste0(Explorer_parameter_label, model$paramNames[pIndex]),
						min = min, max = max,
						value = (0.1 * (max - min)),
						step = (0.001 * (max - min))
					)
				)
			})
		}			
		} else {
			debug("[param_sliders_bio] remove bio sliders")			
			titlePanel(
				"No model loaded. Please compute approximation first."
			)
		}
	})

	# Remove plots (always - what if variables changed?) when model changes and enable button
	observeEvent(session$pithya$approximatedModel$model, {		
		enabled <- !is.null(session$pithya$approximatedModel$model)
		debug("[explorer] model changed. explorer enabled: ", enabled)
		# remove plots
		lapply(isolate(reactiveValuesToList(plotRows)), function(row) {			
			if (!is.null(row)) {	# TODO this seems to be happenning with the very last removed plot
				row$destroy()
				plotRows[[row$outRow]] <- NULL	
			}
		})
		updateButton(session$shiny, "add_plot_row", disabled = !enabled)
	})

	observeEvent(input$add_plot_row, {
		debug("[explorer] new plot row")
		let(session$pithya$approximatedModel$model, function(model) {
			original <- session$pithya$approximatedModel$original
			row <- createPlotRow(session$pithya$nextId(), model, original, params(model), input, session, output, 
				onApplyAllVector = function(selection) {
					for (row in isolate(reactiveValuesToList(plotRows))) {
						if (!is.null(row)) {
							row$vector$state$selection <- selection
						}
					}
				},
				onApplyAllState = function(selection) {
					for (row in isolate(reactiveValuesToList(plotRows))) {
						if (!is.null(row)) {
							row$state$state$selection <- selection
						}
					}
				},
				onRemove = function(row) {
					row$destroy()
					plotRows[[row$outRow]] <- NULL
				}
			)
			plotRows[[row$outRow]] <- row			
		})			
	})	

	output$plots <- renderUI({
		debug("[explorer] render plots")
		# We have to sort them by numeric ID
		sortedIds <- sort(unlist(lapply(reactiveValuesToList(plotRows), function(x) x$id)))
		tagList(			
			lapply(sortedIds, function(id) {
				uiOutput(paste0("row_output_", id))
			})
        )
	})	

}