source("config.R")          # global configuration
source("tooltips.R")        # texts
source("explorer/plot.R")

explorerServer <- function(input, session, output) {

	plots <- reactiveValues()

	# Show parameter numberic inputs when model is loaded
	output$param_sliders_bio <- renderUI({
		model <- session$pithya$approximatedModel$model
		if (!is.null(model)) {	
			lapply(1:length(model$paramNames), function(pIndex) {
				debug("[param_sliders_bio] render bio slider")
				min <- model$paramRanges[[pIndex]]$min
				max <- model$paramRanges[[pIndex]]$max
				tooltip(tooltip = Explorer_parameter_tooltip,
					numericInput(
						inputId = paste0("param_slider_", pIndex),
						label = paste0(Explorer_parameter_label, model$paramNames[pIndex]),
						min = min, max = max,
						value = (0.1 * (max - min)),
						step = (0.001 * (max - min))
					)
				)
			})
		} else {
			debug("[param_sliders_bio] remove bio sliders")			
			"Model missing. Compute approximation first."
		}
	})

	# TODO move add button out of the dynamic output so that it has to update only on one place

	# Remove graphs (always - what if variables changed?) when model changes and enable button
	observeEvent(session$pithya$approximatedModel$model, {		
		enabled <- !is.null(session$pithya$approximatedModel$model)
		debug("[explorer] model changed. explorer enabled: ", enabled)
		# remove graphs
		lapply(isolate(reactiveValuesToList(plots)), function(plot) {
			if (!is.null(plot)) {	# TODO this seems to be happenning with the very last removed plot
				plot$destroy()
				plots[[plot$plotOutput]] <- NULL	
			}
		})
		updateButton(session$shiny, "add_vf_plot", disabled = !enabled)
	})

	observeEvent(input$add_vf_plot, {
		debug("[explorer] new plot")
		plot <- createPlot(session$pithya$plotId(), input, session, output, function(plotOutput) {
			plots[[plotOutput]]$destroy()
			plots[[plotOutput]] <- NULL	
		})
		plots[[plot$plotOutput]] <- plot
	})	

	output$plots <- renderUI({
		debug("[explorer] render plots")
		# We have to sort them by numeric ID
		sortedIds <- sort(unlist(lapply(reactiveValuesToList(plots), function(x) x$id)))
		tagList(			
			lapply(sortedIds, function(id) {
				uiOutput(plotOutputId(id))	
			}),
			tooltip(tooltip = Explorer_addPlot_tooltip,
                bsButton("add_vf_plot", Explorer_addPlot_label, icon=icon("picture",lib="glyphicon"), disabled = is.null(session$pithya$approximatedModel$model))
            )
        )
	})	

}