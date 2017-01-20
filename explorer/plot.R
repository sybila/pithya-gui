source("config.R")          # global configuration
source("tooltips.R")        # texts
source("explorer/ui.R")

plotOutputId <- function(id) { paste0("plot_output_", id) }

## Initializes a plot into the input/output objects and returns a plot
# plot = list(id = num, outputId = string, destroy = function(), ...)
createPlot <- function(id, input, session, output, onRemove = function(outputId) {}) {
	debug("[createPlot] create new plot ", id)
	
	# Input element IDs	
	removePlot <- paste0("plot_remove_", id)
	hidePlot <- paste0("plot_hide_", id)
	xDimSelect <- paste0("plot_dimen_x_", id)
	yDimSelect <- paste0("plot_dimen_y_", id)
	applyVectorToAll <- paste0("plot_apply_vector_to_all", id)
	applyStateToAll <- paste0("plot_apply_state_to_all", id)
	applyVectorToState <- paste0("plot_apply_vector_to_state_", id)
	applyStateToVector <- paste0("plot_apply_state_to_vector_", id)	# not used
	clearVector <- paste0("plot_clear_vector_", id)
	clearState <- paste0("plot_clear_state_", id)
	unzoomVector <- paste0("plot_unzoom_vector_", id)
	unzoomState <- paste0("plot_unzoom_state_", id)
	usePWMA <- paste0("plot_pwma_", id)
	exactVector <- paste0("plot_exact_vector_", id)
	exactState <- paste0("plot_exact_state_", id)
	plotVector <- paste0("plot_vector_", id)
	plotState <- paste0("plot_state_", id)

	# Output IDs
	plotOutput <- plotOutputId(id)
	scaleVector <- paste0("plot_scale_vector_", id)
	scaleState <- paste0("plot_scale_state_", id)
	
	# Event inputs
	clickVector <- paste0("plot_click_vector_", id)
	clickState <- paste0("plot_click_state_", id)
	doubleClickVector <- paste0("plot_double_click_vector_", id)
	doubleClickState <- paste0("plot_double_click_state_", id)
	brushVector <- paste0("plot_brush_vector_", id)
	brushState <- paste0("plot_brush_state_", id)
	hoverVector <- paste0("plot_hover_vector_", id)
	hoverState <- paste0("plot_hover_state_", id)



	dimensions <- session$pithya$approximatedModel$model$varNames
	
	# Render plot UI
	output[[plotOutput]] <- renderUI({
		debug("[plot] render: ", id)
		explorerPlot()
	})

	

	# Notify parent when remove button is clicked
	removeObserver <- observeEvent(input[[removePlot]], {
		onRemove(plotOutput)
	})

	list(
		id = id,
		outputId = plotOutput,
		destroy = function() {
			# cleanup function
			debug("[plot] destroy ", id)
			removeObserver$destroy()
		}
	)
}