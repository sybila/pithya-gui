source("config.R")          # global configuration
source("tooltips.R")        # texts
source("explorer/ui.R")

plotOutputId <- function(id) { paste0("plot_output_", id) }

## Initializes a plot into the input/output objects and returns a plot
# plot = list(id = num, outputId = string, destroy = function(), ...)
createPlot <- function(id, input, session, output, onRemove = function(outputId) {}) {
	debug("[createPlot] create new plot ", id)

	plot <- list()

	plot$id <- id

	# Input element IDs	
	plot$removePlot <- paste0("plot_remove_", id)
	plot$hidePlot <- paste0("plot_hide_", id)
	plot$xDimSelect <- paste0("plot_dimen_x_", id)
	plot$yDimSelect <- paste0("plot_dimen_y_", id)
	plot$applyVectorToAll <- paste0("plot_apply_vector_to_all", id)
	plot$applyStateToAll <- paste0("plot_apply_state_to_all", id)
	plot$applyVectorToState <- paste0("plot_apply_vector_to_state_", id)
	plot$applyStateToVector <- paste0("plot_apply_state_to_vector_", id)	# not used
	plot$clearVector <- paste0("plot_clear_vector_", id)
	plot$clearState <- paste0("plot_clear_state_", id)
	plot$unzoomVector <- paste0("plot_unzoom_vector_", id)
	plot$unzoomState <- paste0("plot_unzoom_state_", id)
	plot$usePWMA <- paste0("plot_pwma_", id)
	plot$exactVector <- paste0("plot_exact_vector_", id)
	plot$exactState <- paste0("plot_exact_state_", id)
	plot$plotVector <- paste0("plot_vector_", id)
	plot$plotState <- paste0("plot_state_", id)

	# Output IDs
	plot$plotOutput <- plotOutputId(id)
	plot$scaleVector <- paste0("plot_scale_vector_", id)
	plot$scaleState <- paste0("plot_scale_state_", id)
	
	# Event inputs
	plot$clickVector <- paste0("plot_click_vector_", id)
	plot$clickState <- paste0("plot_click_state_", id)
	plot$doubleClickVector <- paste0("plot_double_click_vector_", id)
	plot$doubleClickState <- paste0("plot_double_click_state_", id)
	plot$brushVector <- paste0("plot_brush_vector_", id)
	plot$brushState <- paste0("plot_brush_state_", id)
	plot$hoverVector <- paste0("plot_hover_vector_", id)
	plot$hoverState <- paste0("plot_hover_state_", id)


	plot$dimensions <- session$pithya$approximatedModel$model$varNames
	
	# Render plot UI
	output[[plot$plotOutput]] <- renderUI({
		debug("[plot] render: ", id)
		explorerPlot(plot)
	})

	# Update slider input to ensure only different values can be selected
	dimensionSelectUpdate <- observeEvent(c(input[[plot$xDimSelect]], input[[plot$yDimSelect]]), {
		debug("[plot] update dimension selectors")	
		if (length(plot$dimensions) == 1) {
			updateSelectInput(session$shiny, plot$xDimSelect, choices = plot$dimensions, selected = plot$dimensions[1])
			updateSelectInput(session$shiny, plot$yDimSelect, choices = list("none"), selected = "none")
		} else {
			xSelected <- input[[plot$xDimSelect]]
			ySelected <- input[[plot$yDimSelect]]			
			d <- plot$dimensions			
			yOptions <- d[!d==xSelected]
			if (!(ySelected %in% yOptions)) {
				ySelected <- yOptions[1]
			}
			updateSelectInput(session$shiny, plot$xDimSelect, choices = plot$dimensions, selected = xSelected)
			updateSelectInput(session$shiny, plot$yDimSelect, choices = yOptions, selected = ySelected)
		}
	})

	# Notify parent when remove button is clicked
	removeObserver <- observeEvent(input[[plot$removePlot]], {
		onRemove(plot$plotOutput)
	})

	plot$destroy <- function() {
		# cleanup function
		debug("[plot] destroy ", id)
		removeObserver$destroy()
		dimensionSelectUpdate$destroy()
	}

	plot
}