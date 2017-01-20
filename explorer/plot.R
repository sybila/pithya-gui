source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       
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


	plot$model <- session$pithya$approximatedModel$model
	plot$vectorDimensionSliders <- sapply(plot$model$varNames, function(d) paste0("plot_slider_vector_", d, "_", id))
	plot$stateDimensionSliders <- sapply(plot$model$varNames, function(d) paste0("plot_slider_state_", d, "_", id))
	
	# Render plot UI
	output[[plot$plotOutput]] <- renderUI({
		debug("[plot] render: ", id)
		explorerPlot(plot)
	})

	# Dimensions without the currently selected. 
	# Debounced for 500ms to ensure selector adjustment is done.
	# TODO can we write our own filter instead of debounce?
	# TODO make a new debounce that can be actually destroyed...
	missingDimensions <- reactive({
			dim <- plot$model$varNames
			dim <- dim[!dim==input[[plot$xDimSelect]]]	
			missing <- dim[!dim==input[[plot$yDimSelect]]]
			if (length(missing) > 0) {
				debug("[plot] new dimension sliders: ", paste0(missing, collapse = ", "))
			}
			missing
	}) %>% debounce(200)

	# Render vector dimension sliders based on missing dimensions
	output[[plot$scaleVector]] <- renderUI({					
		lapply(missingDimensions(), function(dim) {
			#debug("[plot] render vector dimension slider: ", dim)
			index <- match(dim, plot$model$varNames)
			range <- plot$model$varRanges[[index]]
			tooltip(tooltip = Explorer_VF_ScaleSlider_tooltip,
                sliderInput(plot$vectorDimensionSliders[index],
                	label = paste0(Explorer_VF_ScaleSlider_label, plot$model$varNames[index]),
                	min = range$min, max = range$max, 
                	value = unwrapOr(input[[plot$vectorDimensionSliders[index]]], range$min), step = scale_granularity
            	)
			)
		})
	})

	# Render state dimension sliders based on missing dimensions
	output[[plot$scaleState]] <- renderUI({
		lapply(missingDimensions(), function(dim) {
			#debug("[plot] render state dimension slider: ", dim)
			index <- match(dim, plot$model$varNames)
			tooltip(tooltip = Explorer_SS_ScaleSlider_tooltip,
				sliderInput(plot$stateDimensionSliders[index],
					label = paste0(Explorer_SS_ScaleSlider_label, plot$model$varNames[index]),
					min = 1, max = length(plot$model$varThresholds[[index]]) - 1, 
					value = unwrapOr(input[[plot$stateDimensionSliders[index]]],1), step = 1
				)
			)
		})		
	})

	# Update slider input to ensure only different values can be selected
	dimensionSelectUpdate <- observeEvent(c(input[[plot$xDimSelect]], input[[plot$yDimSelect]]), {
		debug("[plot] update dimension selectors")	
		if (length(plot$model$varNames) == 1) {
			updateSelectInput(session$shiny, plot$xDimSelect, choices = plot$model$varNames, selected = plot$model$varNames[1])
			updateSelectInput(session$shiny, plot$yDimSelect, choices = list("none"), selected = "none")
		} else {
			xSelected <- input[[plot$xDimSelect]]
			ySelected <- input[[plot$yDimSelect]]			
			d <- plot$model$varNames			
			yOptions <- d[!d==xSelected]
			if (!(ySelected %in% yOptions)) {
				ySelected <- yOptions[1]
			}
			updateSelectInput(session$shiny, plot$xDimSelect, choices = plot$model$varNames, selected = xSelected)
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