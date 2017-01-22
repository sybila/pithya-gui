source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       
source("explorer/ui.R")
source("plot/vector_plot.R")
source("plot/state_plot.R")

## Initializes a plot into the input/output objects and returns a plot
# plot = list(id = num, outputId = string, destroy = function(), ...)
createPlotRow <- function(id, model, params, input, session, output, 
	onApplyAllVector = function(sel) {}, 	# called when apply to all button is clicked next to the vector plot
	onApplyAllState = function(sel) {},		# called when apply to all button is clicked next to the state plot
	onRemove = function(row) {}				# called when row is removed
) {
	debug("[createPlot] create plot row ", id)

	row <- list()
	row$id <- id
	row$model <- model

	row$vector <- createVectorPlot(model, NULL, session$pithya$nextId(), input, session, output)
	row$vector$state$params <- params
	row$state <- createStatePlot(model, session$pithya$nextId(), input, session, output)
	row$state$state$params <- params

	# Input elements
	row$remove <- paste0("row_remove_", id)
	row$hide <- paste0("row_hide_", id)
	row$xDimSelect <- paste0("row_dimen_x_", id)
	row$yDimSelect <- paste0("row_dimen_y_", id)
	row$applyVectorToAll <- paste0("row_apply_vector_to_all_", id)
	row$applyStateToAll <- paste0("row_apply_state_to_all_", id)
	row$applyVectorToState <- paste0("row_apply_vector_to_state_", id)

	# Output elements
	row$outRow <- paste0("row_output_", id)

	# Update slider input to ensure only different values can be selected
	row$.dimensionSelectUpdate <- observeEvent(c(input[[row$xDimSelect]], input[[row$yDimSelect]]), {
		debug("[row] update dimension selectors")	
		if (length(row$model$varNames) == 1) {
			updateSelectInput(session$shiny, row$xDimSelect, choices = row$model$varNames, selected = row$model$varNames[1])
			updateSelectInput(session$shiny, row$yDimSelect, choices = list("none"), selected = "none")
		} else {
			xSelected <- input[[row$xDimSelect]]
			ySelected <- input[[row$yDimSelect]]			
			d <- row$model$varNames			
			yOptions <- d[!d==xSelected]
			if (!(ySelected %in% yOptions)) {
				ySelected <- yOptions[1]
			}
			row$vector$updateDimensions(x = xSelected, y = ySelected)
			row$state$updateDimensions(x = xSelected, y = ySelected)
			updateSelectInput(session$shiny, row$xDimSelect, choices = row$model$varNames, selected = xSelected)
			updateSelectInput(session$shiny, row$yDimSelect, choices = yOptions, selected = ySelected)
		}
	})

	# Render plot UI
	output[[row$outRow]] <- renderUI({
		debug("[row] render row UI: ", id)
		plotRow(row)
	})

	# Notify parent when remove button is clicked
	row$.removeObserver <- observeEvent(input[[row$remove]], {
		onRemove(row)
	})

	row$.applyToState <- observeEvent(input[[row$applyVectorToState]], {
		row$state$state$selection <- row$vector$state$selection
	})

	row$.applyToAllVector <- observeEvent(input[[row$applyVectorToAll]], {
		onApplyAllVector(row$vector$state$selection)	
	})

	row$.applyToAllState <- observeEvent(input[[row$applyStateToAll]], {
		onApplyAllState(row$state$state$selection)	
	})

	row$destroy <- function() {
		# cleanup function
		debug("[plot] destroy ", id)
		row$vector$destroy()
		row$.dimensionSelectUpdate$destroy()
		row$.removeObserver$destroy()
		row$.applyToState$destroy()
		row$.applyToAllVector$destroy()
		row$.applyToAllState$destroy()
	}

	row
}