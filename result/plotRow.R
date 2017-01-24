source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       
source("result/ui.R")
source("plot/result_state_plot.R")
source("plot/result_param_plot.R")

createResultPlotRow <- function(id, result, input, session, output,
	onRemove = function(row) {}		# called when row remove button is clicked
) {

	row <- list()
	row$id <- id
	row$result <- result

	row$params <- createResultParamPlot(result, session$pithya$nextId(), input, session, output)
	row$states <- createResultStatePlot(result, session$pithya$nextId(), input, session, output)
	row$states$state$formulaIndex <- 1

	row$remove <- paste0("row_remove_", id)
	row$hide <- paste0("row_hide_", id)
	row$formula <- paste0("row_formula_", id)
	row$xDimParams <- paste0("row_x_dim_params_", id)
	row$yDimParams <- paste0("row_y_dim_params_", id)
	row$xDimStates <- paste0("row_x_dim_states_", id)
	row$yDimStates <- paste0("row_y_dim_states_", id)

	row$outRow <- paste0("row_output_", id)

	# States plot
	# Update select input to ensure only different values can be selected
	row$.statesDimSelectUpdate <- observeEvent(c(input[[row$xDimStates]], input[[row$yDimStates]]), {
		debug("[row] update states dimension selectors")	
		if (length(row$result$varNames) == 1) {
			updateSelectInput(session$shiny, row$xDimStates, choices = row$result$varNames, selected = row$result$varNames[1])
			updateSelectInput(session$shiny, row$yDimStates, choices = list("none"), selected = "none")
		} else {
			xSelected <- input[[row$xDimStates]]
			ySelected <- input[[row$yDimStates]]			
			d <- row$result$varNames			
			yOptions <- d[!d==xSelected]
			if (!(ySelected %in% yOptions)) {
				ySelected <- yOptions[1]
			}
			row$states$updateDimensions(x = xSelected, y = ySelected)
			updateSelectInput(session$shiny, row$xDimStates, choices = row$result$varNames, selected = xSelected)
			updateSelectInput(session$shiny, row$yDimStates, choices = yOptions, selected = ySelected)
		}
	})

	row$.formulaUpdate <- observeEvent(input[[row$formula]], {
		index <- match(input[[row$formula]], row$result$formulas)
		row$states$state$formulaIndex <- index
		row$params$state$formulaIndex <- index
	})

	row$.syncSelectedStates <- observe(
		row$params$state$selectedStates <- row$state$state$selectedStates		
	)

	row$.syncSelectedParams <- observe(
		row$states$state$selectedParams <- row$params$state$selectedParams
	)

	# Params plot 
	# Update select input to ensure no duplicates and one value is always parameter
	row$.paramsDimSelectUpdate <- observeEvent(c(input[[row$xDimParams]], input[[row$yDimParams]]), {
		debug("[row] update params dimension selectors")	
		params <- row$result$paramNames
		vars <- row$result$varNames
		all <- c(vars, params)

		# TODO - No parameters?!
		
		xSelected <- input[[row$xDimParams]]
		ySelected <- input[[row$yDimParams]]
		xOptions <- all
		yOptions <- all
		if (xSelected %in% vars) {
			yOptions <- params
			if (!(ySelected %in% yOptions)) {
				ySelected <- yOptions[1]
			}
		} else {
			yOptions <- all[!all==xSelected]
			if (!(ySelected %in% yOptions)) {
				ySelected <- yOptions[1]
			}
		}
		row$params$updateDimensions(x = xSelected, y = ySelected)
		updateSelectInput(session$shiny, row$xDimParams, choices = xOptions, selected = xSelected)
		updateSelectInput(session$shiny, row$yDimParams, choices = yOptions, selected = ySelected)
	})

	# Notify parent when remove button is clicked
	row$.removeObserver <- observeEvent(input[[row$remove]], {
		onRemove(row)
	})

	# Render plot UI
	output[[row$outRow]] <- renderUI({
		debug("[row] render row UI: ", id)
		resultRow(row, input)
	})

	row$destroy <- function() {
		debug("[plot row] destroy ", id)
		row$.statesDimSelectUpdate$destroy()
	}

	row

}