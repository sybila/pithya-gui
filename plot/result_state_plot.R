source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R") 
source("plot/plot.R")      

createResultStatePlot <- function(result, id, input, session, output) {

	plot <- createBasePlot(
		varNames = result$varNames, 
		varThresholds = result$varThresholds, 
		varContinuous = sapply(result$varNames, function(x) FALSE),
		useProjections = FALSE, 
		id, input, session, output
	)

	debug(id, ":resultStatePlot create")

	plot$result <- result
	plot$state$formulaIndex <- NULL
	plot$state$selectedStates <- NULL
	plot$state$selectedParams <- NULL

	# Create a configuration object for the main plot
	plot$config <- reactive({
		baseConfig <- plot$baseConfig()
		formula <- plot$state$formulaIndex
		if (is.null(baseConfig) || is.null(formula)) {
			NULL
		} else {

			baseConfig$mapping <- plot$result$resultMapping[[formula]]
			baseConfig$selectedStates <- plot$state$selectedStates
			baseConfig$selectedParams <- plot$state$selectedParams

			baseConfig$xThres <- plot$result$varThresholds[[baseConfig$x]]
			baseConfig$yThres <- plot$result$varThresholds[[baseConfig$y]]
			
			baseConfig
		}
	}) %>% debounce(200)

	# Render the actual state plot
	output[[plot$outImage]] <- renderPlot({			
		config <- plot$config()
		if (is.null(config)) {
			# TODO some loading
			debug(id, ":resultStatePlot invalid config")			
		} else {
			debug(id, ":resultStatePlot render plot")

			xThres <- config$xThres
			yThres <- config$yThres

			# Draw plot outline
			# TODO experiment with margins
			plot$setupPlot(config)

			title(Result_SS_label)

			# Draw threshold lines
			abline(v = xThres, h = yThres)		

			xThresholdCount <- length(xThres)
			yThresholdCount <- length(yThres)
			xStateCount <- xThresholdCount - 1
			yStateCount <- yThresholdCount - 1

			dimensionMask <- lapply(1:plot$varCount, function(v) {
				if (v == config$x || v == config$y) {
					TRUE
				} else {
					config$vars[[v]]
				}
			})
			
			# dimensionMask ensures that all except two dimensions are reduced to trivial
			projectedValues <- do.call("[", append(list(config$mapping, drop = TRUE), dimensionMask))
			projection <- projectedValues > 0

			# If current dimension ordering is reversed, we have to transpose the results
			if (config$x > config$y) {
				projection <- t(projection)
				projectedValues <- t(projectedValues)
			}

			xLow <- replicate(yStateCount, xThres[-xThresholdCount])
			xHigh <- replicate(yStateCount, xThres[-1])
			yLow <- t(replicate(xStateCount, yThres[-yThresholdCount]))
			yHigh <- t(replicate(xStateCount, yThres[-1]))

			# Draw basic validity
			rect(xLow[projection], yLow[projection], xHigh[projection], yHigh[projection], 
				col = first_formula_color, border = "black", lwd = 1.5
			)			

			# Draw selected states
			if (!is.null(config$selectedStates)) {
				selectedProjection <- do.call("[", append(list(config$selectedStates, drop = TRUE), dimensionMask))
				if (config$x > config$y) {
					selectedProjection <- t(selectedProjection)
				}
				
				selectedProjection <- projection & selectedProjection
				rect(xLow[selectedProjection], yLow[selectedProjection], xHigh[selectedProjection], yHigh[selectedProjection], 
					col = first_formula_color_clicked, border = "black", lwd = 1.5
				)			
			}			

			# Draw selected params 
			if (!is.null(config$selectedParams)) {
				p <- config$selectedParams
				# as.matrix is here for one dimensional graphs, because drop=TRUE will cause them to have just one dimension
				selectedParamStates <- apply(as.matrix(projectedValues), c(1,2), function(v) v > 0 && p[v])
				rect(xLow[selectedParamStates], yLow[selectedParamStates], xHigh[selectedParamStates], yHigh[selectedParamStates], 
					border = "blue", lwd = 2
				)					
			}	

		}
	}, height = function() { session$shiny$clientData[[paste0("output_",plot$outImage,"_width")]] })
	
	# Add/remove to selected items when selection changes
	plot$.selectionUpdate <- observe({
		sel <- plot$state$selection
		baseConfig <- isolate(plot$baseConfig())
		plotConfig <- isolate(plot$config())
		if (is.null(sel) || is.null(baseConfig)) {
			plot$state$selectedStates <- NULL
		} else {
		    
		    # we need trueness (possible satisfiability) of mapping across all dimensions
		    projection <- do.call("[", append(list(plotConfig$mapping, drop = TRUE), rep(T,plot$varCount))) > 0

		    # If current dimension ordering is reversed, we have to transpose the results
		    if (baseConfig$x > baseConfig$y) {
		        projection <- t(projection)
		    }
		    debug("## projection")
		    debug(projection)

			stateSpaceSizes <- sapply(plot$varThresholds, function(x) length(x) - 1)
			current <- unwrapOr(isolate(plot$state$selectedStates), array(FALSE, stateSpaceSizes))
			vars <- baseConfig$vars
			vars[[baseConfig$x]] <- plot$resolveStateIndex(baseConfig$x, sel$x)
			vars[[baseConfig$y]] <- plot$resolveStateIndex(baseConfig$y, sel$y)

			currentValue <- do.call("[", append(list(current), vars))
			newValue <- do.call("[<-", append(list(current), append(vars, !currentValue)))
			debug("## newValue")
			debug(newValue)
			newValue <- newValue & projection
			
			if (any(newValue)) {
			    debug("!!! something has been chosen")
				plot$state$selectedStates <- newValue
			} else {
			    debug("!!! nohing important has been chosen")
				plot$state$selectedStates <- NULL
				plot$state$selection <- NULL
				updateButton(session$shiny, plot$buttonUnselect, disabled=T)
			}
		}
	})

	plot$destroy <- function() {
		plot$baseDestroy()
		debug(id, ":resultStatePlot destroy")

		plot$.selectionUpdate()$destroy()

		output[[plot$outImage]] <- renderPlot({ "Destroyed" })
	}	

	plot
}