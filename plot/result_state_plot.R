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

	debug(id, ":statePlot create")

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
			debug(id, ":statePlot invalid config")			
		} else {
			debug(id, ":statePlot render plot")

			xThres <- config$xThres
			yThres <- config$yThres

			# Draw plot outline
			# TODO experiment with margins
			par(mar = c(2,2,2,2))
			plot(
				x = config$zoom[1,], y = config$zoom[2,],
				xlab = plot$varNames[config$x], ylab = plot$varNames[config$y],
				xaxs = "i", yaxs = "i", type = "n"
			)

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
				selectedProjection <- projection & do.call("[", append(list(config$selectedStates, drop = TRUE), dimensionMask))
				rect(xLow[selectedProjection], yLow[selectedProjection], xHigh[selectedProjection], yHigh[selectedProjection], 
					col = first_formula_color_clicked, border = "black", lwd = 1.5
				)			
			}			

			# Draw selected params 
			if (!is.null(config$selectedParams)) {
				p <- config$selectedParams
				selectedParamStates <- apply(projectedValues, c(1,2), function(v) v > 0 && p[v])
				rect(xLow[selectedParamStates], yLow[selectedParamStates], xHigh[selectedParamStates], yHigh[selectedParamStates], 
					border = "blue", lwd = 2
				)					
			}	
		}
	}, height = function() { session$shiny$clientData[[paste0("output_",plot$outImage,"_width")]] })
	
	# Add/remove to selected items when selection changes
	plot$.selectionUpdate <- observe({
		sel <- plot$state$selection
		config <- isolate(plot$baseConfig())
		if (is.null(sel) || is.null(config)) {
			plot$state$selectedStates <- NULL
		} else {
			stateSpaceSizes <- sapply(plot$varThresholds, function(x) length(x) - 1)
			current <- unwrapOr(isolate(plot$state$selectedStates), array(FALSE, stateSpaceSizes))
			vars <- config$vars
			vars[[config$x]] <- plot$resolveStateIndex(config$x, sel$x)
			vars[[config$y]] <- plot$resolveStateIndex(config$y, sel$y)

			# TODO: can we also deselect using this? 
			# Problem: how to tell fast if the selection is empty
			#currentValue <- do.call("[", append(list(current), vars))
			plot$state$selectedStates <- do.call("[<-", append(list(current), append(vars, TRUE)))
		}
	})

	plot$destroy <- function() {
		plot$baseDestroy()
		debug(id, ":statePlot destroy")

		plot$.selectionUpdate()$destroy()

		output[[plot$outImage]] <- renderPlot({ "Destroyed" })
	}	

	plot
}