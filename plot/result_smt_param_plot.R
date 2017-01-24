source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R") 
source("plot/plot.R")      

createSmtResultPlot <- function(result, id, input, session, output) {

	varNames <- c(result$varNames, result$paramNames)
	varThresholds <- result$varThresholds
	for (p in result$paramRanges) {
		varThresholds[[length(varThresholds)+1]] <- c(p$min, p$max)
	}

	debug("base plot:")
	debug(varNames)
	debug(varThresholds)

	plot <- createBasePlot(
		varNames = varNames, 
		varThresholds = varThresholds, 
		varContinuous = sapply(varNames, function(x) x %in% result$paramNames),
		useProjections = TRUE, 
		id, input, session, output
	)

	debug(id, ":statePlot create")

	plot$result <- result
	plot$state$coverage <- session$pithya$synthesisResult$coverage
	plot$state$formulaIndex <- NULL
	plot$state$selectedStates <- NULL
	plot$state$selectedParams <- NULL

	# Create a configuration object for the main plot
	plot$config <- reactive({
		debug(id, ":resultRectParamsPlot update config")
		baseConfig <- plot$baseConfig()
		formula <- plot$state$formulaIndex
		coverage <- plot$state$coverage
		if (is.null(baseConfig) || is.null(formula) || is.null(coverage)) {
			NULL
		} else {

			baseConfig$mapping <- plot$result$resultMapping[[formula]]
			baseConfig$inverseMapping <- plot$result$resultInverseMapping[[formula]]
			baseConfig$coverage <- plot$state$coverage
			baseConfig$selectedStates <- plot$state$selectedStates
			baseConfig$selectedParams <- plot$state$selectedParams

			baseConfig$xThres <- plot$varThresholds[[baseConfig$x]]
			baseConfig$yThres <- plot$varThresholds[[baseConfig$y]]
			
			baseConfig
		}
	}) %>% debounce(200)

	# Render the actual state plot
	output[[plot$outImage]] <- renderPlot({			
		config <- plot$config()
		if (is.null(config)) {
			# TODO some loading
			debug(id, ":resultRectParamsPlot invalid config")			
		} else {
			debug(id, ":resultRectParamsPlot render plot")

			xThres <- config$xThres
			yThres <- config$yThres
			vars <- config$vars

			# Draw plot outline
			# TODO experiment with margins
			par(mar = c(2,2,2,2))
			plot(
				x = config$zoom[1,], y = config$zoom[2,],
				xlab = plot$varNames[config$x], ylab = plot$varNames[config$y],
				xaxs = "i", yaxs = "i", type = "n"
			)

			finalMapping <- config$mapping

			varSpaceSizes <- sapply(plot$result$varThresholds, function(x) length(x) - 1)

			varSpaceMask <- array(TRUE, varSpaceSizes)			

			# If there are some selected states, remove results which are not selected
			if (!is.null(config$selectedStates)) {
				varSpaceMask <- varSpaceMask & config$selectedStates
			}			

			# If projections are turned off, apply exact state values
			for (v in 1:plot$result$varCount) {
				value <- vars[[v]]
				if (!is.null(value)) {
					# Create state space with F and set it to T only on selected threshold
					valueMask <- assignRow(array(FALSE, varSpaceSizes), v, value, TRUE)
					varSpaceMask <- varSpaceMask & valueMask
				}
			}

			finalMapping <- ifelse(varSpaceMask, finalMapping, 0) 

			if (plot$varContinuous[config$x] && plot$varContinuous[config$y]) {
				# We have two parameters!

				xp <- config$x - plot$result$varCount
				yp <- config$y - plot$result$varCount
				
				pValues <- unique(Reduce(c, lapply(finalMapping, function(p) if (p > 0) p else NULL)))

				debug(pValues)
				
				validity <- Reduce(function(a,b) a + b, lapply(pValues, function(p) { 
					config$coverage$data[[p]] * config$inverseMapping[p]
				}))
				
				# If projections are turned off, apply exact param values
				for (p in 1:plot$result$paramCount) {
					v <- plot$result$varCount + p
					value <- vars[[v]]					
					if (!is.null(value)) {
						#TODO
						#thresholds <- config$coverage$thresholds[[v]]
						#thresholdIndex <- 1
						#for (i in 2:length(thresholds)) {
						#	if (thresholds[i-1] <= value && value <= thresholds[i]) {
						#		thresholdIndex <- i - 1
						#	}
						#}
						#validity <- rowProjection(validity, p, thresholdIndex)
						#validRectangles <- Filter(function(r) rectangleContains(r, p, value), validRectangles)
					}
				}

				maxValidity <- max(validity)
				alpha <- validity / maxValidity

				xpThres <- config$coverage$thresholds[[xp]]				
				ypThres <- config$coverage$thresholds[[yp]]

				xCount <- length(xpThres)
				yCount <- length(ypThres)

				xLow <- replicate(yCount, xpThres[-xCount])
				xHigh <- replicate(yCount, xpThres[-1])
				yLow <- t(replicate(xCount, ypThres[-yCount]))
				yHigh <- t(replicate(xCount, ypThres[-1]))

				validity <- validity > 0

				# Compute projection to the displayed parameters
				# Note that this will also transpose the rectangles if needed.
				rect(
					xLow[validity], yLow[validity], xHigh[validity], yHigh[validity],
					col = rgb(0, 0.5, 0, alpha = alpha[validity]), border = NA
				)
			} else {
				# We have variable and parameter
				iVar <- if (config$x <= plot$result$varCount) config$x else config$y
				iParam <- (if (iVar == config$x) config$y else config$x) - plot$result$varCount				
			}

			# Draw threshold lines
			# Draw last to ensure they are visible above parameter space
			abline(v = xThres, h = yThres)		
		}
	}, height = function() { session$shiny$clientData[[paste0("output_",plot$outImage,"_width")]] })	

	plot$destroy <- function() {
		plot$baseDestroy()
		debug(id, ":statePlot destroy")

		output[[plot$outImage]] <- renderPlot({ "Destroyed" })
	}	

	plot
}