source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R") 
source("plot/plot.R")      

createRectResultPlot <- function(result, id, input, session, output) {

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
	plot$state$formulaIndex <- NULL
	plot$state$selectedStates <- NULL
	plot$state$selectedParams <- NULL

	# Create a configuration object for the main plot
	plot$config <- reactive({
		debug(id, ":resultRectParamsPlot update config")
		baseConfig <- plot$baseConfig()
		formula <- plot$state$formulaIndex
		if (is.null(baseConfig) || is.null(formula)) {
			NULL
		} else {

			baseConfig$mapping <- plot$result$resultMapping[[formula]]
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
				
				validRectangles <- unique(Reduce(c, lapply(finalMapping, function(p) {
					if (p > 0) plot$result$paramValues[[p]]	else NULL
				})))

				#debug("valid rectangles")
				#debug(paste0(validRectangles, collapse = ", "))

				# If projections are turned off, apply exact param values
				for (p in 1:plot$result$paramCount) {
					v <- plot$result$varCount + p
					value <- vars[[v]]
					if (!is.null(value)) {
						validRectangles <- Filter(function(r) rectangleContains(r, p, value), validRectangles)
					}
				}

				rectangles <- validRectangles

				# Compute projection to the displayed parameters
				# Note that this will also transpose the rectangles if needed.
				rect(
					sapply(rectangles, function(r) r[2*(xp-1) + 1]),
					sapply(rectangles, function(r) r[2*(yp-1) + 1]),
					sapply(rectangles, function(r) r[2*(xp-1) + 2]),
					sapply(rectangles, function(r) r[2*(yp-1) + 2]),
					col = "forestgreen", border = NA
				)
			} else {
				# We have variable and parameter
				iVar <- if (config$x <= plot$result$varCount) config$x else config$y
				iParam <- (if (iVar == config$x) config$y else config$x) - plot$result$varCount

				validRectangles <- lapply(1:varSpaceSizes[iVar], function(v) {
					validRectangles <- unique(Reduce(c, lapply(rowProjection(finalMapping, iVar, v), function(p) {
						if (p > 0) plot$result$paramValues[[p]] else NULL
					})))	
				})

				# If projections are turned off, apply exact param values
				for (p in 1:plot$result$paramCount) {
					v <- plot$result$varCount + p
					value <- vars[[v]]
					if (!is.null(value)) {
						validRectangles <- lapply(validRectangles, function(v) {
							debug("valid rectangles")
							debug(paste0(v, collapse = ", "))
							r <- Filter(function(r) rectangleContains(r, p, value), v) 
							debug("after filtering in ", p, " for ", value)
							debug(paste0(r, collapse = ", "))
							r
						})
					}
				}

				validRectangles <- Reduce(c, lapply(1:varSpaceSizes[iVar], function(i) {
					varLow <- plot$varThresholds[[iVar]][i]
					varHigh <- plot$varThresholds[[iVar]][i+1]
					unique(lapply(validRectangles[[i]], function(r) {
						if (config$x > config$y) {	# Param is first
							c(r[2*(iParam-1) + 1], r[2*(iParam-1) + 2], varLow, varHigh)
						} else {
							c(varLow, varHigh, r[2*(iParam-1) + 1], r[2*(iParam-1) + 2])
						}
					}))
				}))

				rectangles <- validRectangles

				rect(
					sapply(rectangles, function(r) r[1]),
					sapply(rectangles, function(r) r[3]),
					sapply(rectangles, function(r) r[2]),
					sapply(rectangles, function(r) r[4]),
					col = "forestgreen", border = NA
				)
				
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