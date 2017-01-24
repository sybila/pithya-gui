source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R") 
source("plot/plot.R")      

createResultParamPlot <- function(result, id, input, session, output) {

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
	plot$state$coverage <- isolate(session$pithya$synthesisResult$coverage)
	plot$state$formulaIndex <- NULL
	plot$state$selectedStates <- NULL
	plot$state$selectedParams <- NULL
	plot$state$pValues <- NULL

	# Clear selection when formula changes
	observeEvent(plot$state$formulaIndex, {
		plot$state$selection <- NULL	
	})

	# Create a configuration object for the main plot
	plot$config <- reactive({
		# Erase pValues, since config changed
		plot$state$pValues <- NULL
		debug(id, ":resultParamsPlot update config")
		baseConfig <- plot$baseConfig()
		formula <- plot$state$formulaIndex
		coverage <- plot$state$coverage
		if (is.null(baseConfig) || is.null(formula) || is.null(coverage)) {
			NULL
		} else {

			baseConfig$selectedStates <- plot$state$selectedStates
			# We don't need it here and it will start looping the 
			# param validity computation.
			#baseConfig$selectedParams <- plot$state$selectedParams

			baseConfig$xThres <- plot$varThresholds[[baseConfig$x]]
			baseConfig$yThres <- plot$varThresholds[[baseConfig$y]]

			baseConfig$mapping <- plot$result$resultMapping[[formula]]
			baseConfig$inverseMapping <- plot$result$resultInverseMapping[[formula]]
					
			baseConfig$coverageEnabled <- unwrapOr(input$coverage_check, FALSE)
			baseConfig$coverageAlpha <- unwrapOr(input$color_alpha_coeficient, 0.9)

			needsCoverage <- plot$result$type == "smt" || baseConfig$coverageEnabled

			if (needsCoverage) {
				baseConfig$coverage <- plot$state$coverage	
			}

			if (needsCoverage && is.null(coverage)) {
				NULL
			} else {
				baseConfig
			}			
		}
	}) %>% debounce(200)

	# config - original graph config
	# mapping - displayed state -> param mapping
	# dimMask - dimensions to reduce validity to
	computeCoverageValidity <- function(config, mapping, dimMask) {
			
		# Get all valid parameter indices
		pValues <- unique(Reduce(c, mapping))
		pValues <- pValues[!pValues==0] 

		plot$state$pValues <- unique(append(pValues, isolate(plot$state$pValues)))

		if (length(pValues) == 0) {
			c()			
		} else {
			# Sum coverage of all pValues
			# Note: Sum is correct because each state is valid for only one pValue, 
			# so the summed sets are distinct.
			validity <- Reduce(function(a,b) a + b, lapply(pValues, function(p) {
				config$coverage$data[[p]] * config$inverseMapping[p]
			}))

			#Apply parameter space cuts (assuming the parameter projection is off)
			for (p in 1:plot$result$paramCount) {					
				value <- config$vars[[p + plot$result$varCount]]
				if (!is.null(value)) {
					index <- stateIndexFromValue(config$coverage$thresholds[[p]], value)
					validity <- rowProjection(validity, p, index)
				}
			}

			# Now validity has still the same number of dimensions as the original space.
			# Here we are going to reduce it to only dimensions selected in dimMask.

			validity <- apply(validity, dimMask, sum)

			# Now validity has only dimensions specified in dim mask.
			validity
		}
	}

	# same args as computeCoverageValidity, but
	# the result is list of rectangles and dimMask also specifies
	# dimensions of these rectangles.
	computeRectangularValidity <- function(config, mapping, dimMask) {

		pValues <- unique(Reduce(c, mapping))
		pValues <- pValues[!pValues==0]

		plot$state$pValues <- unique(append(pValues, isolate(plot$state$pValues)))

		if (length(pValues) == 0) {
			list()
		} else {

			validRectangles <- Reduce(function(x, y) unique(append(x, y)), lapply(pValues, function(p) {
				# list of rectangles
				plot$result$paramValues[[p]]	
			}))

			# Apply parameter space cuts (assuming projection is off)
			for (p in 1:plot$result$paramCount) {
				value <- config$vars[[p + plot$result$varCount]]
				if (!is.null(value)) {					
					validRectangles <- Filter(function(r) rectangleContains(r, p, value), validRectangles)
				}
			}

			indexMask <- c(sapply(dimMask, function(d) c(2*d-1, 2*d)))
			rect <- unique(lapply(validRectangles, function(r) r[indexMask]))			

			rect
		}		
	}

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
			par(mar = c(4,4,4,4))
			plot(
				x = config$zoom[1,], y = config$zoom[2,],
				xlab = plot$varNames[config$x], ylab = plot$varNames[config$y],
				xaxs = "i", yaxs = "i", type = "n"
			)

			#### HERE WE APPLY ALL STATE SPACE RESTRICTIONS ####

			mapping <- config$mapping

			varSpaceSizes <- sapply(plot$result$varThresholds, function(x) length(x) - 1)			

			# If there are some selected states, remove results which are not selected
			if (!is.null(config$selectedStates)) {
				mapping <- ifelse(config$selectedStates, mapping, 0)
			}			

			# Apply state space cuts (assuming the projection is off)
			for (v in 1:plot$result$varCount) {
				value <- vars[[v]]
				# Note: value is the state index
				if (!is.null(value)) {
					mapping <- rowProjection(mapping, v, value)					
				}
			}
			
			if (!is.null(config$coverage)) {

				# If coverage is not null, it means we are drawing either an smt plot or rect. 
				# plot with coverage.

				debug("render coverage")

				if (plot$varContinuous[config$x] && plot$varContinuous[config$y]) {
					# We have two parameters to render

					xp <- config$x - plot$result$varCount
					yp <- config$y - plot$result$varCount

					# Compute 2D validity (with proper transposition)
					validity <- computeCoverageValidity(config, mapping, c(xp, yp))

					if (length(validity) > 0) {
						if (config$coverageEnabled) {
							maxValidity <- max(validity)
							alpha <- (validity / maxValidity) * config$coverageAlpha
						} else {
							alpha <- array(1, dim(validity))
						}

						validity <- validity > 0

						xpThres <- config$coverage$thresholds[[xp]]
						ypThres <- config$coverage$thresholds[[yp]]
						xCount <- length(xpThres)
						yCount <- length(ypThres)

						# dim(validity) ensures proper transposition
						xLow <- array(xpThres[-xCount], dim(validity))
						xHigh <- array(xpThres[-1], dim(validity))
						yLow <- t(array(ypThres[-yCount], dim(validity)))
						yHigh <- t(array(ypThres[-1], dim(validity)))

						rect(
							xLow[validity], yLow[validity], xHigh[validity], yHigh[validity],
							col = rgb(0, 0.5, 0, alpha = alpha[validity]), border = NA
						)
					}					
				} else {
					# We have variable and parameter
					iVar <- if (config$x <= plot$result$varCount) config$x else config$y
					iParam <- (if (iVar == config$x) config$y else config$x) - plot$result$varCount	

					for (iT in 1:(length(plot$varThresholds[[iVar]])-1)) {

						# Reduce state space to this threshold.
						reducedMapping <- rowProjection(mapping, iVar, iT)

						# Compute 1D validity
						validity <- computeCoverageValidity(config, mapping, c(iParam))

						if (length(validity) > 0) {
							if (config$coverageEnabled) {
								maxValidity <- max(validity)
								alpha <- (validity / maxValidity) * config$coverageAlpha
							} else {
								alpha <- rep(1, length(validity))
							}

							validity <- validity > 0
						}

						varLow <- plot$varThresholds[[iVar]][iT]
						varHigh <- plot$varThresholds[[iVar]][iT+1]	

						pThres <- config$coverage$thresholds[[iParam]]
						pCount <- length(pThres)

						validity <- validity > 0

						if (config$x == iVar) {
							xLow <- rep(varLow, length(validity))
							xHigh <- rep(varHigh, length(validity))
							yLow <- pThres[-pCount]
							yHigh <- pThres[-1]
						} else {
							xLow <- pThres[-pCount]
							xHigh <- pThres[-1]
							yLow <- rep(varLow, length(validity))
							yHigh <- rep(varHigh, length(validity))							
						}

						rect(
							xLow[validity], yLow[validity], xHigh[validity], yHigh[validity],
							col = rgb(0, 0.5, 0, alpha = alpha[validity]), border = NA
						)
					}
				}
			} else {
				
				## Coverage is null, that means we have rectangle results and can use simplified rendering.

				debug("render rectangles")

				if (plot$varContinuous[config$x] && plot$varContinuous[config$y]) {
					# We have two parameters!

					xp <- config$x - plot$result$varCount
					yp <- config$y - plot$result$varCount
					
					rectangles <- computeRectangularValidity(config, mapping, c(xp, yp))

					# Compute projection to the displayed parameters
					# Note that this will also transpose the rectangles if needed.
					rect(
						sapply(rectangles, function(r) r[1]),
						sapply(rectangles, function(r) r[3]),
						sapply(rectangles, function(r) r[2]),
						sapply(rectangles, function(r) r[4]),
						col = "forestgreen", border = NA
					)
				} else {
					# We have variable and parameter
					iVar <- if (config$x <= plot$result$varCount) config$x else config$y
					iParam <- (if (iVar == config$x) config$y else config$x) - plot$result$varCount	

					for (iT in 1:(length(plot$varThresholds[[iVar]])-1)) {

						# Reduce state space to this threshold.
						reducedMapping <- rowProjection(mapping, iVar, iT)

						# Compute 1D rectangles
						intervals <- computeRectangularValidity(config, mapping, c(iParam))

						varLow <- plot$varThresholds[[iVar]][iT]
						varHigh <- plot$varThresholds[[iVar]][iT+1]	

						if (config$x == iVar) {
							rect(
								sapply(intervals, function(r) varLow),
								sapply(intervals, function(r) r[1]),
								sapply(intervals, function(r) varHigh),
								sapply(intervals, function(r) r[2]),
								col = "forestgreen", border = NA
							)
						} else {
							rect(
								sapply(intervals, function(r) r[1]),
								sapply(intervals, function(r) varLow),
								sapply(intervals, function(r) r[2]),
								sapply(intervals, function(r) varHigh),
								col = "forestgreen", border = NA
							)
						}						
					}
					
				}


			}

			sel <- config$selection			
			if (!is.null(sel)) {
				debug("selection: ", sel)
				if (plot$varContinuous[config$x] && plot$varContinuous[config$y]) {
					points(sel$x, sel$y, 
						col=param_space_clicked_point$color, pch=param_space_clicked_point$type, ps=param_space_clicked_point$size, lwd=param_space_clicked_point$width
					)
				} else if (plot$varContinuous[config$x]) {
					abline(v = sel$x, col = param_space_clicked_point$color)
				} else {
					abline(h = sel$y, col = param_space_clicked_point$color)
				}
			}		

			# Draw threshold lines
			# Draw last to ensure they are visible above parameter space
			abline(v = xThres, h = yThres)		
		}
	}, height = function() { session$shiny$clientData[[paste0("output_",plot$outImage,"_width")]] })	

	# Recompute selected parameters when pValues of selection changes
	observe({
		pValues <- plot$state$pValues
		sel <- plot$state$selection
		config <- isolate(plot$config())
		if (is.null(pValues) || is.null(sel) || is.null(config)) {
			plot$state$selectedParams <- NULL
		} else {
			vars <- config$vars
			if (plot$varContinuous[config$x]) {
				vars[[config$x]] <- sel$x				
			} else {				
				vars[[config$x]] <- plot$resolveStateIndex(config$x, sel$x)
			}
			if (plot$varContinuous[config$y]) {				
				vars[[config$y]] <- sel$y
			} else {				
				vars[[config$y]] <- plot$resolveStateIndex(config$y, sel$y)
			}
			
			if (!is.null(config$coverage)) {	# We are using coverage!
				coverage <- config$coverage				
				pValues <- Filter(function(p) {
					validity <- coverage$data[[p]]					
					# Apply parameter space cuts
					for (p in 1:plot$result$paramCount) {
						value <- vars[[p + plot$result$varCount]]						
						if (!is.null(value)) {							
							index <- stateIndexFromValue(coverage$thresholds[[p]], value)
							validity <- rowProjection(validity, p, index)
						}
					}
					any(validity)
				}, pValues)		
				plot$state$selectedParams <- sapply(1:length(plot$result$paramValues), function(p) {
					p %in% pValues	
				})
			} else {
				pValues <- Filter(function(p) {				
					for (rect in plot$result$paramValues[[p]]) {
						valid <- TRUE
						for (p in 1:plot$result$paramCount) {
							value <- vars[[p + plot$result$varCount]]
							if (!is.null(value)) {
								valid <- valid && rectangleContains(rect, p, value)								
							}
						}
						if (valid) {
							return(TRUE)
						}
					}			
					FALSE
				}, pValues)
				plot$state$selectedParams <- sapply(1:length(plot$result$paramValues), function(p) {
					p %in% pValues	
				})
			}
		}
	})

	plot$destroy <- function() {
		plot$baseDestroy()
		debug(id, ":statePlot destroy")

		output[[plot$outImage]] <- renderPlot({ "Destroyed" })
	}	

	plot
}