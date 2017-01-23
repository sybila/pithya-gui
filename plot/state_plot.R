source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R") 
source("plot/plot.R")      
source("bio.R")

createStatePlot <- function(model, id, input, session, output) {

	plot <- createBasePlot(model$varNames, model$varRanges, id, input, session, output)

	debug(id, ":statePlot create")

	plot$model <- model

	plot$state$params <- NULL
	plot$state$transitions <- NULL
	plot$state$reachable <- NULL
	plot$state$transitionsConfig <- NULL

	# Utility function which return the index of the state to which the vlaue belongs
	plot$resolveStateIndex <- function(dim, x) {
		t <- plot$model$varThresholds[[dim]]
		for (i in 2:length(t)) {
			if (t[i-1] <= x && x <= t[i]) {
				return(i-1)
			}
		}
		length(t-1)
	}

	# Utility function which translated continuous values into threshold boundaries
	plot$resolveThresholds <- function(dim, x) {
		t <- plot$model$varThresholds[[dim]]
		i <- plot$resolveStateIndex(dim, x)
		c(t[i], t[i+1])		
	}

	# Create a configuration object for computing the transition system.
	plot$transitionsConfig <- reactive({
		baseConfig <- plot$baseConfig()
		params <- plot$state$params
		if (is.null(baseConfig) || is.null(params))	{
			NULL
		} else {

			# Restrict model to thresholds that are visible
			restrictedThresholds <- lapply(1:plot$varCount, function(i) {
				t <- plot$model$varThresholds[[i]]
				if (i == baseConfig$x || i == baseConfig$y) { t } else {
					index <- baseConfig$vars[[i]]
					c(t[index], t[index+1])
				}
			})
			
			restrictedModel <- list(
					varNames = plot$model$varNames,
					varRanges = lapply(restrictedThresholds, function(x) list(min = x[1], max = x[length(x)])),
					varThresholds = restrictedThresholds,
					varEQ = plot$model$varEQ,
					paramNames = plot$model$paramNames,
					paramRanges = plot$model$paramRanges
				)			

			# We can't use basConfig, becuase we don't want to get notified about zoom and selection
			newConfig <- list(
				vars = baseConfig$vars,
				params = params,
				restrictedModel = restrictedModel,
				x = baseConfig$x, y = baseConfig$y
			)

			# Update only if changed
			if (!isTRUE(all.equal(newConfig, plot$state$transitionsConfig))) {
				debug("update transition config")
				plot$state$transitionsConfig <- newConfig
			}
		}
	}) %>% debounce(200)

	# Recompute the transition system when the configuration object changes.
	plot$.computeTransitions <- observe({
		config <- plot$state$transitionsConfig
		if (is.null(config)) {
			plot$state$transitions <- NULL
		} else {

			boundedUp <- sapply(1:plot$varCount, function(i) {
				t <- plot$model$varThresholds[[i]]
				# dimension is either rendered or is set to highest threshold
				i == config$x || i == config$y || config$vars[[i]] == length(t)-1
			})

			boundedDown <- sapply(1:plot$varCount, function(i) {
				# dimension is either rendered or is set to lowest threshold
				i == config$x || i == config$y || config$vars[[i]] == 1
			})

			withProgress(message = "Computing transitions", min = 0, max = 1, expr = {
				transitions <- computeTransitions(config$restrictedModel, config$params, boundedUp, boundedDown)
			})			

			# Note: the drop trick works only because all other dimensions in restricted model have size 1
			xUp <- drop(transitions$trans[[config$x]]$up)
			xDown <- drop(transitions$trans[[config$x]]$down)
			yUp <- drop(transitions$trans[[config$y]]$up)
			yDown <- drop(transitions$trans[[config$y]]$down)
			loop <- drop(transitions$loop)

			# Compute transitions returns results in increasing order always.
			# If current dimension ordering is reversed, we have to transpose the results
			if (config$x > config$y) {
				xUp <- t(xUp); xDown <- t(xDown)
				yUp <- t(yUp); yDown <- t(yDown)
				loop <- t(loop)
			}			
			
			plot$state$transitions <- list(
				x = list(up = xUp, down = xDown), y = list(up = yUp, down = yDown), loop = loop
			)
		}
	}, label = "computeTransitions")

	plot$.computeReachability <- observe({
		config <- plot$baseConfig()
		transitions <- plot$state$transitions
		if (is.null(config) || is.null(config$selection) || is.null(transitions)) {
			plot$state$reachable <- NULL
		} else {
			xStateCount <- length(plot$model$varThresholds[[config$x]]) - 1 
			yStateCount <- length(plot$model$varThresholds[[config$y]]) - 1
			reachable <- array(0, c(xStateCount, yStateCount))
			startX <- plot$resolveStateIndex(config$x, config$selection$x)[1]
			startY <- plot$resolveStateIndex(config$y, config$selection$y)[1]
			reachable[startX, startY] <- 1
			
			# Unfortuantely, shift works only with numeric matrices, so we are going to use 
			# good old numbers for this one
			xUp <- ifelse(transitions$x$up, 1, 0)
			xDown <- ifelse(transitions$x$down, 1, 0)
			yUp <- ifelse(transitions$y$up, 1, 0)
			yDown <- ifelse(transitions$y$down, 1, 0)
			repeat {
				newReachable <- reachable
				# Note: The operations are a little weird, because down actually means up,
				# since [1,1] is first state and down shifts if to [2,1]
				newReachable <- newReachable + shift.down(newReachable * xUp, 1)
				newReachable <- newReachable + shift.up(newReachable * xDown, 1)
				newReachable <- newReachable + shift.right(newReachable * yUp, 1)
				newReachable <- newReachable + shift.left(newReachable * yDown, 1)
				newReachable <- pmin(newReachable, 1)	# reset counters to one
				if (all(newReachable == reachable)) { break } else {
					reachable <- newReachable
				}				
			}
			plot$state$reachable <- reachable > 0
		}
	}, label = "computeReachability")

	# Create a configuration object for the main plot
	plot$config <- reactive({
		baseConfig <- plot$baseConfig()
		transitions <- plot$state$transitions
		reachable <- plot$state$reachable
		if (is.null(baseConfig) || is.null(transitions)) {
			NULL
		} else {

			baseConfig$xThres <- plot$model$varThresholds[[baseConfig$x]]
			baseConfig$yThres <- plot$model$varThresholds[[baseConfig$y]]

			baseConfig$reachable <- reachable
			baseConfig$transitions <- transitions

			baseConfig$coloringVariant <- unwrapOr(input$colVariant, "both")
			baseConfig$arrowWidth <- unwrapOr(input$transWidth, 1.5)

			baseConfig
		}
	}) %>% debounce(100)

	# Render state dimension discrete sliders based on missing dimensions
	output[[plot$outSliders]] <- renderUI({				
		lapply(plot$missingDimensions(), function(var) {
			range <- plot$varRanges[[var]]
			tooltip(tooltip = Explorer_SS_ScaleSlider_tooltip,			
				sliderInput(plot$sliders[var],
					label = paste0(Explorer_SS_ScaleSlider_label, plot$varNames[var]),
					min = 1, max = length(plot$model$varThresholds[[var]]) - 1, 
					value = unwrapOr(plot$baseConfig()$vars[[var]], 1), step = 1
				)
			)			
		})
	})

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

			xCenters <- (xThres[-1] + xThres[-xThresholdCount]) / 2
			yCenters <- (yThres[-1] + yThres[-yThresholdCount]) / 2

			# Centers of states
			xStates <- replicate(yStateCount, xCenters)
			yStates <- t(replicate(xStateCount, yCenters))

			transitions <- config$transitions

			xUp <- config$transitions$x$up
			xDown <- config$transitions$x$down
			yUp <- config$transitions$y$up
			yDown <- config$transitions$y$down
			loop <- config$transitions$loop
			
			suppressWarnings(
				arrows(
					xStates[xUp], yStates[xUp], replicate(yStateCount, xThres[-1])[xUp], yStates[xUp],
					angle = 20, length = 0.07, lty = transitions_line_type, lwd = config$arrowWidth,
					col = if (config$coloringVariant %in% c("both", "horizontal")) positive_color else neutral_color
				)
			)	
			suppressWarnings(		
				arrows(
					xStates[xDown], yStates[xDown], replicate(yStateCount, xThres[-xThresholdCount])[xDown], yStates[xDown],
					angle = 20, length = 0.07, lty = transitions_line_type, lwd = config$arrowWidth,
					col = if (config$coloringVariant %in% c("both", "horizontal")) negative_color else neutral_color
				)	
			)
			suppressWarnings(
				arrows(
					xStates[yUp], yStates[yUp], xStates[yUp], t(replicate(xStateCount, yThres[-1]))[yUp],
					angle = 20, length = 0.07, lty = transitions_line_type, lwd = config$arrowWidth,
					col = if (config$coloringVariant %in% c("both", "vertical")) positive_color else neutral_color
				)
			)
			suppressWarnings(
				arrows(
					xStates[yDown], yStates[yDown], xStates[yDown], t(replicate(xStateCount, yThres[-yThresholdCount]))[yDown],
					angle = 20, length = 0.07, lty = transitions_line_type, lwd = config$arrowWidth,
					col = if (config$coloringVariant %in% c("both", "vertical")) negative_color else neutral_color
				)
			)

			# Loops
			points(xStates[loop], yStates[loop], pch = 19)

			reach <- config$reachable
			if (!is.null(reach)) {
				xLow <- replicate(yStateCount, xThres[-xThresholdCount])
				xHigh <- replicate(yStateCount, xThres[-1])
				yLow <- t(replicate(xStateCount, yThres[-yThresholdCount]))
				yHigh <- t(replicate(xStateCount, yThres[-1]))
				rect(xLow[reach], yLow[reach], xHigh[reach], yHigh[reach], border = "blue", lwd = 2)
			}			
		}
	}, height = function() { session$shiny$clientData[[paste0("output_",plot$outImage,"_width")]] })

	printValue <- function(dim, x) {
		t <- plot$resolveThresholds(dim, x)
		paste0("[", round(t[1], digits = 3), ", ", round(t[2], digits = 3), "]")
	}
	printInterval <- function(dim, x, y) {
		x <- plot$resolveThresholds(dim, x)
		y <- plot$resolveThresholds(dim, y)			
		paste0("[", round(x[1], digits = 3), ", ", round(y[2], digits = 3), "]")
	}

	# Print currently displayed exact values
	output[[plot$outExact]] <- plot$buildPrintExact(
		printVarsValue = printValue, printHoverValue = printValue,
		printZoomInterval = printInterval, printRangeInterval = printInterval
	)
	
	plot$destroy <- function() {
		plot$baseDestroy()
		debug(id, ":statePlot destroy")

		plot$.computeTransitions$destroy()
		output[[plot$outImage]] <- renderPlot({ "Destroyed" })
	}	

	plot
}