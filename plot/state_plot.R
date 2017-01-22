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
	plot$state$reachable <- NULL

	plot$config <- reactive({
		baseConfig <- plot$baseConfig()
		params <- plot$state$params
		if (is.null(baseConfig) || is.null(params)) {
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
			debug(restrictedThresholds)

			baseConfig$restrictedModel <- list(
				varNames = plot$model$varNames,
				varRanges = lapply(restrictedThresholds, function(x) list(min = x[1], max = x[length(x)])),
				varThresholds = restrictedThresholds,
				varEQ = plot$model$varEQ,
				paramNames = plot$model$paramNames,
				paramRanges = plot$model$paramRanges
			)

			baseConfig$params <- params
			baseConfig$xThres <- plot$model$varThresholds[[baseConfig$x]]
			baseConfig$yThres <- plot$model$varThresholds[[baseConfig$y]]

			baseConfig$coloringVariant <- unwrapOr(input$colVariant, "both")
			baseConfig$arrowWidth <- unwrapOr(input$transWidth, 1.5)

			baseConfig
		}
	}) %>% debounce(200)

	# Render state dimension discrete sliders based on missing dimensions
	output[[plot$outSliders]] <- renderUI({				
		lapply(plot$missingDimensions(), function(var) {
			range <- plot$varRanges[[var]]
			tooltip(tooltip = Explorer_SS_ScaleSlider_tooltip,			
				sliderInput(plot$sliders[var],
					label = paste0(Explorer_SS_ScaleSlider_label, plot$varNames[var]),
					min = 1, max = length(plot$model$varThresholds[[var]]) - 1, 
					value = unwrapOr(isolate(input[[plot$sliders[var]]]), 1), step = 1
				)
			)			
		})
	})

	plot$resolveThresholds <- function(dim, x) {
		t <- plot$model$varThresholds[[dim]]
		for (i in 2:length(t)) {
			if (t[i-1] <= x && x <= t[i]) {
				return(c(t[i-1], t[i]))
			}
		}
		c(t[length(t) - 1], t[length(t)])
	}

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

			boundedUp <- sapply(1:plot$varCount, function(i) {
				t <- plot$model$varThresholds[[i]]
				# dimension is either rendered or is set to highest threshold
				i == config$x || i == config$y || config$vars[[i]] == length(t)-1
			})

			boundedDown <- sapply(1:plot$varCount, function(i) {
				# dimension is either rendered or is set to lowest threshold
				i == config$x || i == config$y || config$vars[[i]] == 1
			})

			transitions <- computeTransitions(config$restrictedModel, config$params, boundedUp, boundedDown)
			
			xUp <- transitions$trans[[config$x]]$up
			xDown <- transitions$trans[[config$x]]$down
			yUp <- transitions$trans[[config$y]]$up
			yDown <- transitions$trans[[config$y]]$down
			loop <- transitions$loop

			# Note: the trick with [xUp] works only because all other dimensions in restricted model have size 1			
			arrows(
				xStates[xUp], yStates[xUp], replicate(yStateCount, xThres[-1])[xUp], yStates[xUp],
				angle = 20, length = 0.07, lty = transitions_line_type, lwd = config$arrowWidth,
				col = if (config$coloringVariant %in% c("both", "horizontal")) positive_color else neutral_color
			)
			arrows(
				xStates[xDown], yStates[xDown], replicate(yStateCount, xThres[-xThresholdCount])[xDown], yStates[xDown],
				angle = 20, length = 0.07, lty = transitions_line_type, lwd = config$arrowWidth,
				col = if (config$coloringVariant %in% c("both", "horizontal")) negative_color else neutral_color
			)	
			arrows(
				xStates[yUp], yStates[yUp], xStates[yUp], t(replicate(xStateCount, yThres[-1]))[yUp],
				angle = 20, length = 0.07, lty = transitions_line_type, lwd = config$arrowWidth,
				col = if (config$coloringVariant %in% c("both", "vertical")) positive_color else neutral_color
			)
			arrows(
				xStates[yDown], yStates[yDown], xStates[yDown], t(replicate(xStateCount, yThres[-yThresholdCount]))[yDown],
				angle = 20, length = 0.07, lty = transitions_line_type, lwd = config$arrowWidth,
				col = if (config$coloringVariant %in% c("both", "vertical")) negative_color else neutral_color
			)

			# Loops
			points(xStates[loop], yStates[loop], pch = 19)
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

		output[[plot$outImage]] <- renderPlot({ "Destroyed" })
	}	

	plot
}