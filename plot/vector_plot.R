source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R") 
source("plot/plot.R")      

createVectorPlot <- function(model, modelPWA, id, input, session, output) {

	plot <- createBasePlot(model$varNames, model$varRanges, id, input, session, output)

	debug(id, ":vectorPlot create")

	plot$model <- model
	plot$modelPWA <- modelPWA

	plot$state$activeModel <- model
	plot$state$params <- NULL
	plot$state$flow <- NULL

	# Gather config about plot drawing and debounce it
	plot$config <- reactive({
		baseConfig <- plot$baseConfig()
		params <- plot$state$params
		model <- plot$state$activeModel
		params <- plot$state$params
		if (is.null(baseConfig) || is.null(params) || is.null(model) || is.null(params)) {
			NULL
		} else {
			baseConfig$params <- params
			baseConfig$flow <- plot$state$flow
			baseConfig$xEQ <- model$varEQ[[baseConfig$x]]
			baseConfig$yEQ <- model$varEQ[[baseConfig$y]]

			# Display options
			baseConfig$arrowCount <- unwrapOr(input$arrows_number, 20)
			baseConfig$arrowSize <- unwrapOr(input$arrowSize, 0.5)
			baseConfig$arrowWidth <- unwrapOr(input$transWidth, 1.5)
			baseConfig$coloringVariant <- unwrapOr(input$colVariant, "both")
			baseConfig$coloringThreshold <- unwrapOr(input$colThr, 0.05)
			
			baseConfig
		}
	}) %>% debounce(200)

	# Gather config data about flow computation and debounce it
	plot$flowConfig <- reactive({
		config <- plot$baseConfig()
		selection <- plot$state$selection
		model <- plot$state$activeModel
		params <- plot$state$params		
		if (is.null(config) || is.null(selection) || is.null(model) || is.null(params)) {
			NULL
		} else {
			vars <- config$vars
			vars[[config$x]] <- selection$x
			vars[[config$y]] <- selection$y
			list(
				params = params,
				startingPoint = unlist(vars),	# vars should be complete now
				equations = model$varEQ,		# we need all equations
				pointCount = unwrapOr(input$flow_points_count, 300),
				pointDensity = unwrapOr(input$flow_points_density, 1)
			)
		}		
	}) %>% debounce(200)

	# Recompute flow when flow config changes
	plot$.flowComputer <- observe({		
		plot$state$flow <- let(plot$flowConfig(), function(config) {
			debug(id, ":vectorPlot recomupte flow ")
			flow <- replicate(plot$varCount, replicate(config$pointCount, 0))
			flow[1,] <- config$startingPoint
			for (i in 2:input$flow_points_count) {
				vars <- flow[i-1,]
				for (v in 1:plot$varCount) {
					flow[i,v] <- vars[v] + config$pointDensity * config$equations[[v]](vars, config$params)
				}				
			}
			plot$state$flow <- flow
		})
	})

	# Render vector dimension continuous sliders based on missing dimensions
	output[[plot$outSliders]] <- renderUI({
		debug(id, ":vectorPlot render sliders")
		lapply(plot$missingDimensions(), function(var) {
			range <- plot$varRanges[[var]]
			tooltip(tooltip = Explorer_VF_ScaleSlider_tooltip,
				sliderInput(plot$sliders[var],
					label = paste0(Explorer_VF_ScaleSlider_label, plot$varNames[var]),
					min = range$min, max = range$max, 
					value = unwrapOr(plot$baseConfig()$vars[[var]], range$min), 
					step = scale_granularity
				)
			)			
		})
	})

	# Render the actual vector plot
	output[[plot$outImage]] <- renderPlot({			
		config <- plot$config()
		if (is.null(config)) {
			# TODO some loading
			debug(id, ":vectorPlot invalid config...")			
		} else {
			debug(id, ":vectorPlot render plot")

			# Draw plot outline
			# TODO experiment with margins
			par(mar = c(2,2,2,2))
			plot(
				x = config$zoom[1,], y = config$zoom[2,],
				xlab = plot$varNames[config$x], ylab = plot$varNames[config$y],
				xaxs = "i", yaxs = "i", type = "n"
			)
			one <- replicate(config$arrowCount, replicate(config$arrowCount, 1))
			range <- config$zoom[,2] - config$zoom[,1]
			# Compute arrow grid
			xDim <- t(replicate(config$arrowCount, sapply(0:(config$arrowCount-1), function(i) { 
				i/(config$arrowCount-1) * range[1] + config$zoom[1,1]
			})))
			yDim <- replicate(config$arrowCount, sapply(0:(config$arrowCount-1), function(i) {
				i/(config$arrowCount-1) * range[2] + config$zoom[2,1]
			}))

			# Convert variable values to matrices
			vars <- config$vars			
			vars[[config$x]] <- xDim
			vars[[config$y]] <- yDim
			vars <- lapply(vars, function(var) one * var)			
			
			# Compute derivations (one * ensures that if the result is just number, it will be transformed to a matrix)
			dx <- one * config$xEQ(vars, config$params)
			dy <- one * config$yEQ(vars, config$params)

			# Draw plot arrows
			direction <- switch(config$coloringVariant, "both" = dx + dy, "horizontal" = dx, "vertical" = dy, "none" = one * 0)
			suppressWarnings(
				quiver(xDim, yDim, dx, dy, length=0.08, angle=30,
					scale = config$arrowSize,
					lwd = config$arrowWidth,
        			col = 	ifelse(direction > config$coloringThreshold, 
        						positive_color, 
        					ifelse(direction < -config$coloringThreshold, 
        						negative_color, 
        						neutral_color)
        					)
				)
			)

			# Draw flow if available
			let(config$flow, function(flow) {
				lines(flow[,1], flow[,2], col = "blue", lwd = size_of_flow_points)
			})
		}
	}, height = function() { session$shiny$clientData[[paste0("output_",plot$outImage,"_width")]] })


	plot$destroy <- function() {
		plot$baseDestroy()
		debug(id, ":vectorPlot destroy")

		plot$.flowComputer$destroy()		
	}	

	plot

}