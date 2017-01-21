source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       
source("explorer/ui.R")


plotOutputId <- function(id) { paste0("plot_output_", id) }

## Initializes a plot into the input/output objects and returns a plot
# plot = list(id = num, outputId = string, destroy = function(), ...)
createPlot <- function(id, input, session, output, onRemove = function(outputId) {}) {
	debug("[createPlot] create new plot ", id)

	plot <- list()

	plot$id <- id

	# Input element IDs	
	plot$removePlot <- paste0("plot_remove_", id)
	plot$hidePlot <- paste0("plot_hide_", id)
	plot$xDimSelect <- paste0("plot_dimen_x_", id)
	plot$yDimSelect <- paste0("plot_dimen_y_", id)
	plot$applyVectorToAll <- paste0("plot_apply_vector_to_all", id)
	plot$applyStateToAll <- paste0("plot_apply_state_to_all", id)
	plot$applyVectorToState <- paste0("plot_apply_vector_to_state_", id)
	plot$applyStateToVector <- paste0("plot_apply_state_to_vector_", id)	# not used
	plot$clearVector <- paste0("plot_clear_vector_", id)
	plot$clearState <- paste0("plot_clear_state_", id)
	plot$unzoomVector <- paste0("plot_unzoom_vector_", id)
	plot$unzoomState <- paste0("plot_unzoom_state_", id)
	plot$usePWMA <- paste0("plot_pwma_", id)
	plot$exactVector <- paste0("plot_exact_vector_", id)
	plot$exactState <- paste0("plot_exact_state_", id)	

	# Output IDs
	plot$plotOutput <- plotOutputId(id)
	plot$scaleVector <- paste0("plot_scale_vector_", id)
	plot$scaleState <- paste0("plot_scale_state_", id)
	plot$plotVector <- paste0("plot_vector_", id)
	plot$plotState <- paste0("plot_state_", id)
	
	# Event inputs
	plot$clickVector <- paste0("plot_click_vector_", id)
	plot$clickState <- paste0("plot_click_state_", id)
	plot$doubleClickVector <- paste0("plot_double_click_vector_", id)
	plot$doubleClickState <- paste0("plot_double_click_state_", id)
	plot$brushVector <- paste0("plot_brush_vector_", id)
	plot$brushState <- paste0("plot_brush_state_", id)
	plot$hoverVector <- paste0("plot_hover_vector_", id)
	plot$hoverState <- paste0("plot_hover_state_", id)


	plot$model <- session$pithya$approximatedModel$model
	plot$vectorDimensionSliders <- sapply(plot$model$varNames, function(d) paste0("plot_slider_vector_", d, "_", id))
	plot$stateDimensionSliders <- sapply(plot$model$varNames, function(d) paste0("plot_slider_state_", d, "_", id))

	plot$r <- reactiveValues(
		zoom = NULL,
		flowOrigin = NULL,
		flow = NULL
	)

	# TODO Clean up all the observers!

	# Dimensions without the currently selected. 
	# Debounced for 200ms to ensure selector adjustment is done.
	# TODO can we write our own filter instead of debounce?
	# TODO make a new debounce that can be actually destroyed...
	missingDimensions <- reactive({
			dim <- plot$model$varNames
			dim <- dim[!dim==input[[plot$xDimSelect]]]	
			missing <- dim[!dim==input[[plot$yDimSelect]]]
			if (length(missing) > 0) {
				debug("[plot] new dimension sliders: ", paste0(missing, collapse = ", "))
			}
			missing
	}) %>% debounce(200)

	activeDimensions <- reactive({
		xName <- input[[plot$xDimSelect]]
		yName <- input[[plot$yDimSelect]]
		if (is.null(xName) || is.null(yName)) {
			NULL
		} else {
			list(x = xName, y = yName, 
				iX = match(xName, plot$model$varNames), 
				iY = match(yName, plot$model$varNames)
			)
		}
	}) %>% debounce(200)

	# Update flow points when origin or rendering params change 
	observeEvent(c(plot$r$flowOrigin, input$flow_points_count, input$flow_points_density), {
		click <- plot$r$flowOrigin
		if (!is.null(click)) {
			debug("[plot] recomupte flow ", id)
			# TODO One dimensional flow
			dim <- activeDimensions()
			density <- input$flow_points_density
			params <- lapply(1:length(plot$model$paramNames), function(p) { input[[paste0("param_slider_", p)]] })
			flow <- replicate(length(plot$model$varNames), replicate(input$flow_points_count, 0))
			flow[1,] <- sapply(1:length(plot$model$varNames), function(var) {
				if (var == dim$iX) { click$x }
				else if (var == dim$iY) { click$y }
				else { isolate(input[[plot$vectorDimensionSliders[var]]]) }
			})
			for (i in 2:input$flow_points_count) {
				vars <- flow[i-1,]
				for (v in 1:length(plot$model$varNames)) {
					flow[i,v] <- vars[v] + density * plot$model$varEQ[[v]](vars, params)
				}				
			}
			plot$r$flow <- flow
		} else {
			plot$r$flow <- NULL
		}
	})	

	# Update flow origin on double click
	observeEvent(input[[plot$doubleClickVector]], {
		click <- input[[plot$doubleClickVector]]
		if (!is.null(click$x) && !is.null(click$y))	{
			plot$r$flowOrigin <- list(x = click$x, y = click$y)
		}
	})

	# Remove flow origin when clear button is clicked
	observeEvent(input[[plot$clearVector]], {
		plot$r$flowOrigin <- NULL
	})

	# Gather all input values, verify that they are valid and return one config object based on which the 
	# graph will be rendered
	vectorConfig <- reactive({
		active <- activeDimensions()
		if (is.null(active)) {
			NULL
		} else {			
			if (is.na(active$iY)) {
				# One dim
				NULL
			} else {
				xRange <- plot$model$varRanges[[active$iX]]
				yRange <- plot$model$varRanges[[active$iY]]
				zoom <- unwrapOr(plot$r$zoom, list(
					xmin = xRange$min, xmax = xRange$max, ymin = yRange$min, ymax = yRange$max
				))
				list(
					x = active$x, y = active$y, iX = active$iX, iY = active$iY, 
					zoom = zoom,
					arrowCount = input$arrows_number,
					arrowSize = input$arrowSize,
					arrowWidth = input$transWidth,
					coloringVariant = input$colVariant,
					coloringThreshold = input$colThr,
					flowPoints = plot$r$flow
				)
			}
		}
	}) %>% debounce(200)

	# Update range list when vector graph is brushed
	observeEvent(input[[plot$brushVector]], {
		brush <- input[[plot$brushVector]]
		if (!is.null(brush$xmin) && !is.null(brush$ymin) && !is.null(brush$xmax) && !is.null(brush$ymax)) {
			plot$r$zoom <- list(
				xmin = brush$xmin, xmax = brush$xmax, ymin = brush$ymin, ymax = brush$ymax
			)
		}		
	})

	# Cancel zoom when unzoom is clicked or dimensions change
	observeEvent(c(activeDimensions, input[[plot$unzoomVector]]), {
		plot$r$zoom <- NULL
	})

	# Render vector dimension sliders based on missing dimensions
	output[[plot$scaleVector]] <- renderUI({					
		lapply(missingDimensions(), function(dim) {
			#debug("[plot] render vector dimension slider: ", dim)
			index <- match(dim, plot$model$varNames)
			range <- plot$model$varRanges[[index]]
			tooltip(tooltip = Explorer_VF_ScaleSlider_tooltip,
                sliderInput(plot$vectorDimensionSliders[index],
                	label = paste0(Explorer_VF_ScaleSlider_label, plot$model$varNames[index]),
                	min = range$min, max = range$max, 
                	value = unwrapOr(isolate(input[[plot$vectorDimensionSliders[index]]]), range$min), step = scale_granularity
            	)
			)
		})
	})

	# Render state dimension sliders based on missing dimensions
	output[[plot$scaleState]] <- renderUI({
		lapply(missingDimensions(), function(dim) {
			#debug("[plot] render state dimension slider: ", dim)
			index <- match(dim, plot$model$varNames)
			tooltip(tooltip = Explorer_SS_ScaleSlider_tooltip,
				sliderInput(plot$stateDimensionSliders[index],
					label = paste0(Explorer_SS_ScaleSlider_label, plot$model$varNames[index]),
					min = 1, max = length(plot$model$varThresholds[[index]]) - 1, 
					value = unwrapOr(isolate(input[[plot$stateDimensionSliders[index]]]),1), step = 1
				)
			)
		})		
	})

	# Update slider input to ensure only different values can be selected
	dimensionSelectUpdate <- observeEvent(c(input[[plot$xDimSelect]], input[[plot$yDimSelect]]), {
		debug("[plot] update dimension selectors")	
		if (length(plot$model$varNames) == 1) {
			updateSelectInput(session$shiny, plot$xDimSelect, choices = plot$model$varNames, selected = plot$model$varNames[1])
			updateSelectInput(session$shiny, plot$yDimSelect, choices = list("none"), selected = "none")
		} else {
			xSelected <- input[[plot$xDimSelect]]
			ySelected <- input[[plot$yDimSelect]]			
			d <- plot$model$varNames			
			yOptions <- d[!d==xSelected]
			if (!(ySelected %in% yOptions)) {
				ySelected <- yOptions[1]
			}
			updateSelectInput(session$shiny, plot$xDimSelect, choices = plot$model$varNames, selected = xSelected)
			updateSelectInput(session$shiny, plot$yDimSelect, choices = yOptions, selected = ySelected)
		}
	})

	# Render vector plot
	output[[plot$plotVector]] <- renderPlot({		
		config <- vectorConfig()
		if (!is.null(config)) {
			debug("[plot] render plot: ", id)
			if (is.na(config$iY)) {
				# one dim plot
			} else {
				# one dim plot
				# TODO experiment with margins
				par(mar = c(2,2,2,2))
				plot(
					x = c(config$zoom$xmin, config$zoom$xmax), 
					y = c(config$zoom$ymin, config$zoom$ymax), 					
					xlab = config$x,
					ylab = config$y,
					xaxs = "i", yaxs = "i", type = "n"
				)
				one <- replicate(config$arrowCount, replicate(config$arrowCount, 1))
				xRange <- config$zoom$xmax - config$zoom$xmin
				yRange <- config$zoom$ymax - config$zoom$ymin
				xDim <- t(replicate(config$arrowCount, sapply(1:config$arrowCount, function(i) (i-1)/(config$arrowCount-1) * xRange + config$zoom$xmin)))
				yDim <- replicate(config$arrowCount, sapply(1:config$arrowCount, function(i) (i-1)/(config$arrowCount-1) * yRange + config$zoom$ymin))
				vars <- lapply(1:length(plot$model$varNames), function(var) {
					if (var == config$iX) { xDim }	
					else if (var == config$iY) { yDim }
					else {
						one * input[[plot$vectorDimensionSliders[var]]]
					}
				})
				params <- lapply(1:length(plot$model$paramNames), function(p) {
					one * input[[paste0("param_slider_", p)]]
				})
				dx <- one * plot$model$varEQ[[config$iX]](vars, params)
				dy <- one * plot$model$varEQ[[config$iY]](vars, params)
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
				if (!is.null(config$flowPoints)) {
					lines(config$flowPoints[,1], config$flowPoints[,2], col = "blue", lwd = size_of_flow_points)
				}				
			}
		}
	}, height = function() { session$shiny$clientData[[paste0("output_",plot$plotVector,"_width")]] })

	# Update exact position on plot hover
	output[[plot$exactVector]] <- renderPrint({
		config <- vectorConfig()
		if (!is.null(config)) {
			lines <- sapply(1:length(plot$model$varNames), function (v) {				
				if (v == config$iX) {
					value <- input[[plot$hoverVector]]$x
					if (is.null(value)) {
						value <- printInterval(config$zoom$xmin, config$zoom$xmax)
					} else {
						value <- round(value, digits = 3)			
					}										
				} else if (v == config$iY) {
					value <- input[[plot$hoverVector]]$y
					if (is.null(value)) {
						value <- printInterval(config$zoom$ymin, config$zoom$ymax)
					} else {
						value <- round(value, digits = 3)			
					}		
				} else {
					value <- input[[plot$vectorDimensionSliders[v]]]
				}
				paste0(plot$model$varNames[v], ": ", value)
			})
			cat(paste0(lines, collapse = "\n"))
		} else {
			cat("...")
		}
	})

	# Render plot UI
	output[[plot$plotOutput]] <- renderUI({
		debug("[plot] render UI: ", id)
		explorerPlot(plot)
	})

	# Notify parent when remove button is clicked
	removeObserver <- observeEvent(input[[plot$removePlot]], {
		onRemove(plot$plotOutput)
	})

	plot$destroy <- function() {
		# cleanup function
		debug("[plot] destroy ", id)
		removeObserver$destroy()
		dimensionSelectUpdate$destroy()
		output[[plot$exactVector]] <- renderUI({})
	}

	plot
}