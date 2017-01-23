source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       

createBasePlot <- function(varNames, varThresholds, varContinuous, useProjections, id, input, session, output) {

	debug(id, ":plot create")

	plot <- list()

	plot$id <- id
	plot$varNames <- varNames
	plot$varCount <- length(varNames)
	plot$varThresholds <- varThresholds
	plot$varContinuous <- varContinuous
	plot$varRanges <- lapply(varThresholds, function(t) list(min = t[1], max = t[length(t)]))	
	plot$useProjections <- useProjections

	# Implementation notes:
	# Zoom and selection are always exact values.
	# Vars is a value for continuous variables and low threshold index for
	# discrete variables.

	plot$state <- reactiveValues(
		dim = NULL, 			# list(x,y) of selected dimension indices
		selection = NULL, 		# last double clicked point
		zoom = NULL				# current zoom state
	)	

	# Input elements
	plot$buttonUnselect <- paste0("plot_unselect_", id)	# Cancel current selection
	plot$buttonUnzoom <- paste0("plot_unzoom_", id)		# Clear current zoom
	plot$sliders <- sapply(varNames, function(var) paste0("plot_slider_", var, "_", id))	# Specify exact position
	plot$project <- sapply(varNames, function(var) paste0("plot_project_", var, "_", id))	# Turn projection on/off

	# Output elements
	plot$outImage <- paste0("plot_image_", id)			# The plot image
	plot$outExact <- paste0("plot_exact_", id)			# Text area with exact cursor position
	plot$outSliders <- paste0("plot_sliders_", id)		# Container for variable sliders

	# Input events
	plot$eventClick <- paste0("plot_click_", id)				# Plot clicked
	plot$eventDoubleClick <- paste0("plot_double_click_", id)	# Plot double clicked
	plot$eventBrush <- paste0("plot_brush_", id)				# Plot brushed
	plot$eventHover <- paste0("plot_hover_", id)				# Plot hovered


	# Update dimensions based on their names instead of indices
	plot$updateDimensions <- function(x, y) {
		iX <- match(x, plot$varNames)
		iY <- match(y, plot$varNames)
		if (is.na(iX) || is.na(iY)) {
			plot$state$dim <- NULL
		} else {
			plot$state$dim <- list(x = iX, y = iY)
		}
	}

	# Utility function which return the index of the state to which the vlaue belongs
	plot$resolveStateIndex <- function(dim, x) {
		t <- plot$varThresholds[[dim]]
		for (i in 2:length(t)) {
			if (t[i-1] <= x && x <= t[i]) {
				return(i-1)
			}
		}
		length(t-1)
	}

	# Utility function which translated continuous values into threshold boundaries
	plot$resolveThresholds <- function(dim, x) {
		t <- plot$varThresholds[[dim]]
		i <- plot$resolveStateIndex(dim, x)
		c(t[i], t[i+1])		
	}

	# Returns vector of variable indices which are not rendered
	plot$missingDimensions <- reactive({
		debug("missingDimensions")
		if (is.null(plot$state$dim)) {
			NULL
		} else {
			(1:plot$varCount)[-c(plot$state$dim$x, plot$state$dim$y)]	
		}		
	})

	# Compute basic configuration object for the graph (or NULL)
	plot$baseConfig <- reactive({
		debug("base config")
		dim <- plot$state$dim
		if (is.null(dim)) {
			NULL
		} else {
			if (!is.null(plot$state$zoom)) {
				zoom <- plot$state$zoom
			} else {
				xRange <- plot$varRanges[[dim$x]]
				yRange <- plot$varRanges[[dim$y]]
				zoom <- matrix(c(xRange$min, yRange$min, xRange$max, yRange$max), 2)
			}
			vars <- lapply(1:plot$varCount, function(i) {
				if (i == dim$x || i == dim$y) {
					NULL
				} else if (plot$useProjections && unwrapOr(input[[plot$project[i]]], FALSE)) {
					NULL
				} else {
					unwrapOr(input[[plot$sliders[i]]], if(plot$varContinuous[i]) { plot$varRanges[[i]]$min } else { 1 })
				}
			})
			
			list(
				vars = vars,
				zoom = zoom,
				x = dim$x, y = dim$y,
				selection = plot$state$selection
			)		
		}
	})

	## State observers

	plot$.dimensionChange <- observeEvent(plot$state$dim, {
		plot$state$zoom <- NULL	
	})

	## UI observers

	# Clear selection on button click
	plot$.unselect <- observeEvent(input[[plot$buttonUnselect]], {
		plot$state$selection <- NULL	
	})

	# Clear zoom on button click
	plot$.unzoom <- observeEvent(input[[plot$buttonUnzoom]], {
		plot$state$zoom <- NULL	
	})

	printContinuousInterval <- function(dim, x, y) {
		paste0("[", round(x, digits = 3), ", ", round(y, digits = 3), "]")
	}

	# Takes a value from vars (continuous or threshold) and prints it accordingly
	printVarsValue <- function(dim, x) {
		if (plot$varContinuous[dim]) {
			paste0(round(x, digits = 3)) 		
		} else {
			t <- plot$varThresholds[[dim]]
			paste0("[", round(t[x], digits = 3), ", ", round(t[x+1], digits = 3), "]")
		}
	}

	# Takes a continuous value and prints it (transforming to thresholds if needed)
	printContinuousValue <- function(dim, x) {
		if (plot$varContinuous[dim]) {
			paste0(round(x, digits = 3)) 		
		} else {
			x <- plot$resolveStateIndex(dim, x)
			t <- plot$varThresholds[[dim]]
			paste0("[", round(t[x], digits = 3), ", ", round(t[x+1], digits = 3), "]")
		}
	}

	# Print interval takes continuous values and prints them as needed
	printInterval <- function(dim, x, y) {
		if (plot$varContinuous[dim]) {
			printContinuousInterval(dim, x, y)
		} else {
			x <- plot$resolveStateIndex(dim, x)
			y <- plot$resolveStateIndex(dim, y)
			t <- plot$varThresholds[[dim]]
			printContinuousInterval(dim, t[x], t[y+1])
		}
	}

	# Print currently displayed exact values
	output[[plot$outExact]] <- renderPrint({
		config <- plot$baseConfig()
		if (is.null(config)) {
			cat("...")
		} else {	
			# render existing values	
			vars <- config$vars
			for (i in 1:plot$varCount) {
				if (!is.null(vars[[i]])) {
					vars[[i]] <- printVarsValue(i, vars[[i]])
				}
			}
			# add data from hover listener
			hover <- input[[plot$eventHover]]
			if (is.null(hover$x) || is.null(hover$y)) {
				vars[[config$x]] <- printInterval(config$x, config$zoom[1,1], config$zoom[1,2])
				vars[[config$y]] <- printInterval(config$y, config$zoom[2,1], config$zoom[2,2])
			} else {
				vars[[config$x]] <- printContinuousValue(config$x, hover$x)
				vars[[config$y]] <- printContinuousValue(config$y, hover$y)
			}
			# fill in null values (projected) and put it all together
			lines <- sapply(1:plot$varCount, function (i) {
				value <- vars[[i]]
				if (is.null(value)) {
					value <- printInterval(i, plot$varRanges[[i]]$min, plot$varRanges[[i]]$max)
				}
				if (plot$varNames[i] != emptyVarName) {
					paste0(plot$varNames[i], ": ", value)
				} else ""
			})
			cat(paste0(lines, collapse = "\n"))
		}
	})

	plot$renderSlider <- function(var) {
		if (plot$varContinuous[var]) {
			range <- plot$varRanges[[var]]
			tooltip(tooltip = Explorer_VF_ScaleSlider_tooltip,
				sliderInput(plot$sliders[var],
					label = paste0(Explorer_VF_ScaleSlider_label, plot$varNames[var]),
					min = range$min, max = range$max, 
					value = unwrapOr(plot$baseConfig()$vars[[var]], range$min), 
					step = scale_granularity
				)
			)
		} else {
			thresholds <- plot$varThresholds[[var]]
			if (length(thresholds) > 2) {	# Don't render sliders for variables that can't be scaled
				tooltip(tooltip = Explorer_SS_ScaleSlider_tooltip,			
					sliderInput(plot$sliders[var],
						label = paste0(Explorer_SS_ScaleSlider_label, plot$varNames[var]),
						min = 1, max = length(thresholds) - 1, 
						value = unwrapOr(plot$baseConfig()$vars[[var]], 1), step = 1
					)
				)
			}			
		}
	}

	# Render state dimension sliders based on missing dimensions
	output[[plot$outSliders]] <- renderUI({				
		lapply(plot$missingDimensions(), function(var) {	
			if (useProjections) {
				advanced(
					tooltip(tooltip = Result_PS_ScaleSwitch_tooltip,
						checkboxInput(plot$project[var], 
							label = paste0(Result_PS_ScaleSlider_label, plot$varNames[var]),
							# Note: the default value is a little bit of a hack, because we assume only
							# parameters are countinuous in a mixed graph.
							value = unwrapOr(input[[plot$project[var]]], plot$varContinuous[var])	# TODO default value
						),

					),
					conditionalPanel(condition = paste0("input.", plot$project[var]), " == false",
						plot$renderSlider(var)
					)
				)				
			} else {
				plot$renderSlider(var)
			}
		})
	})

	## Plot event listeners

	# Zoom on brush
	plot$.zoom <- observeEvent(input[[plot$eventBrush]], {
		debug("zoom")
		event <- input[[plot$eventBrush]]
		if (!is.null(event$xmin) && !is.null(event$xmax) && !is.null(event$ymin) && !is.null(event$ymax)) {
			plot$state$zoom <- matrix(c(event$xmin, event$ymin, event$xmax, event$ymax), 2)
		}
	}) 

	# Select on double click
	plot$.select <- observeEvent(input[[plot$eventDoubleClick]], {
		debug("select")
		event <- input[[plot$eventDoubleClick]]
		if (!is.null(event$x) && !is.null(event$y)) {
			plot$state$selection <- list(x = event$x, y = event$y)
		}	
	})

	## UI renderers

	plot$renderUnselectButton <- function(label = "Unselect", tooltip = "Clear selection") {
		tooltip(tooltip = tooltip, actionButton(plot$buttonUnselect, label))
	}

	plot$renderUnzoomButton <- function(label = "Unzoom", tooltip = "Clear zoom") {
		tooltip(tooltip = tooltip, actionButton(plot$buttonUnzoom, label))
	}

	plot$renderImage <- function() {
		imageOutput(plot$outImage, "auto", "auto", 
       		click = plot$eventClick,
       		dblclick = plot$eventDoubleClick,
            brush = brushOpts(id = plot$eventBrush, resetOnNew = TRUE),
            hover = plot$eventHover
        )
	}

	plot$renderExact <- function(tooltip = "Current values") {
		tooltip(tooltip = tooltip, verbatimTextOutput(plot$outExact))           
	}

	plot$renderSliders <- function() {
		uiOutput(plot$outSliders)
	}

	plot$renderProjectionCheckbox <- function(var, labelPrefix = "Projection ", tooltip = "Set exact variable value.", step = 1) {
		tooltip(tooltip = tooltip,
			checkboxInput(plot$project[var],
				label = paste0(labelPrefix, plot$varNames[var]),
				value = unwrapOr(isolate(input[[plot$project[var]]]), T)
			)
		)
	}

	## Destructor - if you want to avoid leaks, you have to call this after plot is removed!
	plot$baseDestroy <- function() {
		debug(id, ":plot destroy")
		plot$.select$destroy()
		plot$.unselect$destroy()
		plot$.zoom$destroy()
		plot$.unzoom$destroy()
		plot$.dimensionChange$destroy()
		output[[plot$outExact]] <- renderPrint({ "Destroyed" })
		output[[plot$outImage]] <- renderPlot({ "Destroyed" })
	}

	plot
}