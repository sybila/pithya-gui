source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       

createBasePlot <- function(varNames, varRanges, id, input, session, output) {

	debug(id, ":plot create")

	plot <- list()

	plot$id <- id
	plot$varNames <- varNames
	plot$varCount <- length(varNames)
	plot$varRanges <- varRanges
	plot$state <- reactiveValues(
		dim = NULL, 			# list(x,y, xName, yName) of selected dimensions
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
					} else if (unwrapOr(input[[plot$project[i]]], FALSE)) {
						NULL
					} else {
						unwrapOr(input[[plot$sliders[i]]], plot$varRanges[[i]]$min)
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
		debug("dimension update")
		plot$state$zoom <- NULL	
	})

	## UI observers

	# Clear selection on button click
	plot$.unselect <- observeEvent(input[[plot$buttonUnselect]], {
		debug("unselect")
		plot$state$selection <- NULL	
	})

	# Clear zoom on button click
	plot$.unzoom <- observeEvent(input[[plot$buttonUnzoom]], {
		debug("unzoom")
		plot$state$zoom <- NULL	
	})

	# Use this function to override default printing behavior
	plot$buildPrintExact <- function(printHoverValue, printVarsValue, printZoomInterval, printRangeInterval) {
		renderPrint({
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
					vars[[config$x]] <- printZoomInterval(config$x, config$zoom[1,1], config$zoom[1,2])
					vars[[config$y]] <- printZoomInterval(config$y, config$zoom[2,1], config$zoom[2,2])
				} else {
					vars[[config$x]] <- printHoverValue(config$x, hover$x)
					vars[[config$y]] <- printHoverValue(config$y, hover$y)
				}
				# fill in null values (projected) and put it all together
				lines <- sapply(1:plot$varCount, function (i) {
					value <- vars[[i]]
					if (is.null(value)) {
						value <- printRangeInterval(i, plot$varRanges[[i]]$min, plot$varRanges[[i]]$max)
					}
					paste0(plot$varNames[i], ": ", value)
				})
				cat(paste0(lines, collapse = "\n"))
			}
		})
	}

	roundToThree <- function(dim, x) { paste0(round(x, digits = 3)) }
	roundIntervalToThree <- function(dim, x, y) {
		paste0("[", round(x, digits = 3), ", ", round(y, digits = 3), "]")
	}

	# Print currently displayed exact values
	output[[plot$outExact]] <- plot$buildPrintExact(
		printVarsValue = roundToThree, printHoverValue = roundToThree,
		printZoomInterval = roundIntervalToThree, printRangeInterval = roundIntervalToThree
	)

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