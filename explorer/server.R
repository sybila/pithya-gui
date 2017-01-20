source("config.R")          # global configuration
source("tooltips.R")        # texts

explorerServer <- function(input, session, output) {

	# Show parameter numberic inputs when model is loaded
	output$param_sliders_bio <- renderUI({
		model <- session$pithya$approximatedModel$model
		if (!is.null(model)) {	
			lapply(1:length(model$paramNames), function(pIndex) {
				debug("[param_sliders_bio] render bio sliders")
				min <- model$paramRanges[[pIndex]]$min
				max <- model$paramRanges[[pIndex]]$max
				tooltip(tooltip = Explorer_parameter_tooltip,
					numericInput(
						inputId = paste0("param_slider_", pIndex),
						label = paste0(Explorer_parameter_label, model$paramNames[pIndex]),
						min = min, max = max,
						value = (0.1 * (max - min)),
						step = (0.001 * (max - min))
					)
				)
			})
		} else {
			debug("[param_sliders_bio] remove bio sliders")			
			"Model missing. Compute approximation first"
		}
	})

}