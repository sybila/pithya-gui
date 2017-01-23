source("config.R")          # global configuration


parseResultFile <- function(file) {
	parseResultData(readLines(file))
}

parseResultData <- function(fileData) {
	file <- fromJSON(fileData)

	thresholds <- file$thresholds
	paramValues <- file$parameter_values
	states <- file$states

	dimensionCount <- length(thresholds)
	
	dimensionSizes <- sapply(thresholds, function(x) length(x))
	stateSpaceSizes <- sapply(dimensionSizes, function(x) x - 1)

	stateSpace <- array(0, stateSpaceSizes)

	debug("param count: ", length(paramValues))

	result <- list(
		formulas = sapply(file$results, function(r) r$formula),
		type = file$type,
		varNames = file$variables,
		varThresholds = file$thresholds,
		varRanges = lapply(file$thresholds, function(t) list(min = t[1], max = t[length(t)])),
		paramNames = file$parameters,
		paramRanges = lapply(file$parameter_bounds, function(p) list(min = p[1], max = p[2])),
		paramValues = lapply(paramValues, function(param) {
			#if (type == "rectangular") {
			#	rectangleCount <- length(param)
			#	parameterCount <- length(file$parameters)				
			#} else if (type == "smt") {
			#	expression <- gsub("&&", "&", fixed = TRUE, gsub("||", "|", fixed = TRUE, param$Rexpression))
			#	eval(parse(text = paste0("function(ip) { ", expression, "} ")))
			#} else "Unknown parameter type"
			NULL
		}),
		resultMapping = lapply(file$results, function(result) {
			map <- stateSpace
			# WARNING!!! This whole thing relies on the predictable state IDs which correspond to 
			# dimensions!
			# Also +1 is because results are indexed from 0

			# Construct an array with parameter value indices (or 0 if missing)
			for (pair in result$data) {	
				stateId <- states[[pair[1]+1]]$id + 1
				map[stateId] <- pair[2] + 1
			}
			map
		})
	)

	debug("[result] parsing finished")
	#debug(result)

	result
}