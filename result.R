source("config.R")          # global configuration


parseResultFile <- function(file) {
	debug("parse result file ", file)
	parseResultData(readLines(file))
}

parseResultData <- function(fileData) {
	file <- fromJSON(fileData)

	# TODO - handle models without parameters?

	varNames <- file$variables
	varThresholds <- file$thresholds

	if (length(varNames) == 1) {
		varNames <- c(varNames, emptyVarName)
		varThresholds[[2]] <- c(0,1)
	}

	paramValues <- file$parameter_values

	dimensionCount <- length(varThresholds)
	
	dimensionSizes <- sapply(varThresholds, function(x) length(x))
	stateSpaceSizes <- sapply(dimensionSizes, function(x) x - 1)

	stateSpace <- array(0, stateSpaceSizes)

	type <- file$type

	result <- list(
		formulas = sapply(file$results, function(r) r$formula),
		type = file$type,
		varNames = varNames,
		varCount = length(varNames),
		varThresholds = varThresholds,
		varRanges = lapply(varThresholds, function(t) list(min = t[1], max = t[length(t)])),
		paramNames = file$parameters,
		paramCount = length(file$parameters),
		paramRanges = lapply(file$parameter_bounds, function(p) list(min = p[1], max = p[2])),
		paramValues = if (type == "rectangular") {
			lapply(paramValues, function(p) lapply(p, function(r) unlist(r)))
		} else if (type == "smt") {
			lapply(paramValues, function(p) {
				expression <- p$Rexpression
				for (p in 1:length(file$parameters)) {
					expression <- gsub(
						x = expression, 
						pattern = paste0("ip$",file$parameters[p]), 
						replacement = paste0("params[[",p,"]]"),
						fixed = TRUE
					)
					expression <- gsub(x = expression, pattern = "||", replacement = "|", fixed = TRUE)
					expression <- gsub(x = expression, pattern = "&&", replacement = "&", fixed = TRUE)					
				}				
				eval(parse(text = paste0("function(params) { ", expression, "} ")))
			})	
		} else {
			# TODO some error
		},
		resultMapping = lapply(file$results, function(result) {
			map <- stateSpace
			# WARNING!!! This whole thing relies on the predictable state IDs which correspond to 
			# dimensions!
			# Also +1 is because results are indexed from 0

			# Construct an array with parameter value indices (or 0 if missing)
			for (pair in result$data) {	
				stateId <- file$states[[pair[1]+1]]$id + 1
				map[stateId] <- pair[2] + 1
			}
			map
		}),
		resultInverseMapping = lapply(file$results, function(result) {
			map <- array(0, c(length(paramValues)))
			
			# Compute sum of inverse mapping - for each formula, how much state space
			# is each param. value covering
			for (pair in result$data) {
				map[pair[2]+1] <- map[pair[2]+1] + 1
			}

			map
		})
	)

	debug("[result] parsing finished")	

	result
}

rectangleContains <- function(rect, dim, value) {
	rect[2*(dim-1)+1] <= value && value <= rect[2*(dim-1)+2]
}

rectangleContainsPoints <- function(rect, points) {
	Reduce(function(x,y) x & y, lapply(1:length(points), function(dim) {		
		rect[2*(dim-1)+1] <= points[[dim]] & points[[dim]] <= rect[2*(dim-1)+2]
	}))
}