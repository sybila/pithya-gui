source("config.R")          # global configuration

#bio model format
#model = list(
#	varNames = vector[string], 
#	varThresholds = list[vector[num]], 
#	varRanges = list[list(min, max)], 
#	varEQ = list[function(list(vars), list(params)) -> num/matrix]
#	paramNames = vector[string], 
#	paramRanges = list[list(min, max)]
#)


parseBioFile <- function(file) {
	parseBioLines(readLines(file))
}

parseBioLines <- function(lines) {
	# TODO multiline comments	

	# \n ensures there is a new line at the end of file
	model <- paste0(c(lines, "\n"), collapse = "\n")	
	# remove comments
	model <- str_replace_all(model, "(#|//).*\n", "\n")
	# remove comments at the end of file 
	# (this is safe because all other comments were removed before, so only place where # or // can be is at the last line)
	model <- str_replace_all(model, "(#|//).*", "\n")	
	# remove whitespace
	model <- str_replace_all(model, "[ \t\r]", "")

	# Extract payload from VARS: lines, split it by ",", remove duplicates and sort; [[1]] = first input (we only have one); [,2] = second row - matches

	varNames <- sort(unlist(unique(c(str_split(str_match_all(model, "VARS:(.+)\n")[[1]][,2], ",")))))	

	# Extract params as a list of "name,min,max"
	params <- sort(unlist(unique(c(str_split(str_match_all(model, "PARAMS:(.+)\n")[[1]][,2], ";")))))

	# Extract name vector
	paramNames <- unlist(lapply(params, function(p) str_split(p, ",")[[1]][1]))

	# Extract range list
	paramRanges <- lapply(params, function(p) { s <- str_split(p, ",")[[1]]; list(min = as.numeric(s[2]), max = as.numeric(s[3])) })

	# Extract threshold lines, sort them, remove variable name, split by ","
	varThresholds <- lapply(sort(str_match_all(model, "THRES:(.+)\n")[[1]][,2]), function(t) {
		as.numeric(str_split(str_split(t, ":")[[1]][2], ",")[[1]])
	})

	# Take first and last threshold
	varRanges <- lapply(varThresholds, function(t) list(min = t[1], max = t[length(t)]))

	# Same as with params, but simpler
	constants <- sort(unlist(c(str_split(str_match_all(model, "CONSTS:(.+)\n")[[1]][,2], ";"))))
	constantNames <- unlist(lapply(constants, function(c) str_split(c, ",")[[1]][1]))
	constantValues <- as.numeric(unlist(lapply(constants, function(c) str_split(c, ",")[[1]][2])))

	# Similar to thresholds, but read just string.
	eqStrings <- lapply(sort(str_match_all(model, "EQ:(.+)\n")[[1]][,2]), function(eq) str_split(eq, "=")[[1]][2])

	# change of format 'Approx(x)([1,2],..,[3,4])' into 'Approx(x,list(c(1,2),..,c(3,4))'
	eqStrings <- lapply(eqStrings, function(eq) {
		gsub("\\]\\)","\\)\\)\\)",gsub("\\],\\[","\\),c\\(",gsub("\\)\\(\\[",",list\\(c\\(",eq)))
	})    
	
	# Split equations to atomic parts
	# The weird ?= and ?! is positive lookahead/lookbehind and it is awesome.
	# see: http://stackoverflow.com/questions/4416425/how-to-split-string-with-some-separator-but-without-removing-that-separator-in-j
	eqStringsTokens <- str_split(eqStrings, "(?=[-*+,()\\[\\]])|(?<=[-*+,()\\[\\]])")
	eqExpressions <- lapply(eqStringsTokens, function(stream) {
		paste0(collapse = "", sapply(stream, function(token) {
			varIndex <- match(token, varNames)			
			paramIndex <- match(token, paramNames)
			constIndex <- match(token, constantNames)
			if (!is.na(varIndex)) {
				paste0("vars[[", varIndex, "]]")
			} else if (!is.na(paramIndex)) {
				paste0("params[[", paramIndex, "]]")
			} else if (!is.na(constIndex)) {
				paste0(constantValues[constIndex])
			} else {
				token
			}
		}))
	})
	varEQ <- lapply(eqExpressions, function(eq) eval(parse(text = paste0("function(vars, params) { ", eq, "} "))))

	# Ensure that no matter what, we have at least two variables!	
	if (length(varNames) == 1) {
		varNames <- c(varNames, emptyVarName)
		varThresholds[[2]] <- c(0, 0)
		varRanges[[2]] <- list(min = 0, max = 0)
		varEQ[[2]] <- function(vars, params) { 0 }
	}

	model <- list(
		varNames = varNames,
		varThresholds = varThresholds,
		varRanges = varRanges,
		varEQ = varEQ,
		paramNames = paramNames,
		paramRanges = paramRanges	
	)
	debug("[.bio parser] Parse successful.")
	#debug(model)

	model
}

# Functions used in the .bio equations
# TODO add sigmoid

hillm <- function(s,t,n=1,a=1,b=0) (t^n)/(s^n + t^n)
Hillm <- function(s,t,n=1,a=1,b=0) (t^n)/(s^n + t^n)
hillp <- function(s,t,n=1,a=0,b=1) (s^n)/(s^n + t^n)
Hillp <- function(s,t,n=1,a=0,b=1) (s^n)/(s^n + t^n)

ramp <- function(value,min,max,a,b) {
    if(a < b) {
        #positive
        if(value < min) return(0)
        if(value > max) return(0)
        return(a+(value-min)/(max-min)*(b-a))
    } else {
        #negative
        if(value < min) return(0)
        if(value > max) return(0)
        return((a-(value-min)/(max-min))*(a-b))
    }
}

# TODO: can we speed this up?!
Approx <- function(m,l) {	
    apply(as.matrix(m),c(1,2),function(s) {
        if(s <= l[[1]][1]) return(l[[1]][2])
        if(s >= l[[length(l)]][1]) return(l[[length(l)]][2])
        for(i in 2:length(l)) {
            a<-l[[i-1]]
            b<-l[[i]]
            if(s >= a[1] && s <= b[1]) return(a[2]+(s-a[1])/(b[1]-a[1])*(b[2]-a[2]))
        }
    })
}

# Compute whole transition system for given model and parametrisation.
#
# You can indicate which dimensions are bounded (create selfloops) using logical 
# vectors boundedUp/Down. (useful for computing a cut of state space using a cut model)
#
# Note: computing whole transition system is necessary to properly evaluate selfloops.
#
# Results: list(loop=array, trans=list(list(up, down)))
# (loop array and up/down arrays should directly correspond to the natural state indexing)
computeTransitions <- function(model, params, boundedUp, boundedDown) {
	
	dimensionCount <- length(model$varNames)
	
	dimensionSizes <- sapply(model$varThresholds, function(x) length(x))
	stateSpaceSizes <- sapply(dimensionSizes, function(x) x - 1)
	
	debug(dimensionSizes)

	# Prepare values for equation evaluation
	one <- array(1, dimensionSizes)
	params <- lapply(params, function(p) one * p)
	vars <- lapply(1:dimensionCount, function(d) {
		thresholds <- model$varThresholds[[d]]
		# create permutation vectors
		dimensions <- 1:dimensionCount
		dimensions[c(1,d)] <- dimensions[c(d,1)]
		sizes <- sapply(dimensions, function(i) dimensionSizes[i])
		aperm(array(model$varThresholds[[d]], sizes), dimensions)
	})

	for (v in vars) {
		debug(dim(v))
	}

	# Compute derivation values in all vertices
	progressPerVar <- 0.8 / dimensionCount
	dv <- lapply(model$varEQ, function(eq) {				
		r <- one * eq(vars, params)
		incProgress(progressPerVar)
		r
	})	

	## Utility functions

	# Remove one specific row(column?) from given dimension without affecting rest of the array
	dropRow <- function(arr, dim, row) {		
		# arr[,,-row,,]
		mask <- lapply(1:dimensionCount, function(i) if (i == dim) -row else TRUE)
		do.call("[", append(list(arr, drop = FALSE), mask))		
	}
	# Update one specific row(column?) from given dimension without affecting rest of the array
	assignRow <- function(arr, dim, row, value) {
		# arr[,,row,,] <- value
		mask <- append(lapply(1:dimensionCount, function(i) if (i == dim) row else TRUE), value)		
		do.call("[<-", append(list(arr), mask))				
	}
	# Merge subsequent values in all dimensions except for the given one.
	# Result is facet validity for investigated dimension.
	contractDims <- function(l, dim) {
		Reduce(init = l, x = (1:dimensionCount)[-dim], f= function(l, d) {
			dropRow(l, d, 1) | dropRow(l, d, dimensionSizes[d])
		})				
	}

	# Map values to their logical counterparts
	dir <- lapply(dv, function(d) list(up = d > 0, down = d < 0))
	
	# For each dimension show if there is an arrow going up or down in that facet (includes border facets)
	# We need both up and down, because there may be zero values which go neither way.
	trans <- lapply(1:dimensionCount, function(dim) {
		list(
			up = contractDims(dir[[dim]]$up, dim),
			down = contractDims(dir[[dim]]$down, dim)
		)
	})
	
	# Compute if flow is present in specific dimension
	dimFlows <- lapply(1:dimensionCount, function(d) {		
		# Note that d has not been dropped from yet, so dropping one row will bring it to 
		# the same relative size as other dimensions
		t <- trans[[d]]
		negativeIn <- dropRow(t$up, d, 1)
		negativeOut <- dropRow(t$down, d, dimensionSizes[d])
		positiveIn <- dropRow(t$down, d, 1)
		positiveOut <- dropRow(t$up, d, dimensionSizes[d])				

		positiveFlow <- negativeIn & positiveOut & !(negativeOut | positiveIn)
		negativeFlow <- negativeOut & positiveIn & !(negativeIn | positiveOut)

		if (boundedUp[d]) {
			# Positive flow can't occur in the highest states
			positiveFlow <- assignRow(positiveFlow, d, stateSpaceSizes[d], FALSE)						
		}

		if (boundedDown[d]) {
			# Negative flow can't occur in the lowest states
			negativeFlow <- assignRow(negativeFlow, d, 1, FALSE)			
		}
		
		positiveFlow|negativeFlow
	})

	setProgress(0.9)

	# Compute global loop property based on flow in every dimension
	flow <- Reduce(function(x,y) x|y, dimFlows)
	loop <- !flow
	setProgress(1.0)
	list(
		loop = loop,
		trans = lapply(1:dimensionCount, function(d) {
			# Remove first and erase last transition. (vice versa for negative)
			# Both go out of the model and should not be considered as transitions.
			t <- trans[[d]]
			t$up <- dropRow(t$up, d, 1)						# can't come from -1
			t$down <- dropRow(t$down, d, dimensionSizes[d])	# can't come from size+1
			t$up <- assignRow(t$up, d, stateSpaceSizes[d], FALSE)	# can't go up from last state
			t$down <- assignRow(t$down, d, 1, FALSE)				# con't go down from first state
			t
		})
	)
}