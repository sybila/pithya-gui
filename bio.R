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

	model <- paste0(lines, collapse = "\n")
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

	model <- list(
		varNames = varNames,
		varThresholds = varThresholds,
		varRanges = varRanges,
		varEQ = varEQ,
		paramNames = paramNames,
		paramRanges = paramRanges	
	)
	debug("[.bio parser] Finished parsing. Model representation will follow.")
	debug(model)

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

Approx <- function(m,l) {
    as.vector(apply(as.matrix(m),c(1,2),function(s) {
        if(s <= l[[1]][1]) return(l[[1]][2])
        if(s >= l[[length(l)]][1]) return(l[[length(l)]][2])
        for(i in 2:length(l)) {
            a<-l[[i-1]]
            b<-l[[i]]
            if(s >= a[1] && s <= b[1]) return(a[2]+(s-a[1])/(b[1]-a[1])*(b[2]-a[2]))
        }
    }))
}
