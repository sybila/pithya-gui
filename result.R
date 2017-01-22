source("config.R")          # global configuration


parseResultFile <- function(file) {
	list(
		formulas = c("prop1", "prop2", "formula2"),
		varNames = c("var1", "var2", "var3"),
		varRanges = list(
			list(min = 1.0, max = 3.0),
			list(min = 0.0, max = 4.0),
			list(min = 1.0, max = 6.0)
		),
		varThresholds = list(
			c(1,1.5,1.8,2.2,2.6,2.9,3.0),
			c(0.0,1.5,2.0,2.5,3.5,4.0),
			c(1,2,3,4,5,6)
		),
		paramNames = c("p1", "p2", "p3"),
		paramRanges = list(
			list(min = 0.0, max = 1.0),
			list(min = 1.0, max = 2.0),
			list(min = 0.5, max = 3.5)
		)
	)
}