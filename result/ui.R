source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       # UI utilities

resultTab <- function() {
	tabPanel(Result_label, icon=icons$barcode,
    	tooltip(tooltip = Result_tooltip,            
    		fluidPage(theme = "simplex.css",
    		    uiOutput("result_notification"),
        		resultControlPanel(),
        		tags$hr(),
        		uiOutput("param_plots"),
            tooltip(tooltip = Result_addPlot_tooltip,
                bsButton("add_param_plot", Result_addPlot_label, icon=icons$picture, disabled=T)
            )
    		)
		)
	)
}

resultControlPanel <- function() {
	fluidRow(
		column(4, class = "result_file_column",
		        tooltip(tooltip = Result_Browse_tooltip,
                fileInput("ps_file", Result_Browse_label,accept=".json")
            ),
		        fluidRow(
  		          column(4,
  		                 tooltip(tooltip = Result_loadSynthResults_tooltip,
  		                         bsButton("load_synth_results", Result_loadSynthResults_label, disabled = T, block =T)
  		                 )
  		          ),
  		          column(4,
  		                 tooltip(tooltip = Result_loadTCAResults_tooltip,
  		                         bsButton("load_tca_results", Result_loadTCAResults_label, disabled = T, block =T)
  		                 )
  		          ),
  		          column(4,
  		                 tooltip(tooltip = Result_loadImportedResults_tooltip,
  		                         bsButton("load_imported_results", Result_loadImportedResults_label, disabled = T, block =T)
  		                 )
  		          )
		        ),
		        tags$br(),
            fluidRow(
                column(6,
                    tooltip(tooltip = Result_saveResults_tooltip,
                        downloadButton("save_result_file", Result_saveResults_label)
                    )
                )
            )
		),
		column(2, class = "coverage_column",
			tooltip(tooltip = Result_showParametersCoverage_tooltip,
                checkboxInput("coverage_check", Result_showParametersCoverage_label, F)
            ),
            conditionalPanel(condition = "input.coverage_check == true",
                tooltip(tooltip = Result_greyShadeDegree_tooltip,
                    sliderInput("color_alpha_coeficient", Result_greyShadeDegree_label,min=0,max=1,value=0.9,step=0.01,ticks=F)
                ),
                tooltip(tooltip = Result_parameterDensity_tooltip,
                    sliderInput("density_coeficient",Result_parameterDensity_label,min=10,max=150,value=50,step=1,ticks=F)
                )
            )
		),
    column(6, class = "param_column", uiOutput("error_message"))
	)
}

resultRow <- function(r, input) {
    varList <- c(r$result$paramNames, r$result$varNames)
    tagList(        
        tags$div(class = "my-row",
        # Header (formula selector, hide, remove)
        fluidRow(class = "row-header",
            column(1, class = "lab", Result_chooseFormulaOfInterest_label),
            column(3, tooltip(tooltip = Result_chooseFormulaOfInterest_tooltip,
                selectInput(r$formula, "", choices = r$result$formulas, 
                    selected = unwrapOr(input[[r$formula]], r$result$formulas[1])
                )
            )),        
            column(1, class = "rem", tooltip(tooltip = Result_cancel_tooltip,
                actionButton(r$remove, "Remove")
            )),
            column(1, class = "hid", tooltip(tooltip = Result_hide_tooltip,
                checkboxInput(r$hide, Result_hide_label, unwrapOr(input[[r$hide]], FALSE))
            )),
            column(1, class = "grid", advanced(tooltip(tooltip = Result_grid_tooltip,
                checkboxInput(r$grid, Result_grid_label, unwrapOr(input[[r$grid]], TRUE))
            )))
        ),
        conditionalPanel(condition = paste0("input.", r$hide, " == false"),
        hr(),
        # Plots
        fluidRow(class = "row-content",
            # Params field controls
            column(2, class = "left-controls controls",
                tags$div(style = "float: right",
                    r$params$renderUnselectButton(),
                    r$params$renderUnzoomButton()
                ),                
                fluidRow(style = "clear: both",
                    column(6, style = "padding-right: 3px;", tooltip(tooltip = Result_horizontal_tooltip,
                        selectInput(r$xDimParams, Result_horizontal_label, choices = varList,
                            selected = unwrapOr(input[[r$xDimParams]], varList[1])
                        )
                    )),
                    column(6, style = "padding-left: 3px;", tooltip(tooltip = Result_vertical_tooltip,
                        selectInput(r$yDimParams, Result_vertical_label, choices = varList,
                            selected = unwrapOr(input[[r$yDimParams]], varList[2])
                        )
                    ))
                ),
                "* at least one parameter must be selected",
                advanced(r$params$renderSliders()),
                r$params$renderExact(tooltip = Result_PS_HoverTextArea_tooltip)
            ),
            # Vector field
            column(4,
                    r$params$renderImage()                  
            ),
            # State field
            column(4,
                    r$states$renderImage()                  
            ),
            # State space controls
            column(2, class = "right-controls controls",
                r$states$renderUnselectButton(),
                r$states$renderUnzoomButton(),
                fluidRow(
                    column(6, style = "padding-right: 3px;", tooltip(tooltip = Result_SS_horizontal_tooltip,
                        selectInput(r$xDimStates, Result_SS_horizontal_label, choices = r$result$varNames,
                            selected = unwrapOr(input[[r$xDimStates]], r$result$varNames[1])
                        )
                    )),
                    column(6, style = "padding-left: 3px;", tooltip(tooltip = Result_SS_vertical_tooltip,
                        selectInput(r$yDimStates, Result_SS_vertical_label, choices = r$result$varNames,
                            selected = unwrapOr(input[[r$yDimStates]], r$result$varNames[2])
                        )
                    ))
                ),
                r$states$renderSliders(),
                r$states$renderExact(tooltip = Result_SS_HoverTextArea_tooltip)
            )    
        )
        )
        )
    )
}