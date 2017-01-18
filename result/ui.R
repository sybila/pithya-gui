source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       # UI utilities

resultTab <- function() {
	tabPanel(Result_label, icon=icons$barcode,
    	tooltip(tooltip = Result_tooltip,            
    		fluidPage(theme = "simplex.css",
        		resultControlPanel(),
        		tags$hr(),
        		uiOutput("param_space_plots")
    		)
		)
	)
}

resultControlPanel <- function() {
	fluidRow(
		column(2,
			tooltip(tooltip = Result_Browse_tooltip,
                fileInput("ps_file", Result_Browse_label,accept=".json")
            ),
            fluidRow(
                column(6,
                    tooltip(tooltip = Result_saveResults_tooltip,
                        downloadButton("save_result_file",Result_saveResults_label)
                    )
                )
            )
		),
		column(2,
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
            ),
            uiOutput("ps_zoom_sliders")
		),
		column(4,
			uiOutput("chosen_ps_states_ui")
		),
		column(4,
			uiOutput("param_selector"),
           	tooltip(tooltip = Result_addPlot_tooltip,
                bsButton("add_param_plot", Result_addPlot_label, icon=icons$picture, disabled=T)
            )
		)
	)
}
