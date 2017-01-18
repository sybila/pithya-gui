source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       # UI utilities

explorerTab <- function() {
	tabPanel(Explorer_label, icon=icons$move,
		tooltip(tooltip = Explorer_tooltip,
			fluidPage(theme = "simplex.css",
				explorerControlPanel(),
				tags$hr(),
        		uiOutput("plots")
			)
		)
	)
}

explorerControlPanel <- function() {
	fluidRow(
		column(4,
			fluidRow(
				column(6,
					tooltip(tooltip = Explorer_countOfDirectionArrows_tooltip,
						sliderInput("arrows_number",Explorer_countOfDirectionArrows_label,10,100,step=1,value=25)
					),
					tooltip(tooltip = Explorer_coloringThreshold_tooltip,
						sliderInput("colThr",Explorer_coloringThreshold_label,0,0.1,step=0.001,value=0.05)
					)
				),
				column(6,
					tooltip(tooltip = Explorer_lengthOfDirectionArrows_tooltip,
                		sliderInput("arrowSize",Explorer_lengthOfDirectionArrows_label,0.01,10,step=0.05,value=0.3)
            		),
                    tooltip(tooltip = Explorer_widthOfAllArrows_tooltip,
                    	sliderInput("transWidth",Explorer_widthOfAllArrows_label,1,5,step=0.5,value=1.5)
                	)
				)
			),
			tooltip(tooltip = Explorer_coloringDirection_tooltip,
                radioButtons("colVariant",Explorer_coloringDirection_label,list(both="both",none="none",horizontal="horizontal",vertical="vertical"),"both",inline=T)
            )
		),
		column(2,
			tooltip(tooltip = Explorer_flowPointsCount_tooltip,
                numericInput("flow_points_count", Explorer_flowPointsCount_label,min=50,max=NA,step=50,value=500)
            ),
            tooltip(tooltip = Explorer_flowPointsDensity_tooltip,
                sliderInput("flow_points_density", Explorer_flowPointsDensity_label,0.01,10,step=0.01,value=1)
            )
		),
		column(2,
			uiOutput("param_sliders_bio")
		),
		column(4,
			uiOutput("selector"),
            tooltip(tooltip = Explorer_addPlot_tooltip,
                bsButton("add_vf_plot",Explorer_addPlot_label,icon=icon("picture",lib="glyphicon"), disabled=T)
            )
		)
	)
}