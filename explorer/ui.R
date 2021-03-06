source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       # UI utilities

explorerTab <- function() {
	tabPanel(Explorer_label, icon=icons$move,
		tooltip(tooltip = Explorer_tooltip,
			fluidPage(theme = "simplex.css",
                uiOutput("explorer_notification"),
				explorerControlPanel(),
				tags$hr(),
        		uiOutput("plots"),
                tooltip(tooltip = Explorer_addPlot_tooltip,
                    bsButton("add_plot_row", Explorer_addPlot_label, icon=icon("picture",lib="glyphicon"), disabled = TRUE)
                )
			)
		)
	)
}

explorerControlPanel <- function() {
	fluidRow(
		column(3, class = "arrow_column_1",
            tooltip(tooltip = Explorer_countOfDirectionArrows_tooltip,
                sliderInput("arrows_number",Explorer_countOfDirectionArrows_label,10,100,step=1,value=25)
            ),
            tooltip(tooltip = Explorer_coloringThreshold_tooltip,
                sliderInput("colThr",Explorer_coloringThreshold_label,0,0.1,step=0.001,value=0.05)
            ),
            tooltip(tooltip = Explorer_coloringDirection_tooltip,
                radioButtons("colVariant",Explorer_coloringDirection_label,list(both="both",none="none",horizontal="horizontal",vertical="vertical"),"both",inline=T)
            )
        ),
        column(3, class = "arrow_column_2",
            tooltip(tooltip = Explorer_lengthOfDirectionArrows_tooltip,
                numericInput("arrowSize",Explorer_lengthOfDirectionArrows_label,min=0.001,max=100,step=0.05,value=1)
            ),
            tooltip(tooltip = Explorer_widthOfAllArrows_tooltip,
                sliderInput("transWidth",Explorer_widthOfAllArrows_label,1,5,step=0.5,value=1.5)
            )
        ),
        advanced(
            column(3, class = "fluid_column",
                tooltip(tooltip = Explorer_flowPointsCount_tooltip,
                    numericInput("flow_points_count", Explorer_flowPointsCount_label,min=50,max=NA,step=50,value=1000)
                ),
                tooltip(tooltip = Explorer_flowPointsDensity_tooltip,
                    sliderInput("flow_points_density", Explorer_flowPointsDensity_label,0.01,10,step=0.01,value=0.1)
                )
            )
        ),
		column(3, class = "param_column",
			uiOutput("param_sliders_bio")
		)
	)
}

plotRow <- function(r, input) {
	oneVar <- length(r$model$varNames) == 1
	tagList(
		# Plot header
        tags$div(class = "my-row",
		fluidRow(class = "row-header",
            column(1, class = "lab", Explorer_horizontal_label),
            column(2, 
                selectInput(r$xDimSelect, "", choices = r$model$varNames, 
                    selected = unwrapOr(input[[r$xDimSelect]], r$model$varNames[1])
                )
            ),
            column(1, class = "lab", Explorer_vertical_label),
            column(2, selectInput(r$yDimSelect, "", choices = r$model$varNames, selected = if (oneVar) { NULL } else { 
                unwrapOr(input[[r$yDimSelect]], r$model$varNames[2])
            })),
            column(1, class = "rem", actionButton(r$remove, "Remove")),
            column(1, class = "hid", checkboxInput(r$hide, Explorer_hide_label, unwrapOr(input[[r$hide]], FALSE)))
		),        
        conditionalPanel(condition = paste0("input.", r$hide, " == false"),
        hr(),
		fluidRow(class = "row-content",
			# Vector field controls
            column(2, class = "left-controls controls",
                tags$div(style = "float: right",
                    tooltip(tooltip = Explorer_VF_ApplyToAll_tooltip,
                        actionButton(r$applyVectorToAll, Explorer_VF_ApplyToAll_label)
                    ),
                    tooltip(tooltip = Explorer_VF_ApplyToTSS_tooltip,
                        actionButton(r$applyVectorToState, Explorer_VF_ApplyToTSS_label)
                    ),
                    r$vector$renderUnselectButton(),
                    r$vector$renderUnzoomButton()   
                ),            
                tags$div(style = "clear: both"),
                advanced(tooltip(tooltip = Explorer_VF_UsePWAmodel_tooltip,
                    checkboxInput(r$vector$usePWMA, Explorer_VF_UsePWAmodel_label, unwrapOr(input[[r$vector$usePWMA]], FALSE))
                )),
                r$vector$renderSliders(),
                r$vector$renderExact(tooltip = Explorer_VF_HoverTextArea_tooltip)      
            ),
            # Vector field
            column(4,
                    r$vector$renderImage()
            ),
            # State field
            column(4,
                    r$state$renderImage()                  
            ),
            # State space controls
            column(2, class = "right-controls controls",
                tooltip(tooltip = Explorer_SS_ApplyToAll_tooltip,
                    actionButton(r$applyStateToAll, Explorer_SS_ApplyToAll_label)
                ),
                r$state$renderUnselectButton(),
                r$state$renderUnzoomButton(),
                r$state$renderSliders(),
                r$state$renderExact(tooltip = Explorer_SS_HoverTextArea_tooltip)      
            )    
        )))		
	)		
}
