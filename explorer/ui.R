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
		column(6,
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
		column(3,
			tooltip(tooltip = Explorer_flowPointsCount_tooltip,
                numericInput("flow_points_count", Explorer_flowPointsCount_label,min=50,max=NA,step=50,value=500)
            ),
            tooltip(tooltip = Explorer_flowPointsDensity_tooltip,
                sliderInput("flow_points_density", Explorer_flowPointsDensity_label,0.01,10,step=0.01,value=1)
            )
		),
		column(3,
			uiOutput("param_sliders_bio")
		)
	)
}

explorerPlot <- function(p) {
	tagList(
		# Plot header
		fluidRow(
			column(1, paste0("Plot ", p$id)),				
			column(2, tooltip(tooltip = Explorer_horizontal_tooltip,
                selectInput(p$xDimSelect, Explorer_horizontal_label, p$dimensions)
            )),
            column(2, tooltip(tooltip = Explorer_vertical_tooltip,
                selectInput(p$yDimSelect, Explorer_vertical_label, p$dimensions)
            )),
            column(2, tooltip(tooltip = Explorer_cancel_tooltip,
            	actionButton(p$removePlot, "Remove")
        	)),
        	column(2, tooltip(tooltip = Explorer_hide_tooltip,
                checkboxInput(p$hidePlot, Explorer_hide_label)
            ))
		),
		fluidRow(
			# Vector field controls
            column(2,
            	tooltip(tooltip = Explorer_VF_ApplyToAll_tooltip,
            		actionButton(p$applyVectorToAll, Explorer_VF_ApplyToAll_label)
        		),
        		tooltip(tooltip = Explorer_VF_ApplyToTSS_tooltip,
        			actionButton(p$applyVectorToState, Explorer_VF_ApplyToTSS_label)
    			),
    			tooltip(tooltip = Explorer_VF_ClearPlot_tooltip,
        			actionButton(p$clearVector, Explorer_VF_ClearPlot_label)
    			),
    			tooltip(tooltip = Explorer_VF_Unzoom_tooltip,
    				actionButton(p$unzoomVector, Explorer_VF_Unzoom_label)
				),
    			advanced(tooltip(tooltip = Explorer_VF_UsePWAmodel_tooltip,
					checkboxInput(p$usePWMA, Explorer_VF_UsePWAmodel_label)
				)),
                uiOutput(p$scaleVector),
                tooltip(tooltip = Explorer_VF_HoverTextArea_tooltip,
                	verbatimTextOutput(p$exactVector)
            	)                       
            ),
            # Vector field
            column(4,
                	helpText(Explorer_VF_label),
                	imageOutput(p$plotVector, "auto", "auto", 
                   		click = p$clickVector,
                   		dblclick = p$doubleClickVector,
                        brush = p$brushVector,
                        hover = p$hoverVector
                    )
            ),
            # State space
            column(4,
            		helpText(Explorer_SS_label),
            		imageOutput(p$plotState, "auto", "auto",
            			click = p$clickState,
            			dblclick = p$doubleClickState,
            			brush = p$brushState,
            			hover = p$hoverState
        			)                       
            ),
            # State space controls
            column(2,
            	tooltip(tooltip = Explorer_SS_ApplyToAll_tooltip,
					actionButton(p$applyStateToAll, Explorer_SS_ApplyToAll_label)
        		),
        		tooltip(tooltip = Explorer_SS_ClearPlot_tooltip,
					actionButton(p$clearState, Explorer_SS_ClearPlot_label)
        		),
        		tooltip(tooltip = Explorer_SS_Unzoom_tooltip,
					actionButton(p$unzoomState, Explorer_SS_Unzoom_label)
        		),
        		uiOutput(p$scaleState),
        		tooltip(tooltip = Explorer_SS_HoverTextArea_tooltip,
        			verbatimTextOutput(p$exactState)
    			)                       
            )
        )		
	)		
}