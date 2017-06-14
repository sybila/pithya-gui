source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")       # UI utilities

editorTab <- function() {
    tabPanel(Editor_label, icon=icons$bug,
        tooltip(tooltip = Editor_tooltip,            
            editorControlPanel(),
            fluidPage(
                column(6, 
                    tooltip(tooltip = Editor_modelTextEditor_tooltip,
                        helpText(Editor_modelTextEditor_label),
                        aceEditor("model_input_area","","plain_text","textmate",debounce=100)
                    )
                ),
                column(6,
                    tooltip(tooltip = Editor_propertyTextEditor_tooltip,
                        helpText(Editor_propertyTextEditor_label),
                        aceEditor("prop_input_area","","plain_text","textmate",debounce=100)
                    )
                )
            )
        )
    )
}

editorControlPanel <- function() {

    # Model controls panel - load, save and run approximation
    modelControls <- fluidPage(
        column(4, class = "model_file_column",
            tooltip(tooltip = Editor_model_Browse_tooltip,
                fileInput("model_file", Editor_model_Browse_label, accept=".bio")
            )  
        ),
        column(4, class = "model_buttons_column",
            tooltip(tooltip = Editor_model_resetChangesInModel_tooltip,
                actionButton("reset_model", Editor_model_resetChangesInModel_label, icon=icons$remove)
            ),
            tooltip(tooltip = Editor_model_saveModel_tooltip,
                downloadButton("save_model_file", Editor_model_saveModel_label)
            )  
        ),
        column(4, class = "model_approx_column",
            advanced(
                tooltip(tooltip = Editor_cutTresholds_tooltip,
                    checkboxInput("thresholds_cut", Editor_cutTresholds_label,F)
                ),
                tooltip(tooltip = Editor_fastApproximation_tooltip,
                    checkboxInput("fast_approximation", Editor_fastApproximation_label,F)
                )
            ),
            tooltip(tooltip = Editor_generateApproximation_tooltip,
                bsButton("generate_abstraction", Editor_generateApproximation_label, disabled=T, style="default")
            )
        )
    )    

    # Property control panel - load and save
    propertyControls <- fluidPage(
        column(6, class = "property_file_column",
            tooltip(tooltip = Editor_property_Browse_tooltip,
                fileInput("prop_file", Editor_property_Browse_label, accept=".ctl")
            )
        ),
        column(6, class = "property_buttons_column",
            verticalLayout(
                tooltip(tooltip = Editor_property_resetChangesInProperties_tooltip,
                    actionButton("reset_prop", Editor_property_resetChangesInProperties_label, icon=icons$remove)
                ),
                tooltip(tooltip = Editor_property_saveProperties_tooltip,
                    downloadButton("save_prop_file", Editor_property_saveProperties_label)
                )
            )
        )
    )

    # Synthesis control panel - start and stop
    synthesisControls <- fluidPage(
        advanced(
            tooltip(tooltip = Editor_numberOfThreads_tooltip,
                sliderInput("threads_number", Editor_numberOfThreads_label, value = 1, #detectCores(), 
                            min=1, max=detectCores(), step=1)
            )
        ),
        tooltip(tooltip = Editor_runParameterSynthesis_tooltip,
            bsButton("process_run", Editor_runParameterSynthesis_label, disabled=T, style="default")
        # ),
        # tooltip(tooltip = Editor_stopParameterSynthesis_tooltip,
        #     bsButton("process_stop", Editor_stopParameterSynthesis_label, disabled=T)
        )
    )

    # And put it all together
    fluidPage(
        column(6, class = "model_control_panel", tags$div(class = "border", titlePanel(Editor_model_controlPanel_label), hr(), modelControls)),
        column(4, class = "property_control_panel", tags$div(class = "border", titlePanel(Editor_property_controlPanel_label), hr(), propertyControls)),
        column(2, class = "synth_control_panel", tags$div(class = "border", titlePanel(Editor_process_controlPanel_label), hr(), synthesisControls))
    )
}