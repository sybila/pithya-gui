if(!require(shiny,quietly = T)) {install.packages("shiny", dependencies=T,quiet = T); library(shiny,quietly = T)}
if(!require(shinyBS,quietly = T)) {install.packages("shinyBS", dependencies=T,quiet = T); library(shinyBS,quietly = T)}
if(!require(shinyAce,quietly = T)) {install.packages("shinyAce", dependencies=T,quiet = T); library(shinyAce,quietly = T)}
if(!require(shinyjs,quietly = T)) {install.packages("shinyjs", dependencies=T,quiet = T); library(shinyjs,quietly = T)}
require(parallel) # it is needed because of function for determination of available CPU cores
#if(!require(shinythemes,quietly=T)) install.packages("shinythemes",quiet=T); library(shinythemes,quietly=T)
#require(shiny)

source("tooltips.R")


## LOOK AT SHINY.OPTIONS
# customSlider javascript function for output threshold instead of index
JS.custom <-
    "
// function to get custom values into a sliderInput
function customSlider (sliderId,values) {
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (values[num]); }
    });

}"

# general handler function for scale sliders calling function customSlider
JS.scaleSliderHandler <- "
Shiny.addCustomMessageHandler('scaleSliderHandler',     
    function(data) {
        //var name = data['name'];
        //var values = data['values'];
        customSlider(data['name'],data['values']);
    }
);
"

# message handler for giving an information about finished prameter synthesis
JS.parameterSynthesisFinished <- "
Shiny.addCustomMessageHandler('paramSynthEnd', function(message) {
        eval(message.value);
});
"
# message handler for giving an information about missing threshold
JS.missingThreshold <- "
Shiny.addCustomMessageHandler('missThres', function(message) {
    Shiny.onInputChange('missing_threshold',eval(message.value));
    Shiny.onInputChange('missing_threshold_counter',Math.random());
    Shiny.onInputChange('missing_threshold_data',eval(message.data));
})
"

source("global.R")

shinyUI(
    fluidPage(
        useShinyjs(),
        tags$head(tags$script(HTML(JS.parameterSynthesisFinished))),
        tags$head(tags$script(HTML(JS.missingThreshold))),
        titlePanel(Tool_name),
        tags$div(title=Editor_advancedSettings_tooltip,
                 checkboxInput("advanced", Editor_advancedSettings_label, F)),
        tags$hr(),
        tabsetPanel(id = "dimensions",
                
tabPanel(Editor_label, icon=icon("bug"), # fa-leaf fa-bug
    tags$div(title=Editor_tooltip,
    fluidPage(
        column(6,
            helpText(Editor_model_controlPanel_label),
            wellPanel(
                fluidPage(
                    column(4,
                        tags$div(title=Editor_model_Browse_tooltip,
                                 fileInput("vf_file", Editor_model_Browse_label, accept=".bio"))
                        # ,tags$div(title="Select input state space (with '.ss.json' extension) for further analysis.",
                        #     fileInput("state_space_file","choose '.abst.bio' file",accept=".bio"))
                    ),
                    column(4,
                           verticalLayout(
                               # tags$div(title="This button accepts all changes made in model editor so they could be passed on to further analysis.",
                               #          actionButton("accept_model_changes","check syntax of model",icon=icon("ok",lib = "glyphicon"))),
                               tags$div(title=Editor_model_resetChangesInModel_tooltip,
                                        actionButton("reset_model", Editor_model_resetChangesInModel_label, icon=icon("remove",lib = "glyphicon"))),
                               tags$div(title=Editor_model_saveModel_tooltip,
                                        downloadButton("save_model_file", Editor_model_saveModel_label)
#                                         ,textInput("save_model_file_name", NULL, "model.bio", "100%", "model.bio")
                               )
                           )
                    ),
                    column(4,
                        # wellPanel(
                            conditionalPanel(
                                condition = "input.advanced == true",
                                tags$div(title=Editor_cutTresholds_tooltip,
                                    checkboxInput("thresholds_cut", Editor_cutTresholds_label,F)),
                                tags$div(title=Editor_fastApproximation_tooltip,
                                    checkboxInput("fast_approximation", Editor_fastApproximation_label,F))
                            ),
                            tags$div(title=Editor_generateApproximation_tooltip,
                                 bsButton("generate_abstraction", Editor_generateApproximation_label, disabled=T))
                        # )
                    )
                )
            )
        ),
#         tags$script(tags$html("<hr width='2' size='500' color='red'>")),
        column(4,
            helpText(Editor_property_controlPanel_label),
            wellPanel(
                fluidPage(
                    column(6,
                           tags$div(title=Editor_property_Browse_tooltip,
                                    fileInput("prop_file", Editor_property_Browse_label, accept=".ctl"))
                    ),
                    column(6,
                           # tags$div(title="This button accepts all changes made in editor so they could be passed on to further analysis.",
                           #          actionButton("accept_prop_changes","check syntax of properties",icon=icon("ok",lib = "glyphicon"))),
                           tags$div(title=Editor_property_resetChangesInProperties_tooltip,
                                    actionButton("reset_prop", Editor_property_resetChangesInProperties_label, icon=icon("remove",lib = "glyphicon"))),
                           tags$div(title=Editor_property_saveProperties_tooltip,
                                    downloadButton("save_prop_file", Editor_property_saveProperties_label))
                    )
                )
            )
        ),
        column(2,
               helpText(Editor_process_controlPanel_label),
               wellPanel(
                   conditionalPanel(
                       condition = "input.advanced == true",
                       tags$div(title=Editor_numberOfThreads_tooltip,
                                sliderInput("threads_number", Editor_numberOfThreads_label, value=detectCores(), 
                                             min=1, max=detectCores(), step=1))
                   ),
                   tags$div(title=Editor_runParameterSynthesis_tooltip,
                            bsButton("process_run", Editor_runParameterSynthesis_label, disabled=T)),
                   tags$div(title=Editor_stopParameterSynthesis_tooltip,
                            bsButton("process_stop", Editor_stopParameterSynthesis_label, disabled=T))
               )
        )
    ),
    # bsModal("missing_threshold","neco","",size="small",wellPanel(
    #     actionButton("yes_button", "Ok"),
    #     actionButton("no_button", "Cancel"))
    # ),
    tags$div(title=Editor_progressBar_tooltip,
             helpText(Editor_progressBar_label),
             verbatimTextOutput("progress_output")),
    fluidPage(
        column(6,
               tags$div(title=Editor_modelTextEditor_tooltip,
                        helpText(Editor_modelTextEditor_label),
                        aceEditor("model_input_area","","plain_text","textmate",debounce=100))
        ),
        column(6,
               tags$div(title=Editor_propertyTextEditor_tooltip,
                        helpText(Editor_propertyTextEditor_label),
                        aceEditor("prop_input_area","","plain_text","textmate",debounce=100))
        )
    ))
),

# tabPanel("property editor",icon=icon("gears"), # fa-eye fa-bolt fa-gears fa-key fa-heartbeat fa-fire fa-history fa-stethoscope
#          tags$div(title="'Property editor' allows the user load, edit or save properties of interest. One property is automatically loaded as example.",
#                   fluidPage(
#                       #theme = shinytheme("united"),
#                       column(3,
#                              tags$div(title="Select input model (with '.bio' extension) for further analysis.",
#                                       fileInput("prop_file","choose '.ctl' file",accept=".ctl"))
#                       ),
#                       column(3
#                       ),
#                       column(6,
#                              tags$div(title="This button accepts all changes made in editor so they could be passed on to further analysis.",
#                                       actionButton("accept_prop_changes","accept changes",icon=icon("ok",lib = "glyphicon"))),
#                              tags$div(title="This button resets all changes made up to last load or save of current file.",
#                                       actionButton("reset_prop","reset changes",icon=icon("remove",lib = "glyphicon"))),
#                              tags$div(title="This button saves current state of properties description.",
#                                       downloadButton("save_prop_file","save"))
#                              
#                       )
#                   ),
#                   tags$textarea(id="prop_input_area", rows=200, cols=300))
# ),

# tabPanel("process settings",icon=icon("wrench",lib = "glyphicon"), # fa-sliders fa-wrench
#     fluidPage(
#         column(2,
#                numericInput("threads_number","no. of threads",1,1,8,1)
#         ),
#         column(2
#         ),
#         column(2,
#                actionButton("process_run","Run model checking")
#                #,checkboxGroupInput("what_to_run","",c("model_checking"="mc", "states_generator"="sg"),c("mc","sg"))
#         )
#     ),
#     helpText("Progress bar:"),
#     verbatimTextOutput("progress_output")
# ),

tabPanel(Explorer_label, icon=icon("move",lib = "glyphicon"),
    tags$div(title=Explorer_tooltip,
    fluidPage(
        theme = "simplex.css",
        fluidRow(
            # column(2,
            #        uiOutput("model_help_text"),
            #        fluidRow(
            #            column(4,
            #                   tags$div(title=Explorer_previousExperiment_tooltip,
            #                            bsButton("model_prev",Explorer_previousExperiment_label,disabled=T,block=T)
            #                   )),
            #            column(4,
            #                   tags$div(title=Explorer_nextExperiment_tooltip,
            #                            bsButton("model_next",Explorer_nextExperiment_label,disabled=T,block=T)
            #                   )),
            #            column(4,
            #                   tags$div(title=Explorer_deleteExperiment_tooltip,
            #                            bsButton("model_del",Explorer_deleteExperiment_label,disabled=T,block=T)
            #                   ))
            #        ),
            #        tags$div(title=Explorer_saveExperimentModel_tooltip,
            #                 downloadButton("save_current_model_file",Explorer_saveExperimentModel_label))
            # ),
            column(4,
                   fluidRow(
                       column(6,
                              tags$div(title=Explorer_countOfDirectionArrows_tooltip,
                                    sliderInput("arrows_number",Explorer_countOfDirectionArrows_label,10,100,step=1,value=25)),
                              tags$div(title=Explorer_coloringThreshold_tooltip,
                                    sliderInput("colThr",Explorer_coloringThreshold_label,0,0.1,step=0.001,value=0.05))
                        ),
                       column(6,
                              tags$div(title=Explorer_lengthOfDirectionArrows_tooltip,
                                   sliderInput("arrowSize",Explorer_lengthOfDirectionArrows_label,0.01,10,step=0.05,value=0.3)),
                              tags$div(title=Explorer_widthOfAllArrows_tooltip,
                                   sliderInput("transWidth",Explorer_widthOfAllArrows_label,1,5,step=0.5,value=1.5))
                        )
                   ),
                   tags$div(title=Explorer_coloringDirection_tooltip,
                        radioButtons("colVariant",Explorer_coloringDirection_label,list(both="both",none="none",horizontal="horizontal",vertical="vertical"),"both",inline=T))
            ),
            column(2,
                   conditionalPanel(
                       condition = "input.advanced == true",
                       tags$div(title=Explorer_flowPointsCount_tooltip,
                                numericInput("flow_points_count",Explorer_flowPointsCount_label,min=50,max=NA,step=50,value=500)),
                       tags$div(title=Explorer_flowPointsDensity_tooltip,
                                sliderInput("flow_points_density",Explorer_flowPointsDensity_label,0.01,10,step=0.01,value=1))
                   )
#                   ,uiOutput("param_sliders")
            ),
            column(2,
                   uiOutput("param_sliders_bio")
            ),
#             column(2,
#                    sliderInput("height","height of plots",min=200,max=800,value=650,step=20),
#                    uiOutput("zoom_sliders"),
#                    actionButton("execute_zoom","zoom")
#             ),
            column(4,
                   uiOutput("selector"),
                   tags$div(title=Explorer_addPlot_tooltip,
                        bsButton("add_vf_plot",Explorer_addPlot_label,icon=icon("picture",lib="glyphicon"), disabled=T))
            )
        ),
        tags$hr(),
        uiOutput("plots")
    ))
),


tabPanel(Result_label, icon=icon("barcode",lib = "glyphicon"),
    tags$div(title=Result_tooltip,            
    fluidPage(
        theme = "simplex.css",
        fluidRow(
            column(2,
                   # uiOutput("result_help_text"),
                   # fluidRow(
                   #     column(4,
                   #            tags$div(title=Result_previousExperiment_tooltip,
                   #                     bsButton("result_prev",Result_previousExperiment_label,disabled=T,block=T)
                   #     )),
                   #     column(4,
                   #            tags$div(title=Result_nextExperiment_tooltip,
                   #                     bsButton("result_next",Result_nextExperiment_label,disabled=T,block=T)
                   #     )),
                   #     column(4,
                   #            tags$div(title=Result_deleteExperiment_tooltip,
                   #                     bsButton("result_del",Result_deleteExperiment_label,disabled=T,block=T)
                   #     ))
                   # ),
                tags$div(title=Result_Browse_tooltip,
                         fileInput("ps_file", Result_Browse_label,accept=".json"))
                ,fluidRow(
                #     column(6,
                #            tags$div(title=Result_BrowseReload_tooltip,
                #                     bsButton("reload_result_file", Result_BrowseReload_label, disabled=T))),
                column(6,
                       tags$div(title=Result_saveResults_tooltip,
                                downloadButton("save_result_file",Result_saveResults_label)))
                )
            ),
            column(2,
                tags$div(title=Result_showParametersCoverage_tooltip,
                    checkboxInput("coverage_check", Result_showParametersCoverage_label, F)),
                conditionalPanel(
                    condition = "input.coverage_check == true",
                    tags$div(title=Result_greyShadeDegree_tooltip,
                             sliderInput("color_alpha_coeficient", Result_greyShadeDegree_label,min=0,max=1,value=0.9,step=0.01,ticks=F)),
                    tags$div(title=Result_parameterDensity_tooltip,
                             sliderInput("density_coeficient",Result_parameterDensity_label,min=10,max=150,value=50,step=1,ticks=F))
                ),
                uiOutput("ps_zoom_sliders")
            ),
            column(4,
                uiOutput("chosen_ps_states_ui")
            ),
            column(4,
                   uiOutput("param_selector"),
                   tags$div(title=Result_addPlot_tooltip,
                        bsButton("add_param_plot",Result_addPlot_label,icon=icon("picture",lib="glyphicon"), disabled=T))
            )
        ),
        tags$hr(),
        uiOutput("param_space_plots")
    ))
)

    )
))
              
              
              