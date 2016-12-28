if(!require(shiny,quietly = T)) {install.packages("shiny", dependencies=T,quiet = T); library(shiny,quietly = T)}
if(!require(shinyBS,quietly = T)) {install.packages("shinyBS", dependencies=T,quiet = T); library(shinyBS,quietly = T)}
if(!require(shinyAce,quietly = T)) {install.packages("shinyAce", dependencies=T,quiet = T); library(shinyAce,quietly = T)}
if(!require(shinyjs,quietly = T)) {install.packages("shinyjs", dependencies=T,quiet = T); library(shinyjs,quietly = T)}
require(parallel) # it is needed because of function for determination of available CPU cores
#if(!require(shinythemes,quietly=T)) install.packages("shinythemes",quiet=T); library(shinythemes,quietly=T)
#require(shiny)


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
Shiny.addCustomMessageHandler('jsCode',
    function(message) {
        eval(message.value);
});
"

source("global.R")

shinyUI(
    fluidPage(
        useShinyjs(),
        tags$head(tags$script(HTML(JS.parameterSynthesisFinished))),
    # titlePanel("PITHYA - Parameter Investigation Tool with HYbrid Approach"),
    titlePanel("PITHYA - Parameter Investigation Tool for HYbrid Analysis"),
    tags$hr(),
    tabsetPanel(id = "dimensions",
                
tabPanel("model editor",icon=icon("bug"), # fa-leaf fa-bug
#          tags$head(tags$script((JS.custom))),
#          tags$head(tags$script((JS.scaleSliderHandler))),
    tags$div(title="'Model editor' allows the user load, edit or save input model. Simple model is automatically loaded as example.",
    fluidPage(
        column(6,
            wellPanel(
                fluidPage(
                    column(4,
                        checkboxInput("advanced","advanced settings",F),
                        tags$div(title="Select input model (with '.bio' extension) for further analysis.",
                            fileInput("vf_file","choose '.bio' file",accept=".bio"))
                        # ,tags$div(title="Select input state space (with '.ss.json' extension) for further analysis.",
                        #     fileInput("state_space_file","choose '.abst.bio' file",accept=".bio"))
                    ),
                    column(4,
                           verticalLayout(
                               # tags$div(title="This button accepts all changes made in model editor so they could be passed on to further analysis.",
                               #          actionButton("accept_model_changes","check syntax of model",icon=icon("ok",lib = "glyphicon"))),
                               tags$div(title="This button resets all changes made up to last load or save of current file.",
                                        actionButton("reset_model","reset changes in model",icon=icon("remove",lib = "glyphicon"))),
                               tags$div(title="This button saves current state of model description.",
                                        downloadButton("save_model_file","save model")
#                                         ,textInput("save_model_file_name", NULL, "model.bio", "100%", "model.bio")
                               )
                           )
                    ),
                    column(4,
                        # wellPanel(
                            conditionalPanel(
                                condition = "input.advanced == true",
                                tags$div(title="During The-PWA-approximation of special functions used inside the model new thresholds are generated and some of them could exceed explicit ones. By this checkbox you can tick off this do not be allowed.",
                                    checkboxInput("thresholds_cut","cut thresholds",F)),
                                tags$div(title="Two versions of The-PWA-approximation are available. Slower one - more precise and computationally more demanding - and fast one - much faster but also less precise.",
                                    checkboxInput("fast_approximation","fast approximation",F))
                            ),
                            tags$div(title="",
                                 bsButton("generate_abstraction","generate approximation",disabled=T))
                        # )
                    )
                )
            )
        ),
#         tags$script(tags$html("<hr width='2' size='500' color='red'>")),
        column(4,
            wellPanel(
                fluidPage(
                    column(6,
                           tags$div(title="Select input model (with '.bio' extension) for further analysis.",
                                    fileInput("prop_file","choose '.ctl' file",accept=".ctl"))
                    ),
                    column(6,
                           # tags$div(title="This button accepts all changes made in editor so they could be passed on to further analysis.",
                           #          actionButton("accept_prop_changes","check syntax of properties",icon=icon("ok",lib = "glyphicon"))),
                           tags$div(title="This button resets all changes made up to last load or save of current file.",
                                    actionButton("reset_prop","reset changes in properties",icon=icon("remove",lib = "glyphicon"))),
                           tags$div(title="This button saves current state of properties description.",
                                    downloadButton("save_prop_file","save properties"))
                    )
                )
            )
        ),
        column(2,
               wellPanel(
                   conditionalPanel(
                       condition = "input.advanced == true",
                       numericInput("threads_number","no. of threads",detectCores(),1,detectCores(),1)
                   ),
                   bsButton("process_run","run parameter synthesis",disabled=T),
                   bsButton("process_stop","stop parameter synthesis",disabled=T)
               )
        )
    ),
    helpText("Progress bar:"),
    verbatimTextOutput("progress_output"),
    fluidPage(
        column(6,
               helpText("Model editor:"),
               aceEditor("model_input_area","","plain_text","textmate")
               # tags$textarea(id="model_input_area", rows=200, cols=300)
               # textAreaInput("model_input_area",NULL,width=NULL,height=NULL,rows=200,cols=100,resize="both")
        ),
        column(6,
               helpText("Properties editor:"),
               aceEditor("prop_input_area","","plain_text","textmate")
               # tags$textarea(id="prop_input_area", rows=200, cols=300)
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

tabPanel("model explorer", icon=icon("move",lib = "glyphicon"),
    fluidPage(
        theme = "simplex.css",
        fluidRow(
            column(2,
                   uiOutput("model_help_text"),
                   fluidRow(
                       column(4,
                              tags$div(title="",
                                       bsButton("model_prev","previous",disabled=T,block=T)
                              )),
                       column(4,
                              tags$div(title="",
                                       bsButton("model_next","next",disabled=T,block=T)
                              )),
                       column(4,
                              tags$div(title="",
                                       bsButton("model_del","delete",disabled=T,block=T)
                              ))
                   ),
                   tags$div(title="This button saves model description in *.bio format of current experiment.",
                            downloadButton("save_current_model_file","save current model"))
            ),
            column(4,
                   fluidRow(
                       column(6,
                              tags$div(title="Total number of direction arrows per dimension inside vector field(s)",
                                    sliderInput("arrows_number","count of direction arrows",10,100,step=1,value=25)),
                              tags$div(title="",
                                    sliderInput("colThr","coloring threshold",0,0.1,step=0.001,value=0.05))
                        ),
                       column(6,
                              tags$div(title="Scaling factor for length of direction arrows inside vector field(s)",
                                   sliderInput("arrowSize","length of direction arrows",0.01,2,step=0.05,value=0.3)),
                              tags$div(title="Scaling factor for width of arrows inside vector field(s) and transition-state space(s)",
                                   sliderInput("transWidth","width of all arrows",1,5,step=0.5,value=1.5))
                        )
                   ),
                   tags$div(title="",
                        radioButtons("colVariant","coloring direction",list(both="both",none="none",horizontal="horizontal",vertical="vertical"),"both",inline=T))
            ),
            column(2,
                   uiOutput("param_sliders_bio")
            ),
            column(2
#                   ,uiOutput("param_sliders")
            ),
#             column(2,
#                    sliderInput("height","height of plots",min=200,max=800,value=650,step=20),
#                    uiOutput("zoom_sliders"),
#                    actionButton("execute_zoom","zoom")
#             ),
            column(4,
                   uiOutput("selector"),
                   tags$div(title="Button will add new layer of plots for vector field or transition-state space or both, depending on which type of input was loaded. 
                                    Then you will be able to play with it.",
                        bsButton("add_vf_plot","add plot",icon=icon("picture",lib="glyphicon"), disabled=T))
            )
        ),
        tags$hr(),
        uiOutput("plots")
    )
),


tabPanel("result explorer",icon=icon("barcode",lib = "glyphicon"),
    fluidPage(
        column(2,
               uiOutput("result_help_text"),
               fluidRow(
                   column(4,
                          tags$div(title="",
                                   bsButton("result_prev","previous",disabled=T,block=T)
                   )),
                   column(4,
                          tags$div(title="",
                                   bsButton("result_next","next",disabled=T,block=T)
                   )),
                   column(4,
                          tags$div(title="",
                                   bsButton("result_del","delete",disabled=T,block=T)
                   ))
               ),
            tags$div(title="Select input parameter space (with '.ps.json' extension) for further analysis.",
                fileInput("ps_file","choose result '.json' file"),accept=".json"),
            fluidRow(
                column(6,
                       tags$div(title="",
                                bsButton("reload_result_file","reload",disabled=T))),
                column(6,
                       tags$div(title="This button saves model checking results.",
                                downloadButton("save_result_file","save results")))
            )
        ),
        column(2,
            tags$div(title="Scaling factor for density of shown rectangles in parameter space.",
                checkboxInput("coverage_check","show parameters coverage",F)),
            conditionalPanel(
                condition = "input.coverage_check == true",
                tags$div(title="Scaling factor in the range <0,1> for shade of colour representing parameters.",
                         sliderInput("color_alpha_coeficient","grey shade degree",min=0,max=1,value=0.9,step=0.01,ticks=F)),
                tags$div(title="Scaling factor in the range <10,150> for density of shown rectangles in parameter space.",
                         sliderInput("density_coeficient","density",min=10,max=150,value=50,step=1,ticks=F))
            ),
            uiOutput("ps_zoom_sliders")
        ),
        column(4,
            uiOutput("chosen_ps_states_ui")
        ),
        column(4,
               uiOutput("param_selector"),
               tags$div(title="Button will add new layer of plots for parameter space and corresponding transition-state space as soon as some file is loaded. 
                                    Then you will be able to play with it.",
                    bsButton("add_param_plot","add plot",icon=icon("picture",lib="glyphicon"), disabled=T))
        )
    ),
    tags$hr(),
    uiOutput("param_space_plots")
)
    )
)
)
              
              
              