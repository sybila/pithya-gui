# in linux install: r-base-dev

source("config.R")          # global configuration
source("tooltips.R")        # texts

# Utility files
source("bio.R")             # .bio file parser and utilities
source("result.R")          # result .json parser

# Separate tab servers
source("editor/server.R")
source("explorer/server.R")
source("result/server.R")

shinyServer(function(input,output,session) {

debug("Reset")
mySession <- list(shiny=session, pithya=list(
    # TODO remove sample file
    approximatedModel = reactiveValues(file = NULL, model = NULL, outdated = FALSE),
    #synthesisResult = reactiveValues(file = NULL, result = NULL, outdated = TRUE),
    synthesisResult = reactiveValues(
        file = NULL,#"example/repressilator_2D/model_dep.results.json", 
        result = parseResultFile("example/repressilator_2D/model_dep.results.json"), 
        outdated = FALSE
    ),
    sessionDir = tempdir(),
    examplesDir = "example//",
    nextId = createCounter(1)
))

# Parse .bio model when approximation changes
observeEvent(mySession$pithya$approximatedModel$file, {
    file <- mySession$pithya$approximatedModel$file
    if (!is.null(file)) {
        tryCatch({
            mySession$pithya$approximatedModel$model <- parseBioFile(file)
        }, error = function(e) {
            debug("[.bio parser] parsing error: ", e)
            showNotification("[INTERNAL ERROR] Model parsing failed")
            mySession$pithya$approximatedModel$file <- NULL
            mySession$pithya$approximatedModel$model <- NULL
        })        
    } else {
        mySession$pithya$approximatedModel$model <- NULL
    }
})

# Parse result .json files
observeEvent(mySession$pithya$synthesisResult$file, {
    file <- mySession$pithya$synthesisResult$file
    if (!is.null(file)) {
        tryCatch({
            mySession$pithya$synthesisResult$result <- parseResultFile(file)
        }, error = function(e) {
            debug("[result parser] parsing error: ", e)
            showNotification("[INTERNAL ERROR] Result parsing failed")
            mySession$pithya$synthesisResult$file <- NULL
            mySession$pithya$synthesisResult$result <- NULL
        })
    } else {
        mySession$pithya$synthesisResult$result <- NULL         
    }
})

editorServer(input, mySession, output)
explorerServer(input, mySession, output)
resultServer(input, mySession, output)

#=============== GLOBALS ===============================================
session_random <- sample(1000^2,1)
    
# .Platform$OS.type=="windows"  or Sys.info()["sysname"]=="Windows"
files_path <- paste0("..//Temp//")
# files_path <- ifelse(.Platform$OS.type=="windows", paste0("..//Temp//"), ifelse(Sys.info()["nodename"]=="psyche05",paste0("..//Temp//"),paste0("~//skola//newbiodivine//") ))
new_programs_path <- paste0("..//biodivine-ctl//build//install//biodivine-ctl//bin//")
# new_programs_path <- ifelse(.Platform$OS.type=="windows", paste0("..//biodivine-ctl//build//install//biodivine-ctl//bin//"), 
#                             ifelse(Sys.info()["nodename"]=="psyche05","..//biodivine-ctl//build//install//biodivine-ctl//bin//","~//skola//newbiodivine//"))

progressFileName <- paste0(files_path,"progress.",session_random,".txt")
file.create(progressFileName)
progressFile <- reactiveFileReader(100,session,progressFileName,readLines)

resultFileName <- paste0(files_path,"result.",session_random,".json")
file.create(resultFileName)
resultFile <- reactiveFileReader(1000,session,resultFileName,readLines)

configFileName <- paste0(files_path,"config.",session_random,".json")
file.create(configFileName)

session$onSessionEnded(function() {
    for(i in c(progressFileName,resultFileName,configFileName)) if(file.exists(i)) file.remove(i)
    #stopApp() # very important for VM so the user will run everytime new and clean instance of the app
})

hide("save_current_model_file")
# hide("save_result_file")
# hide("save_model_file")
# hide("save_prop_file")

loaded_prop_file    <- reactiveValues(data=NULL,filedata=NULL,filename=NULL)
loaded_vf_file      <- reactiveValues(data=NULL,filedata=NULL,filename=NULL)
loaded_ss_file      <- reactiveValues(data=NULL,filedata=NULL,filename=NULL)
loaded_ps_file      <- reactiveValues(data=NULL,filedata=NULL,filename=NULL)

# stored_vf_files     <- reactiveValues(data=list(),current=0,max=1)
# stored_vf_current_params    <- reactiveValues(data=list())
# stored_vf_chosen    <- reactiveValues(data=list())
# stored_vf_parsed_data   <- reactiveValues(data=list())
# stored_ss_files     <- reactiveValues(data=list(),current=0,max=1)
# stored_ss_parsed_data   <- reactiveValues(data=list())
# stored_ps_files     <- reactiveValues(data=list(),current=0,max=1)
# stored_ps_current_formula    <- reactiveValues(data=list())
# stored_ps_parsed_data   <- reactiveValues(data=list())
# stored_ps_chosen    <- reactiveValues(data=list())

vf_brushed          <- reactiveValues(data=list(),click_counter=list())
ss_brushed          <- reactiveValues(data=list(),click_counter=list())
ps_brushed          <- reactiveValues(data=list(),click_counter=list())
param_ss_brushed    <- reactiveValues(data=list(),click_counter=list())

vector_field_space  <- reactiveValues(data=list(),globals=list())
transition_state_space<- reactiveValues(data=list(),globals=list())
param_state_space   <- reactiveValues(data=list(),globals=list())
param_space         <- reactiveValues(data=list(),globals=list())
#satisfiable_ps      <- reactiveValues(data=list())

vector_field_clicked  <- reactiveValues(data=list(),point=list(),old_point=list(),click_counter=list(),apply_to_all_click_counter=list(),apply_to_tss_click_counter=list(),
                                        old_count=list(),old_dense=list())
state_space_clicked   <- reactiveValues(data=list(),point=list(),old_point=list(),click_counter=list(),apply_to_all_click_counter=list())
param_ss_clicked      <- reactiveValues(data=list(),point=list(),old_point=list(),click_counter=list(),apply_to_all_click_counter=list())
param_space_clicked   <- reactiveValues(data=list(),point=list(),old_point=list(),click_counter=list(),apply_to_all_click_counter=list())

vf_chosen           <- reactiveValues(data=NULL,max=1)
param_chosen        <- reactiveValues(data=NULL,max=1)

abstraction_reactivation <- reactiveValues(counter=0)

#=============== MODEL INPUT TAB =======================================
#=======================================================================

reset_globals <- function(i=NA) {
    if(is.na(i)) {
        for(i in vf_update()) reset_particular_global(i)
    } else {
        reset_particular_global(i)
    }
}
reset_particular_global <- function(i) {
    # stored_vf_chosen$data[[stored_vf_files$current]] <- vf_update()[vf_update() != i]
    vf_chosen$data <- vf_update()[vf_update() != i]
    # some kind of garbage collector would be very convenient in this phase
    # either gc() or rm()
    #                rm(input[[paste0("vf_selector_x_",i)]], input[[paste0("vf_selector_y_",i)]]) #TODO: doplnit dalsie
    
    # removeUI function
    transition_state_space$data[[i]] <- NA
    transition_state_space$globals[[i]] <- NA
    state_space_clicked$data[[i]] <- NA
    state_space_clicked$click_counter[[i]] <- NA
    state_space_clicked$apply_to_all_click_counter[[i]] <- NA
    state_space_clicked$old_point[[i]] <- NA
    state_space_clicked$point[[i]] <- NA
    ss_brushed$data[[i]] <- NA
    ss_brushed$click_counter[[i]] <- NA
    
    vector_field_space$data[[i]] <- NA
    vector_field_space$globals[[i]] <- NA
    vector_field_clicked$data[[i]] <- NA
    vector_field_clicked$apply_to_all_click_counter[[i]] <- NA
    vector_field_clicked$apply_to_tss_click_counter[[i]] <- NA
    vector_field_clicked$click_counter[[i]] <- NA
    vector_field_clicked$point[[i]] <- NA
    vector_field_clicked$old_point[[i]] <- NA
    vector_field_clicked$old_count[[i]] <- NA
    vector_field_clicked$old_dense[[i]] <- NA
    vf_brushed$data[[i]] <- NA
    vf_brushed$click_counter[[i]] <- NA
    
    print(gc())
}

################################################################################################################################################################
#=============== RESULT TAB ============================================
#=======================================================================

reset_globals_param <- function(i=NA) {
    if(is.na(i)) {
        for(i in param_update()) reset_particular_global_param(i)
    } else {
        reset_particular_global_param(i)
    }
}
reset_particular_global_param <- function(i) {
    # stored_ps_chosen$data[[stored_ps_files$current]] <- param_update()[param_update() != i]
    param_chosen$data <- param_update()[param_update() != i]
    # some kind of garbage collector would be very convenient in this phase
    # either gc() or rm()
    #rm(input[[paste0("vf_selector_x_",i)]], input[[paste0("param_selector_y_",i)]]) #TODO: doplnit dalsie
    param_space$data[[i]] <- NA
    param_space$globals[[i]] <- NA
    param_space_clicked$data[[i]] <- NA
    param_space_clicked$click_counter[[i]] <- NA
    param_space_clicked$apply_to_all_click_counter[[i]] <- NA
    param_space_clicked$old_point[[i]] <- NA
    param_space_clicked$point[[i]] <- NA
    ps_brushed$data[[i]] <- NA
    ps_brushed$click_counter[[i]] <- NA
    
    param_state_space$data[[i]] <- NA
    param_state_space$globals[[i]] <- NA
    param_ss_clicked$data[[i]] <- NA
    param_ss_clicked$apply_to_all_click_counter[[i]] <- NA
    param_ss_clicked$click_counter[[i]] <- NA
    param_ss_clicked$point[[i]] <- NA
    param_ss_clicked$old_point[[i]] <- NA
    param_ss_brushed$data[[i]] <- NA
    param_ss_brushed$click_counter[[i]] <- NA
    
    # print(gc())
}

observeEvent(c(resultFile()),{
    if(!is.null(resultFile()) && length(resultFile()) > 0) {
        loaded_ps_file$filename <- resultFileName
        loaded_ps_file$filedata <- readLines(loaded_ps_file$filename)
    }
})

observeEvent(c(input$ps_file,input$reload_result_file),{
    if(!is.null(input$ps_file) && !is.null(input$ps_file$datapath)) {
        loaded_ps_file$filename <- input$ps_file$name
        loaded_ps_file$filedata <- readLines(input$ps_file$datapath)
        updateButton(session,"add_param_plot",style="default",disabled=F)
    }
})
observeEvent(input$reload_result_file,{
    updateButton(session,"reload_result_file",disabled=T)
})

loading_ps_file <- reactive({
    
})


output$chosen_ps_states_ui <- renderUI({
    if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$param_space) != 0) {
        id <- paste0("chosen_ps_formula_",1)
        formulae_list <- loading_ps_file()$formulae
        names(formulae_list) <- loading_ps_file()$formulae
        selected_formula <- 1
        # selected_formula <- ifelse(!is.null(input[[id]]), input[[id]], 1)
        # selected_formula <- ifelse(length(stored_ps_current_formula$data) < stored_ps_files$current || isempty(stored_ps_current_formula$data[[stored_ps_files$current]]),
        #                          1,
        #                          stored_ps_current_formula$data[[stored_ps_files$current]])
        list(
            tags$div(title=Result_chooseFormulaOfInterest_tooltip,
                     selectInput(id, Result_chooseFormulaOfInterest_label, formulae_list, selected_formula, selectize=F, size=1, width="100%"))
        )
    } else
        h3(Result_chooseFormulaOfInterest_error)
})
chosen_ps_formulae_clean <- eventReactive(input[[paste0("chosen_ps_formula_",1)]],{
    # stored_ps_files$data[[stored_ps_files$current]]$data$chosen_ps_formula <- input$chosen_ps_formula
    return(input[[paste0("chosen_ps_formula_",1)]])
})


output$param_selector <- renderUI({
    if(!is.null(loading_ps_file())) {
        list_of_param_names <- as.list(c(loading_ps_file()$param_names, loading_ps_file()$var_names, "Choose"=empty_sign))
        #list_of_param_names <- as.list(c(loading_ps_file()$param_names, "Choose"=empty_sign))
        lapply(param_update(), function(i) {
            idx <- paste0("param_selector_x_",i)
            labelx <- paste0(Result_horizontal_label)# axis in plot ",i)
            choicesx <- list_of_param_names
            selectedx <- ifelse(!is.null(input[[paste0("param_selector_x_",i)]]), input[[paste0("param_selector_x_",i)]], #empty_sign)
                                list_of_param_names[1])
            
            idy <- paste0("param_selector_y_",i)
            labely <- paste0(Result_vertical_label)# axis in plot ",i)
            choicesy <- list_of_param_names
            selectedy <- ifelse(!is.null(input[[paste0("param_selector_y_",i)]]), input[[paste0("param_selector_y_",i)]], #empty_sign)
                                ifelse(length(list_of_param_names) > 1, list_of_param_names[2], list_of_param_names[1]))
            
            fluidRow(
                column(5,
                       tags$div(title=Result_horizontal_tooltip,
                                selectInput(idx,labelx,choicesx,selectedx))
                ),
                column(5,
                       tags$div(title=Result_vertical_tooltip,
                                selectInput(idy,labely,choicesy,selectedy))
                )
            )
        })
    }
})


# observer caring for hiding plot if some of selectors are invalidating
observe({
    if(!is.null(loading_ps_file())) {
        for(i in param_update()) {
            if(!is.null(input[[paste0("param_selector_x_",i)]]) && input[[paste0("param_selector_x_",i)]] != empty_sign &&
                   !is.null(input[[paste0("param_selector_y_",i)]]) && input[[paste0("param_selector_y_",i)]] != empty_sign )
               # && (!input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names || !input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names))
#                   (length(loading_ps_file()$param_names) == 1 || !is.null(input[[paste0("param_selector_y_",i)]]) && input[[paste0("param_selector_y_",i)]] != empty_sign))
                next #updateCheckboxInput(session,paste0("hide_ps_",i),value=F)
            else
                updateCheckboxInput(session,paste0("hide_ps_",i),value=T)
        }
    }
})
# observer caring for showing last plot after selector(s) is(are) properly selected
observe({
    if(!is.null(loading_ps_file())) {
        i <- param_update()[length(param_update())]
        if(!is.null(input[[paste0("param_selector_x_",i)]]) && input[[paste0("param_selector_x_",i)]] != empty_sign &&
               !is.null(input[[paste0("param_selector_y_",i)]]) && input[[paste0("param_selector_y_",i)]] != empty_sign )
           # && (!input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names || !input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names))
#           (length(loading_ps_file()$param_names) == 1 || !is.null(input[[paste0("param_selector_y_",i)]]) && input[[paste0("param_selector_y_",i)]] != empty_sign))
            updateCheckboxInput(session,paste0("hide_ps_",i),value=F)
    }
})
# observer caring for canceling a plot
observe({
    if(!is.null(loading_ps_file())) {
        for(i in param_update()) {
            if(!is.null(input[[paste0("cancel_ps_",i)]]) && input[[paste0("cancel_ps_",i)]] > 0) {
                reset_globals_param(i)
            }
        }
    }
})
# observer providing new selector(s) and cancel button and hide checkbox after 'add plot' button is clicked
observeEvent(input$add_param_plot,{
    if(!is.null(loading_ps_file())) {
        # very important initialisation of unzoom button click counters
        ps_brushed$click_counter[[param_chosen$max]] <- -1
        param_ss_brushed$click_counter[[param_chosen$max]] <- -1
        
        param_space_clicked$old_point[[param_chosen$max]] <- NA
        param_space_clicked$click_counter[[param_chosen$max]] <- -1
        param_space_clicked$apply_to_all_click_counter[[param_chosen$max]] <- -1
        
        param_ss_clicked$point[[param_chosen$max]] <- vector()
        param_ss_clicked$old_point[[param_chosen$max]] <- NA
        param_ss_clicked$click_counter[[param_chosen$max]] <- -1
        param_ss_clicked$apply_to_all_click_counter[[param_chosen$max]] <- -1
        
        param_space$globals[[param_chosen$max]] <- NA
        param_state_space$globals[[param_chosen$max]] <- NA
        #satisfiable_ps$data[[param_chosen$max]] <- NA
        
        # stored_ps_chosen$data[[stored_ps_files$current]] <- c(param_update(),param_chosen$max)
        param_chosen$data <- c(param_update(),param_chosen$max)
        param_chosen$max  <- param_chosen$max + 1
    }
})
param_update <- reactive({
    # if(length(stored_ps_chosen$data) < stored_ps_files$current) return(list())
    # else return(stored_ps_chosen$data[[stored_ps_files$current]])
    # return(preloading_ps_file()$data$param_chosen$data)
    return(param_chosen$data)
})
visible_ps_plots <- reactive({
    if(!is.null(loading_ps_file())) {
        local_result <- c()
        for(i in param_update()) {
            if(!is.null(input[[paste0("param_selector_x_",i)]]) && input[[paste0("param_selector_x_",i)]] != empty_sign && !input[[paste0("hide_ps_",i)]] &&
                   !is.null(input[[paste0("param_selector_y_",i)]]) && input[[paste0("param_selector_y_",i)]] != empty_sign)
                local_result <- c(local_result,i)
        }
        return(local_result)
    } else return(NULL)
})


output$param_space_plots <- renderUI({
    input$add_param_plot
    if(!is.null(loading_ps_file())) {
        list_of_var_names <- as.list(c(loading_ps_file()$var_names, "Choose"=empty_sign))
        list_of_all_names <- c(loading_ps_file()$param_names, loading_ps_file()$var_names)
        list_of_param_names <- loading_ps_file()$param_names
        one_line <- lapply(visible_ps_plots(), function(i) {
            if(input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names && input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names)
                h3(Result_noParameterSelected_error)
            else
            fluidRow(
                column(2,
                       #actionButton(paste0("apply_plot_ps_",i),"Apply to all"),
                       tags$div(title=Result_PS_DeselectClick_tooltip,
                                actionButton(paste0("clear_plot_ps_",i),Result_PS_DeselectClick_label)),
                       tags$div(title=Result_PS_Unzoom_tooltip,
                                actionButton(paste0("unzoom_plot_ps_",i),Result_PS_Unzoom_label)),
                       lapply(1:length(list_of_all_names), function(t) {
                          if(!list_of_all_names[[t]] %in% c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]])) {
                              is_mixed <- input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names
                              is_var <- list_of_all_names[[t]] %in% loading_ps_file()$var_names
                              label <- paste0(Result_PS_ScaleSlider_label,list_of_all_names[[t]])
                              name <- paste0("scale_slider_ps_",i,"_",t)
                              if(is_var)    values <- range(loading_ps_file()$thresholds[[ which(loading_ps_file()$var_names == list_of_all_names[[t]]) ]])
                              else          values <- param_ranges()[[t]]
                              fluidRow(
                                  column(12,
                                         fluidRow(
                                             column(1,
                                                conditionalPanel(
                                                    condition = "input.advanced == true",
                                                    tags$div(title=Result_PS_ScaleSwitch_tooltip,
                                                             checkboxInput(paste0("scale_switch_ps_",i,"_",t), label=NULL,
                                                                           ifelse(!is.null(input[[paste0("scale_switch_ps_",i,"_",t)]]), 
                                                                                  input[[paste0("scale_switch_ps_",i,"_",t)]], ifelse(is_var, F, T))))
                                                )
                                             ),
                                             column(11,
                                                if(input$advanced || !is_var)
                                                    helpText(label)
                                             )
                                         ),
                                         conditionalPanel(
                                             condition = paste0("input.scale_switch_ps_",i,"_",t," == true"),
                                             tags$div(title=Result_PS_ScaleSlider_tooltip,
                                                      sliderInput(name,label=NULL,min=values[1],max=values[2],step=0.001,
                                                         value=ifelse(is.null(input[[name]]), values[1], input[[name]] )))
                                         )
                                  )
                              )
                          }
                       }),

                    tags$div(title=Result_PS_HoverTextArea_tooltip,
                             verbatimTextOutput(paste0("hover_text_ps_",i)))
                ),
                column(4,
                       if(input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names) {
                           helpText(Result_PSmixed_label)
                       } else {
                           helpText(Result_PS_label)
                       },
                       div(id = "plot-container",
                           tags$img(
                               # src = "circle.png",
                               src = "spinner.gif",
                               id = "loading-spinner"),
                           imageOutput(paste0("param_space_plot_",i),"auto","auto", click=paste0("ps_",i,"_click"), dblclick=paste0("ps_",i,"_dblclick"),
                                       hover=hoverOpts(id=paste0("ps_",i,"_hover"),delayType="debounce",delay=hover_delay_limit),
                                       brush=brushOpts(id=paste0("ps_",i,"_brush"),delayType="debounce",delay=brush_delay_limit,resetOnNew=T))
                       )
                ),
                column(4,
                       helpText(Result_SS_label),
                       if(!is.null(input[[paste0("param_ss_selector_x_",i)]]) && input[[paste0("param_ss_selector_x_",i)]] != empty_sign &&
                              !is.null(input[[paste0("param_ss_selector_y_",i)]]) && input[[paste0("param_ss_selector_y_",i)]] != empty_sign) {
                           div(id = "plot-container",
                               tags$img(
                                   # src = "circle.png",
                                   src = "spinner.gif",
                                   id = "loading-spinner"),
                               imageOutput(paste0("param_ss_plot_",i),"auto","auto", click=paste0("param_ss_",i,"_click"), dblclick=paste0("param_ss_",i,"_dblclick"),
                                           hover=hoverOpts(id=paste0("param_ss_",i,"_hover"),delayType="debounce",delay=hover_delay_limit),
                                           brush=brushOpts(id=paste0("param_ss_",i,"_brush"),delayType="debounce",delay=brush_delay_limit,resetOnNew=T))
                           )
                       }
                ),
                column(2,
                       #actionButton(paste0("apply_plot_param_ss_",i),"Apply to all"),
                       tags$div(title=Result_SS_DeselectAll_tooltip,
                                actionButton(paste0("clear_plot_param_ss_",i),Result_SS_DeselectAll_label)),
                       tags$div(title=Result_SS_Unzoom_tooltip,
                                actionButton(paste0("unzoom_plot_param_ss_",i),Result_SS_Unzoom_label)),
                       fluidRow(
                           column(6,
                                  tags$div(title=Result_SS_horizontal_tooltip,
                                           selectInput(paste0("param_ss_selector_x_",i),Result_SS_horizontal_label,list_of_var_names,
                                               ifelse(!is.null(input[[paste0("param_ss_selector_x_",i)]]),input[[paste0("param_ss_selector_x_",i)]], list_of_var_names[[1]])))
                           ),
                           column(6,
                                  tags$div(title=Result_SS_vertical_tooltip,
                                           selectInput(paste0("param_ss_selector_y_",i),Result_SS_vertical_label,list_of_var_names,
                                               ifelse(!is.null(input[[paste0("param_ss_selector_y_",i)]]),input[[paste0("param_ss_selector_y_",i)]], #empty_sign))
                                                      ifelse(length(loading_ps_file()$var_names) > 1, list_of_var_names[[2]], list_of_var_names[[1]]))))
                           )
                       ),
                       if(!is.null(input[[paste0("param_ss_selector_x_",i)]]) && input[[paste0("param_ss_selector_x_",i)]] != empty_sign &&
                                !is.null(input[[paste0("param_ss_selector_y_",i)]]) && input[[paste0("param_ss_selector_y_",i)]] != empty_sign) {
                           lapply(1:length(loading_ps_file()$var_names), function(t) {
                               if(!loading_ps_file()$var_names[[t]] %in% c(input[[paste0("param_ss_selector_x_",i)]],input[[paste0("param_ss_selector_y_",i)]])) {
                                   label <- paste0(Result_SS_ScaleSlider_label,loading_ps_file()$var_names[[t]])
                                   name <- paste0("scale_slider_param_ss_",i,"_",t)
                                   #values <- c(1,length(loading_ps_file()$thresholds[[t]])-1)
                                   values <- range(loading_ps_file()$thresholds[[t]])
                                   tags$div(title=Result_SS_ScaleSlider_tooltip,
                                            sliderInput(name,label=label,min=values[1],max=values[2],step=0.001, #,step=1,
                                                value=ifelse(is.null(input[[paste0("scale_slider_param_ss_",i,"_",t)]]),values[1],
                                                             input[[paste0("scale_slider_param_ss_",i,"_",t)]])))
                               }
                           })
                       },
                       tags$div(title=Result_SS_HoverTextArea_tooltip,
                                verbatimTextOutput(paste0("hover_text_param_ss_",i)))
                )
            )
            #}
        })
        do.call(tagList, one_line)
    }
})

draw_param_ss_plots <- observe({
    for (i in visible_ps_plots()) {
        local({
            my_i <- i
            if(!is.null(input[[paste0("param_ss_selector_x_",my_i)]]) && input[[paste0("param_ss_selector_x_",my_i)]] != empty_sign &&
                   !is.null(input[[paste0("param_ss_selector_y_",my_i)]]) && input[[paste0("param_ss_selector_y_",my_i)]] != empty_sign) {
                plotname <- paste0("param_ss_plot_",my_i)
                output[[plotname]] <- renderPlot({
                    draw_param_ss_crossroad(isolate(input[[paste0("param_ss_selector_x_",my_i)]]), isolate(input[[paste0("param_ss_selector_y_",my_i)]]), my_i, 
                                            param_ss_brushed$data[[my_i]])
                },height=change_height())
            }
        })
    }
})
hover_over_param_ss_plots <- observe({
    for (i in visible_ps_plots()) {
        local({
            my_i <- i
            hover <- input[[paste0("param_ss_",my_i,"_hover")]]
            list_of_all_names <- loading_ps_file()$var_names
            output[[paste0("hover_text_param_ss_",my_i)]] <- renderPrint({
                cat(paste0(
                    sapply(1:length(list_of_all_names), function(x) {
                        name <- list_of_all_names[[x]]
                        if(!name %in% c(input[[paste0("param_ss_selector_x_",my_i)]],input[[paste0("param_ss_selector_y_",my_i)]])) {
                            ifelse(is.null(input[[paste0("scale_slider_param_ss_",my_i,"_",x)]]), paste0(name,": "),
                                   paste0(name,": ",round(input[[paste0("scale_slider_param_ss_",my_i,"_",x)]],rounding_in_hover)) )
                        } else {
                            ifelse(is.null(hover), paste0(name,": "),
                                   ifelse(name %in% input[[paste0("param_ss_selector_x_",my_i)]], paste0(name,": ",round(hover$x,rounding_in_hover)),
                                          paste0(name,": ",round(hover$y,rounding_in_hover)) ))
                        }
                    }), collapse="\n"))
            },width="100")
        })
    }
})

draw_param_plots <- observe({
    for(i in visible_ps_plots()) {
        local({
            my_i <- i
            plotname <- paste0("param_space_plot_",my_i)
            output[[plotname]] <- renderPlot({
                draw_param_space_crossroad(isolate(input[[paste0("param_selector_x_",my_i)]]), isolate(input[[paste0("param_selector_y_",my_i)]]), my_i, ps_brushed$data[[my_i]])
            },height=change_height() # input$height
            )
        })
    }
})
hover_over_ps_plots <- observe({
    for (i in visible_ps_plots()) {
        local({
            my_i <- i
            hover <- input[[paste0("ps_",my_i,"_hover")]]
            if(input[[paste0("param_selector_x_",my_i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_y_",my_i)]] %in% loading_ps_file()$var_names)
                list_of_all_names <- c(loading_ps_file()$param_names, loading_ps_file()$var_names)
            else
                list_of_all_names <- loading_ps_file()$param_names
            output[[paste0("hover_text_ps_",my_i)]] <- renderPrint({
                cat(paste0(
                    sapply(1:length(list_of_all_names), function(x) {
                        name <- list_of_all_names[[x]]
                        if(!name %in% c(input[[paste0("param_selector_x_",my_i)]],input[[paste0("param_selector_y_",my_i)]])) {
                            # TODO: reformat
                            if(name %in% loading_ps_file()$var_names) {
                                if(!is.null(input[[paste0("scale_switch_ps_",my_i,"_",x)]]) && input[[paste0("scale_switch_ps_",my_i,"_",x)]] )
                                    ifelse(is.null(input[[paste0("scale_slider_ps_",my_i,"_",x)]]), paste0(name,": "),
                                           paste0(name,": ",round(input[[paste0("scale_slider_ps_",my_i,"_",x)]],rounding_in_hover)) )
                                else
                                    paste0(name,": ",round(range(loading_ps_file()$thresholds[[name]])[1],rounding_in_hover)," - ",
                                           round(range(loading_ps_file()$thresholds[[name]])[2],rounding_in_hover) )
                            } else {
                                if(!is.null(input[[paste0("scale_switch_ps_",my_i,"_",x)]]) && input[[paste0("scale_switch_ps_",my_i,"_",x)]] )
                                    ifelse(is.null(input[[paste0("scale_slider_ps_",my_i,"_",x)]]), paste0(name,": "),
                                           paste0(name,": ",round(input[[paste0("scale_slider_ps_",my_i,"_",x)]],rounding_in_hover)) )
                                else
                                    paste0(name,": ",round(param_ranges()[[name]][1],rounding_in_hover)," - ",
                                           round(param_ranges()[[name]][2],rounding_in_hover) )
                            }
                        } else {
                            ifelse(is.null(hover), paste0(name,": "),
                                   ifelse(name %in% input[[paste0("param_selector_x_",my_i)]], paste0(name,": ",round(hover$x,rounding_in_hover)),
                                          paste0(name,": ",round(hover$y,rounding_in_hover)) ))
                        }
                    }), collapse="\n"))
            },width="100")
        })
    }
})

reset_advanced_settings <- observeEvent(input$advanced,{
    if(!is.null(loading_ps_file()) && !input$advanced) {
        variables <- loading_ps_file()$var_names
        params    <- loading_ps_file()$param_names
        list_of_all_names <- c(params, variables)
        for(i in visible_ps_plots()) {
            # if(!input[[paste0("param_selector_x_",i)]] %in% variables && !input[[paste0("param_selector_y_",i)]] %in% variables) {
                for(t in 1:length(list_of_all_names)) {
                    if(list_of_all_names[[t]] %in% variables)
                        updateCheckboxInput(session,paste0("scale_switch_ps_",i,"_",t),value = F)
                    else
                        updateCheckboxInput(session,paste0("scale_switch_ps_",i,"_",t),value = T)
                }
        }
    }
})


draw_param_ss_crossroad <- function(name_x, name_y, plot_index, boundaries) {
    variables <- loading_ps_file()$var_names
    if(name_x == name_y) return(draw_1D_param_ss(name_x, plot_index, boundaries))
    else                 return(draw_param_ss(name_x, name_y, plot_index, boundaries))
}
draw_param_ss <- function(name_x, name_y, plot_index, boundaries) {
    variables <- loading_ps_file()$var_names
    params <- loading_ps_file()$param_names
    index_x <- match(name_x,variables)
    index_y <- match(name_y,variables)
    thres <- loading_ps_file()$thresholds
    
    if(!is.null(loading_ps_file())) {
        # create current set of globals
        checkpoint <- list(selectors=list(x=input[[paste0("param_ss_selector_x_",plot_index)]], y=input[[paste0("param_ss_selector_y_",plot_index)]]),
                           formula=chosen_ps_formulae_clean(),
                           # counter=input$process_run,
                           sliders=list() )
        for(t in 1:length(variables)) {
            if(!variables[[t]] %in% c(input[[paste0("param_ss_selector_x_",plot_index)]],input[[paste0("param_ss_selector_y_",plot_index)]] )) {
                checkpoint$sliders[[variables[[t]] ]] <- input[[paste0("scale_slider_param_ss_",plot_index,"_",t)]]
            }
        }
        # check for any change in globals for particular plot
        if(is.na(param_state_space$globals[[plot_index]]) || !identical(param_state_space$globals[[plot_index]],checkpoint) ||
               !identical(param_ss_clicked$point[[plot_index]],param_ss_clicked$old_point[[plot_index]])) {
            
            states <- copy(satisfiable_states())
            ids <- states$id    # all ids at first
            for(x in 1:length(variables)) {
                if(!x %in% c(index_x,index_y)) {
                    sid <- input[[paste0("scale_slider_param_ss_",plot_index,"_",x)]] # right state value in dimension x
                    ids <- intersect(ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, id])
                }
            }        # incremental intersection of ids in order to get right ids
            states <- states[id %in% ids]
            # states[, color:=first_formula_color]   # corresponding rectangles based on selected parameter point could have blue border
            states[, color:=ifelse(id %in% param_ss_clicked$point[[plot_index]],first_formula_color_clicked, first_formula_color)]         # selected rectangles could be in darkgreen
            param_state_space$data[[plot_index]] <- states
        }
        plot(range(thres[[index_x]]), range(thres[[index_y]]),
             type="n", xlab=name_x, ylab=name_y, xaxs="i", yaxs="i", xlim=boundaries[[index_x]], ylim=boundaries[[index_y]])
        abline(v=thres[[index_x]],h=thres[[index_y]])
        states <- param_state_space$data[[plot_index]]
        rect(states[[paste0("V",index_x*2-1)]], states[[paste0("V",index_y*2-1)]], states[[paste0("V",index_x*2)]], states[[paste0("V",index_y*2)]],
            border=states$border, col=states$color, lwd=1.5)
        
        ##======= reaction on click inside a PS plot =========================
        if(length(param_space_clicked$point) >= plot_index && !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]])) ) {
            
            point <- param_space_clicked$point[[plot_index]]
            ps <- copy(satisfiable_param_space_for_formula())
            
            if(loading_ps_file()$type == "smt") {
                # for symbolic parameters
                ps <- unique(ps)
                setkey(ps,id)
                num <- input$density_coeficient
                dim_indices <- letters[-which(letters %in% c("a"))]
                
                dt <- data.table(a=point[[1]])
                input_params <- paste0("list(",names(point)[1],"=dt$a")
                for(x in 1:length(params)) {
                    name <- params[x]
                    di <- dim_indices[x]
                    if(name != names(point)[1]) {
                        if(!name %in% names(point)) {
                            if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]] ) {
                                dt[ ,V1:=input[[paste0("scale_slider_ps_",plot_index,"_",x)]] ]
                            } else {
                                data <- seq(param_ranges()[[name]][1],param_ranges()[[name]][2],length.out = num)
                                data <- sapply(2:length(data), function(i) (data[i-1]+data[i])*0.5 )
                                dt <- as.data.table(merge.default(dt, data.table(V1=data) ))
                            }
                        } else {
                            # only possible case is that 'name' equals point[[2]] 
                            dt[ ,V1:=point[[name]] ]
                        }
                        setnames( dt, "V1", di)
                        input_params <- paste(input_params,paste0(name,"=dt$",di),sep = ",")
                    }
                }
                dt[,id:=1:nrow(dt)]
                input_params <- paste0(input_params,")")
                
                for(ex in ps$id) ps[id==ex,blue:=(T %in% eval(parse(text=expr))(eval(parse(text=input_params)))) ]
                ids <- unique(ps[blue==T,id])
                
            } else {
                # for rectangular parameters
                ids <- ps$row_id    # all ids at first
                for(x in 1:length(params)) {
                    name <- params[x]
                    if(!name %in% c(input[[paste0("param_selector_x_",plot_index)]],input[[paste0("param_selector_y_",plot_index)]]) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) {
                            sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                            ids <- intersect(ids, ps[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) > sid, row_id])
                        }
                    } else {
                        ids <- intersect(ids, ps[get(paste0("V",x*2-1)) <= point[[name]] & get(paste0("V",x*2)) > point[[name]], row_id])
                    }
                }        # incremental intersection of ids in order to get right ids
                ids <- unique(ps[row_id %in% ids, id])
            }
            
            blue_ids <- loading_ps_file()$param_space[(param+1) %in% ids & formula==chosen_ps_formulae_clean(),state+1]
            if(input[[paste0("param_selector_x_",plot_index)]] %in% variables || input[[paste0("param_selector_y_",plot_index)]] %in% variables) {
                for(x in 1:length(variables)) {
                    name <- variables[x]
                    if(!name %in% c(input[[paste0("param_selector_x_",plot_index)]],input[[paste0("param_selector_y_",plot_index)]]) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x+length(params))]]) && 
                           input[[paste0("scale_switch_ps_",plot_index,"_",x+length(params))]]) {
                            sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(params))]] # right state value in dimension x
                            blue_ids <- intersect(blue_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) > sid, id])
                        }
                    # } else {
                    #     blue_ids <- intersect(blue_ids, states[get(paste0("V",x*2-1)) <= point[[name]] & get(paste0("V",x*2)) > point[[name]], id])
                    }
                }        # incremental intersection of ids in order to get right ids
            }
            states <- states[id %in% blue_ids]
            
            if(nrow(states) > 0) {
                rect(states[[paste0("V",index_x*2-1)]], states[[paste0("V",index_y*2-1)]], states[[paste0("V",index_x*2)]], states[[paste0("V",index_y*2)]],
                     border="blue", col=NA, lwd=2)
            }
        }
        # this has to be at the end
        param_state_space$globals[[plot_index]] <- checkpoint
        param_ss_clicked$old_point[[plot_index]] <- param_ss_clicked$point[[plot_index]]
    }
}
draw_1D_param_ss <- function(name_x, plot_index, boundaries) {
    variables <- loading_ps_file()$var_names
    params <- loading_ps_file()$param_names
    index_x <- match(name_x, variables)
    
    if(!is.null(loading_ps_file())) {
        # create current set of globals
        checkpoint <- list(selectors=list(x=input[[paste0("param_ss_selector_x_",plot_index)]], y=input[[paste0("param_ss_selector_y_",plot_index)]]),
                           formula=chosen_ps_formulae_clean(),
                           # counter=input$process_run,
                           sliders=list() )
        for(t in 1:length(variables)) {
            if(!variables[[t]] %in% c(input[[paste0("param_ss_selector_x_",plot_index)]],input[[paste0("param_ss_selector_y_",plot_index)]] )) {
                checkpoint$sliders[[variables[[t]] ]] <- input[[paste0("scale_slider_param_ss_",plot_index,"_",t)]]
            }
        }
        # check for any change in globals for particular plot
        if(is.na(param_state_space$globals[[plot_index]]) || !identical(param_state_space$globals[[plot_index]],checkpoint) ||
           !identical(param_ss_clicked$point[[plot_index]],param_ss_clicked$old_point[[plot_index]])) {
            
            states <- copy(satisfiable_states())
            ids <- states$id    # all ids at first
            for(x in 1:length(variables)) {
                if(!x %in% c(index_x)) {
                    sid <- input[[paste0("scale_slider_param_ss_",plot_index,"_",x)]] # right state value in dimension x
                    ids <- intersect(ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, id])
                }
            }        # incremental intersection of ids in order to get right ids
            states <- states[id %in% ids]
            # states[, color:=first_formula_color]   # corresponding rectangles based on selected parameter point could have blue border
            states[, color:=ifelse(id %in% param_ss_clicked$point[[plot_index]],first_formula_color_clicked, first_formula_color)]         # selected rectangles could be in darkgreen
            param_state_space$data[[plot_index]] <- states
        }
        states <- param_state_space$data[[plot_index]]
        plot(range(loading_ps_file()$thresholds[[index_x]]), c(0,1), type="n", xlab=name_x, ylab="", yaxt="n", xlim=boundaries[[index_x]], xaxs="i", yaxs="i")
        abline(v=loading_ps_file()$thresholds[[index_x]])
        if(nrow(states) > 0) {
            rect(states[[paste0("V",index_x*2-1)]], 0, states[[paste0("V",index_x*2)]], 1,
                 border=states$border, col=states$color, lwd=1.5)
        }
        ##======= reaction on click inside a PS plot =========================
        if(length(param_space_clicked$point) >= plot_index && !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]])) ) {
            
            point <- param_space_clicked$point[[plot_index]]
            ps <- copy(satisfiable_param_space_for_formula())
            
            if(loading_ps_file()$type == "smt") {
                # for symbolic parameters
                ps <- unique(ps)
                setkey(ps,id)
                num <- input$density_coeficient
                dim_indices <- letters[-which(letters %in% c("a"))]
                
                dt <- data.table(a=point[[1]])
                input_params <- paste0("list(",names(point)[1],"=dt$a")
                for(x in 1:length(params)) {
                    name <- params[[x]]
                    di <- dim_indices[x]
                    if(name != names(point)[1]) {
                        if(!name %in% names(point)) {
                            if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]] ) {
                                dt[ ,V1:=input[[paste0("scale_slider_ps_",plot_index,"_",x)]] ]
                            } else {
                                data <- seq(param_ranges()[[name]][1],param_ranges()[[name]][2],length.out = num)
                                data <- sapply(2:length(data), function(i) (data[i-1]+data[i])*0.5 )
                                dt <- as.data.table(merge.default(dt, data.table(V1=data) ))
                            }
                        } else {
                            # only possible case is that 'name' equals point[[2]] 
                            dt[ ,V1:=point[[name]] ]
                        }
                        setnames( dt, "V1", di)
                        input_params <- paste(input_params,paste0(name,"=dt$",di),sep = ",")
                    }
                }
                dt[,id:=1:nrow(dt)]
                input_params <- paste0(input_params,")")
                
                for(ex in ps$id) ps[id==ex,blue:=(T %in% eval(parse(text=expr))(eval(parse(text=input_params)))) ]
                ids <- unique(ps[blue==T,id])
                
            } else {
                # for rectangular parameters
                ids <- ps$row_id    # all ids at first
                for(x in 1:length(params)) {
                    name <- params[[x]]
                    if(!name %in% c(input[[paste0("param_selector_x_",plot_index)]],input[[paste0("param_selector_y_",plot_index)]]) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) {
                            sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                            ids <- intersect(ids, ps[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) > sid, row_id])
                        }
                    } else {
                        ids <- intersect(ids, ps[get(paste0("V",x*2-1)) <= point[[name]] & get(paste0("V",x*2)) > point[[name]], row_id])
                    }
                }        # incremental intersection of ids in order to get right ids
                ids <- unique(ps[row_id %in% ids, id])
            }
            
            blue_ids <- loading_ps_file()$param_space[(param+1) %in% ids & formula==chosen_ps_formulae_clean(),state+1]
            if(input[[paste0("param_selector_x_",plot_index)]] %in% variables || input[[paste0("param_selector_y_",plot_index)]] %in% variables) {
                for(x in 1:length(variables)) {
                    name <- variables[[x]]
                    if(!name %in% c(input[[paste0("param_selector_x_",plot_index)]],input[[paste0("param_selector_y_",plot_index)]]) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x+length(params))]]) && 
                           input[[paste0("scale_switch_ps_",plot_index,"_",x+length(params))]]) {
                            sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(params))]] # right state value in dimension x
                            blue_ids <- intersect(blue_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) > sid, id])
                        }
                        # } else {
                        #     blue_ids <- intersect(blue_ids, states[get(paste0("V",x*2-1)) <= point[[name]] & get(paste0("V",x*2)) > point[[name]], id])
                    }
                }        # incremental intersection of ids in order to get right ids
            }
            states <- states[id %in% blue_ids]
            
            if(nrow(states) > 0) {
                rect(states[[paste0("V",index_x*2-1)]], 0, states[[paste0("V",index_x*2)]], 1,
                     border="blue", col=NA, lwd=2)
            }
        }
        # this has to be at the end
        param_state_space$globals[[plot_index]] <- checkpoint
        param_ss_clicked$old_point[[plot_index]] <- param_ss_clicked$point[[plot_index]]
    }
}


draw_param_space_crossroad <- function(name_x, name_y, plot_index, boundaries) {
    variables <- loading_ps_file()$var_names
    if(name_x == name_y) return(draw_1D_param_space(name_x, plot_index, boundaries))
    else {
        if(name_x %in% variables || name_y %in% variables) return(draw_param_space_mixed(name_x, name_y, plot_index, boundaries))
        else return(draw_param_space(name_x, name_y, plot_index, boundaries))
    }
}
draw_param_space_mixed <- function(name_x, name_y, plot_index, boundaries) {
    variables <- loading_ps_file()$var_names
    params    <- loading_ps_file()$param_names
    list_of_all_names <- c(params, variables)
    if(name_x %in% variables) {
        index_x <- match(name_x,variables)
        index_y <- match(name_y,params)
        full_range_x <- range(loading_ps_file()$thresholds[[name_x]])
        full_range_y <- param_ranges()[[name_y]]
        if(!is.null(boundaries[[name_x]]))  range_x <- boundaries[[name_x]]
        else                                range_x <- range(loading_ps_file()$thresholds[[name_x]])
        if(!is.null(boundaries[[name_y]]))  range_y <- boundaries[[name_y]]
        else                                range_y <- param_ranges()[[name_y]]
        it_is_x <- T
    } else {
        index_x <- match(name_x,params)
        index_y <- match(name_y,variables)
        full_range_x <- param_ranges()[[name_x]]
        full_range_y <- range(loading_ps_file()$thresholds[[name_y]])
        if(!is.null(boundaries[[name_x]]))  range_x <- boundaries[[name_x]]
        else                                range_x <- param_ranges()[[name_x]]
        if(!is.null(boundaries[[name_y]]))  range_y <- boundaries[[name_y]]
        else                                range_y <- range(loading_ps_file()$thresholds[[name_y]])
        it_is_x <- F
    }
    ##============ drawing of 2D parameter space ===============================
    if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$param_space) != 0) {
        
        # create current set of globals
        checkpoint <- list(selectors=list(x=input[[paste0("param_selector_x_",plot_index)]], y=input[[paste0("param_selector_y_",plot_index)]]),
                           boundaries=boundaries,
                           density=input$density_coeficient,
                           coverage=input$coverage_check,
                           formula=chosen_ps_formulae_clean(),
                           param_ss_clicked_point=param_ss_clicked$point[[plot_index]],
                           # counter=input$process_run,
                           sliders_checkbox=list(),
                           sliders=list() )
        for(x in 1:length(list_of_all_names)) {
            name <- list_of_all_names[[x]]
            if(!name %in% c(name_x,name_y)) {
                # if(list_of_all_names[[x]] %in% variables) {
                checkpoint$sliders_checkbox[[name]] <- input[[paste0("scale_switch_ps_",plot_index,"_",x)]]
                checkpoint$sliders[[name]] <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]]
                # } else {
                #     checkpoint$sliders_checkbox[[params[[x]] ]] <- input[[paste0("scale_switch_ps_",plot_index,"_",x)]]
                #     checkpoint$sliders[[params[[x]] ]] <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]]
                # }
            }
        }
        # check for any change in globals for particular plot
        if(is.na(param_space$globals[[plot_index]]) || !identical(param_space$globals[[plot_index]],checkpoint) ) {
            
            if(loading_ps_file()$type == "smt") {
                # symbolic type of parameters
                if(length(param_ss_clicked$point[[plot_index]]) == 0) {
                    ps <- copy(satisfiable_param_space_for_formula())
                } else {
                    ps <- loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & 
                                                            (state+1) %in% param_ss_clicked$point[[plot_index]],.(param=param+1,cov)]
                    ps <- merge(loading_ps_file()$params, ps, by.x="id", by.y="param")
                }
                ps <- unique(ps)
                setkey(ps,id)
                num <- input$density_coeficient
                dim_indices <- letters[-which(letters %in% c("x","y"))]
                if(!it_is_x) {
                    input_params <- paste0("list(",name_x,"=x")
                    
                    thr <- loading_ps_file()$thresholds[[name_y]]
                    thr <- sort(unique(c(range_y, thr[which(thr > range_y[1] & thr < range_y[2])])))
                    nesh <- meshgrid(seq(range_x[1],range_x[2],length.out = num), thr)
                    
                    dt <- data.table(x1=unlist(as.list(nesh$X[1:(length(thr)-1),1:(num-1)])),x2=unlist(as.list(nesh$X[2:length(thr),2:num])),
                                     y1=unlist(as.list(nesh$Y[1:(length(thr)-1),1:(num-1)])),y2=unlist(as.list(nesh$Y[2:length(thr),2:num])))
                } else {
                    input_params <- paste0("list(",name_y,"=y")
                    
                    thr <- loading_ps_file()$thresholds[[name_x]]
                    thr <- sort(unique(c(range_x, thr[which(thr > range_x[1] & thr < range_x[2])])))
                    nesh <- meshgrid(thr, seq(range_y[1],range_y[2],length.out = num))
                    
                    dt <- data.table(x1=unlist(as.list(nesh$X[1:(num-1),1:(length(thr)-1)])),x2=unlist(as.list(nesh$X[2:num,2:length(thr)])),
                                     y1=unlist(as.list(nesh$Y[1:(num-1),1:(length(thr)-1)])),y2=unlist(as.list(nesh$Y[2:num,2:length(thr)])))
                }
                dt[,x:=x1+(x2-x1)*0.5]
                dt[,y:=y1+(y2-y1)*0.5]
                
                for(p in 1:length(params)) {
                    name <- params[p]
                    if(!name %in% c(name_x,name_y)) {
                        data <- seq(param_ranges()[[name]][1],param_ranges()[[name]][2],length.out = num)
                        dt <- as.data.table(merge.default(dt, data.table(V1=data[1:(num-1)], V2=data[2:num]) ))
                        dt[, V3 := V1+(V2-V1)*0.5 ]
                        di <- dim_indices[p]
                        setnames( dt, c("V1","V2","V3"), c(paste0(di,1),paste0(di,2),di) )
                        input_params <- paste(input_params,paste0(name,"=",di),sep = ",")
                    }
                }
                dt[,cov:=0]
                dt[,id:=1:nrow(dt)]
                input_params <- paste0(input_params,")")
                
                #### Layers !!!!!!!!!
                ids <- dt$id    # all ids at first
                states <- copy(satisfiable_states())
                if(it_is_x) setnames(states,c(paste0("V",index_x*2-1),paste0("V",index_x*2)),c("x1","x2"))
                else        setnames(states,c(paste0("V",index_y*2-1),paste0("V",index_y*2)),c("y1","y2"))
                if(length(param_ss_clicked$point[[plot_index]]) == 0)    st_ids <- states$id     # all ids for states at first
                else                                                     st_ids <- param_ss_clicked$point[[plot_index]]
                for(x in 1:length(list_of_all_names)) {
                    name <- list_of_all_names[[x]]
                    if(!name %in% c(name_x,name_y) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]] ) {
                            if(name %in% variables) {
                                x <- x-length(params)
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(params))]] # right state value in dimension x
                                st_ids <- intersect(st_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, id])
                            } else {
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                                di <- dim_indices[x]
                                ids    <- intersect(ids, dt[get(paste0(di,1)) <= sid & get(paste0(di,2)) >= sid, id])
                            }
                        }
                    }
                }
                ps <- merge(merge(loading_ps_file()$param_space[(state+1) %in% st_ids & formula == chosen_ps_formulae_clean(), .(state=state+1,param=param+1)], 
                                  ps,by.x="param",by.y="id",allow.cartesian=T), states[id %in% st_ids],by.x="state",by.y="id")
                # ps <- ps[id %in% unique(loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & (state+1) %in% st_ids, param+1]) ]
                dt <- dt[id %in% ids ]
                
                if(it_is_x) time <- system.time(for(ex in ps$param) dt[x1 %in% ps[param==ex,x1] & x2 %in% ps[param==ex,x2], 
                                                                       cov:=cov+ifelse(eval(parse(text=ps[param==ex,expr]))(eval(parse(text=input_params))),1,0)])
                else        time <- system.time(for(ex in ps$param) dt[y1 %in% ps[param==ex,y1] & y2 %in% ps[param==ex,y2], 
                                                                       cov:=cov+ifelse(eval(parse(text=ps[param==ex,expr]))(eval(parse(text=input_params))),1,0)])
                print(time)
                
                dt <- dt[cov>0,.(cov=max(.SD$cov)),by=.(x1,x2,y1,y2)]
                if(input$coverage_check) {
                    param_space$data[[plot_index]] <- dt[cov > 0]
                } else {
                    dt[cov > 0, cov:=1]
                    param_space$data[[plot_index]] <- dt[cov > 0]
                }
                
            } else {
                # rectangular type of parameters
                if(length(param_ss_clicked$point[[plot_index]]) == 0) {
                    ps <- copy(satisfiable_param_space_for_formula())
                } else {
                    ps <- loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & 
                                                            (state+1) %in% param_ss_clicked$point[[plot_index]],.(param=param+1,cov)]
                    ps <- merge(loading_ps_file()$params, ps, by.x="id", by.y="param")
                }
                if(it_is_x) setnames(ps,c(paste0("V",index_y*2-1),paste0("V",index_y*2)),c("y1","y2"))
                else        setnames(ps,c(paste0("V",index_x*2-1),paste0("V",index_x*2)),c("x1","x2"))
                ps[, cov:=1]
                
                #### Layers !!!!!!!!!
                ids <- ps$row_id            # all ids for params at first
                states <- copy(satisfiable_states())
                if(it_is_x) setnames(states,c(paste0("V",index_x*2-1),paste0("V",index_x*2)),c("x1","x2"))
                else        setnames(states,c(paste0("V",index_y*2-1),paste0("V",index_y*2)),c("y1","y2"))
                if(length(param_ss_clicked$point[[plot_index]]) == 0)    st_ids <- states$id     # all ids for states at first
                else                                                     st_ids <- param_ss_clicked$point[[plot_index]]
                for(x in 1:length(list_of_all_names)) {
                    name <- list_of_all_names[[x]]
                    if(!name %in% c(name_x,name_y) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]] ) {
                            if(name %in% variables) {
                                x <- x-length(params)
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(params))]] # right state value in dimension x
                                st_ids <- intersect(st_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, id])
                            } else {
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                                ids <- intersect(ids, ps[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, row_id])
                            }
                        }
                        # if(name %in% params && length(param_space_clicked$point) >= plot_index && 
                        #    !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]]))) {
                        #     if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) {
                        #         param_space_clicked$point[[plot_index]][[params[[x]] ]] <- c(input[[paste0("scale_slider_ps_",plot_index,"_",x)]],
                        #                                                                      input[[paste0("scale_slider_ps_",plot_index,"_",x)]])
                        #     } else {
                        #         param_space_clicked$point[[plot_index]][[params[[x]] ]] <- c(param_ranges()[[params[[x]] ]])
                        #     }
                        # }
                    } else {
                        if(name == name_x) {
                            if(it_is_x) st_ids <- intersect(st_ids, states[x1 < range_x[2] & x2 >= range_x[2] |
                                                                           x1 <= range_x[1] & x2 > range_x[1] |
                                                                           x1 >= range_x[1] & x2 <= range_x[2], id])
                            else        ids <- intersect(ids, ps[x1 < range_x[2] & x2 >= range_x[2] |
                                                                 x1 <= range_x[1] & x2 > range_x[1] |
                                                                 x1 >= range_x[1] & x2 <= range_x[2], row_id])
                        } else {
                            if(it_is_x) ids <- intersect(ids, ps[y1 < range_y[2] & y2 >= range_y[2] |
                                                                 y1 <= range_y[1] & y2 > range_y[1] |
                                                                 y1 >= range_y[1] & y2 <= range_y[2], row_id])
                            else        st_ids <- intersect(st_ids, states[y1 < range_y[2] & y2 >= range_y[2] |
                                                                           y1 <= range_y[1] & y2 > range_y[1] |
                                                                           y1 >= range_y[1] & y2 <= range_y[2], id])
                        }
                    }
                }        # incremental intersection of ids in order to get right ids
                ps <- merge(merge(loading_ps_file()$param_space[(state+1) %in% st_ids & formula == chosen_ps_formulae_clean(), .(state=state+1,param=param+1)], 
                                  ps[row_id %in% ids],by.x="param",by.y="id",allow.cartesian=T), states[id %in% st_ids],by.x="state",by.y="id")
                # param_space_clicked$data[[plot_index]] <- copy(ps[row_id %in% ids])
                
                if(input$coverage_check && nrow(ps) != 0) {
                    num <- input$density_coeficient
                    if(it_is_x) {
                        thr <- loading_ps_file()$thresholds[[name_x]]
                        thr <- thr[which(thr > range_x[1] & thr < range_x[2])]
                        nesh <- meshgrid(sort(c(seq(range_x[1],range_x[2],length.out = num-length(thr)),thr)),
                                         seq(range_y[1],range_y[2],length.out = num))
                    } else {
                        thr <- loading_ps_file()$thresholds[[name_y]]
                        thr <- thr[which(thr > range_y[1] & thr < range_y[2])]
                        nesh <- meshgrid(seq(range_x[1],range_x[2],length.out = num),
                                         sort(c(seq(range_y[1],range_y[2],length.out = num-length(thr)),thr)))
                    }
                    dt <- data.table(x1=unlist(as.list(nesh$X[1:(num-1),1:(num-1)])),x2=unlist(as.list(nesh$X[2:num,2:num])),
                                     y1=unlist(as.list(nesh$Y[1:(num-1),1:(num-1)])),y2=unlist(as.list(nesh$Y[2:num,2:num])))
                    dt[,x:=x1+(x2-x1)*0.5]
                    dt[,y:=y1+(y2-y1)*0.5]
                    
                    uniq_x <- unique(ps[,.(x1,x2)])
                    uniq_y <- unique(ps[,.(y1,y2)])
                    if(nrow(dt) != 0) {
                        # timing <- system.time({
                        #     rang_x <- range(uniq_x)
                        #     rang_y <- range(uniq_y)
                        #     dt <- dt[x <= rang_x[2] & x >= rang_x[1] & y <= rang_y[2] & y >= rang_y[1] ]
                        #     if(nrow(uniq_x) < nrow(uniq_y)) {    # merge over the axis which has less unique intervals: (x1,x2) or (y1,y2)
                        #         setkey(ps,x1,x2)
                        #         one <- foverlaps(dt[,.(x=x,y=y,xe=x,ye=y)],ps, by.x=c("x","xe"),type="within")[y1<=y & y2>=y,.(cov=length(unique(param))),by=.(x,y)]
                        #     } else {
                        #         setkey(ps,y1,y2)
                        #         one <- foverlaps(dt[,.(x=x,y=y,xe=x,ye=y)],ps, by.x=c("y","ye"),type="within")[x1<=x & x2>=x,.(cov=length(unique(param))),by=.(x,y)]
                        #     }
                        #     dt <- merge(dt, one, by.x=c("x","y"), by.y=c("x","y"))
                        #     rm(one)
                        # })
                        # print(timing)
                        timing <- system.time({
                            rang_x <- range(uniq_x)
                            rang_y <- range(uniq_y)
                            dt <- dt[x <= rang_x[2] & x >= rang_x[1] & y <= rang_y[2] & y >= rang_y[1] ]
                            dt <- dt[ps,.(x1=x.x1,x2=x.x2,y1=x.y1,y2=x.y2,id=i.param),on=.(x>=x1,x<=x2,y>=y1,y<=y2),allow.cartesian=T,nomatch=0][,.(cov=length(unique(id))),by=.(x1,x2,y1,y2)]
                        })
                        print(timing)
                        print(paste0("uniq cov: ",paste0(unique(dt$cov),collapse = ", ")))
                    } else print(paste0("dt is empty"))
                    
                    param_space$data[[plot_index]] <- dt
                } else {
                    param_space$data[[plot_index]] <- ps
                }
            }
        }
        plot(full_range_x, full_range_y, type="n", xlab=name_x, ylab=name_y, xaxs="i", yaxs="i",
                 xlim=range_x, ylim=range_y)
        if(it_is_x) abline(v=loading_ps_file()$thresholds[[name_x]])
        else        abline(h=loading_ps_file()$thresholds[[name_y]])
        
        ps <- param_space$data[[plot_index]]
        if(nrow(ps) > 0) {
            range_cov <- range(ps$cov)
            ps[,rect(x1, y1, x2, y2, col=rgb(0,0.5,0,alpha = (cov/range_cov[2])*ifelse(input$coverage_check, grey_shade(), 1)), border=NA)]
        }
        ##======= draw point due to click inside a plot =========================
        if(length(param_space_clicked$point) >= plot_index && !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]])) ) {
            point <- param_space_clicked$point[[plot_index]]
            if(it_is_x) abline(h=point[[index_y]], col=param_space_clicked_point$color, lwd=param_space_clicked_point$width)
            else        abline(v=point[[index_x]], col=param_space_clicked_point$color, lwd=param_space_clicked_point$width)
            # if(it_is_x) rect(full_range_x[1], point[[name_y]][1], full_range_x[2], point[[name_y]][2], col=param_space_clicked_point$color, lwd=param_space_clicked_point$width)
            # else        rect(point[[name_x]][1], full_range_y[1], point[[name_x]][2], full_range_y[2], col=param_space_clicked_point$color, lwd=param_space_clicked_point$width)
        }
        # this must be at the end
        param_space$globals[[plot_index]] <- checkpoint
    }
}


draw_param_space <- function(name_x, name_y, plot_index, boundaries) {
    params    <- loading_ps_file()$param_names
    variables <- loading_ps_file()$var_names
    list_of_all_names <- c(params, variables)
    index_x <- match(name_x,params)
    index_y <- match(name_y,params)
    full_range_x <- param_ranges()[[name_x]]
    full_range_y <- param_ranges()[[name_y]]
    if(!is.null(boundaries[[name_x]]))  range_x <- boundaries[[name_x]]
    else                                range_x <- param_ranges()[[name_x]]
    if(!is.null(boundaries[[name_y]]))  range_y <- boundaries[[name_y]]
    else                                range_y <- param_ranges()[[name_y]]
    
    ##============ drawing of 2D parameter space ===============================
    if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$param_space) != 0) {
        
        # create current set of globals
        checkpoint <- list(selectors=list(x=input[[paste0("param_selector_x_",plot_index)]], y=input[[paste0("param_selector_y_",plot_index)]]),
                           boundaries=boundaries,
                           density=input$density_coeficient,
                           coverage=input$coverage_check,
                           param_ss_clicked_point=param_ss_clicked$point[[plot_index]],
                           formula=chosen_ps_formulae_clean(),
                           input=loading_ps_file(),
                           # sps=satisfiable_param_space_for_formula(),
                           # sss=satisfiable_states(),
                           # counter=input$process_run,
                           sliders_checkbox=list(),
                           sliders=list() )
        for(x in 1:length(list_of_all_names)) {
            name <- list_of_all_names[[x]]
            if(!name %in% c(name_x,name_y)) {
                checkpoint$sliders_checkbox[[name]] <- input[[paste0("scale_switch_ps_",plot_index,"_",x)]]
                checkpoint$sliders[[name]] <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]]
            }
        }
        # check for any change in globals for particular plot
        if(is.na(param_space$globals[[plot_index]]) || !identical(param_space$globals[[plot_index]],checkpoint) ) {
            
            if(loading_ps_file()$type == "smt") {
                # symbolic type of parameters
                if(length(param_ss_clicked$point[[plot_index]]) == 0) {
                    ps <- copy(satisfiable_param_space_for_formula())
                } else {
                    ps <- loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & 
                                                            (state+1) %in% param_ss_clicked$point[[plot_index]],.(param=param+1,cov)]
                    ps <- merge(loading_ps_file()$params, ps, by.x="id", by.y="param")
                }
                ps <- unique(ps)
                setkey(ps,id)
                num <- input$density_coeficient
                dim_indices <- letters[-which(letters %in% c("x","y"))]
                input_params <- paste0("list(",name_x,"=x,",name_y,"=y")
                
                nesh <- meshgrid(seq(range_x[1],range_x[2],length.out = num),
                                 seq(range_y[1],range_y[2],length.out = num))
                dt <- data.table(x1=unlist(as.list(nesh$X[1:(num-1),1:(num-1)])),x2=unlist(as.list(nesh$X[2:num,2:num])),
                                 y1=unlist(as.list(nesh$Y[1:(num-1),1:(num-1)])),y2=unlist(as.list(nesh$Y[2:num,2:num])))
                dt[,x:=x1+(x2-x1)*0.5]
                dt[,y:=y1+(y2-y1)*0.5]
                for(p in 1:length(params)) {
                    name <- params[[p]]
                    if(!name %in% c(name_x,name_y)) {
                        data <- seq(param_ranges()[[name]][1],param_ranges()[[name]][2],length.out = num)
                        dt <- as.data.table(merge.default(dt, data.table(V1=data[1:(num-1)], V2=data[2:num]) ))
                        dt[, V3 := V1+(V2-V1)*0.5 ]
                        di <- dim_indices[p]
                        setnames( dt, c("V1","V2","V3"), c(paste0(di,1),paste0(di,2),di) )
                        input_params <- paste(input_params,paste0(name,"=",di),sep = ",")
                    }
                }
                dt[,cov:=0]
                dt[,id:=1:nrow(dt)]
                input_params <- paste0(input_params,")")
                
                #### Layers !!!!!!!!!
                ids <- dt$id    # all ids at first
                states <- copy(satisfiable_states())
                if(length(param_ss_clicked$point[[plot_index]]) == 0)    st_ids <- states$id     # all ids for states at first
                else                                                     st_ids <- param_ss_clicked$point[[plot_index]]
                for(x in 1:length(list_of_all_names)) {
                    name <- list_of_all_names[[x]]
                    if(!name %in% c(name_x,name_y) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]] ) {
                            if(name %in% variables) {
                                x <- x-length(params)
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(params))]] # right state value in dimension x
                                st_ids <- intersect(st_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, id])
                            } else {
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                                di <- dim_indices[x]
                                ids <- intersect(ids, dt[get(paste0(di,1)) <= sid & get(paste0(di,2)) >= sid, id])
                            }
                        }
                    }
                }
                ps <- ps[id %in% unique(loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & (state+1) %in% st_ids, param+1]) ]
                dt <- dt[id %in% ids ]
                
                time <- system.time(for(ex in ps$id) dt[,cov:=cov+ifelse(eval(parse(text=ps[id==ex,expr]))(eval(parse(text=input_params))),1,0)])
                print(time)
                
                if(input$coverage_check) {
                    dt <- dt[cov > 0,.(cov=max(.SD$cov)),by=.(x1,x2,y1,y2)]
                    param_space$data[[plot_index]] <- dt[cov > 0]
                } else {
                    dt[cov > 0, cov:=1]
                    param_space$data[[plot_index]] <- dt[cov > 0]
                }
                
            } else {
                # rectangular type of parameters
                if(length(param_ss_clicked$point[[plot_index]]) == 0) {
                    ps <- copy(satisfiable_param_space_for_formula())
                } else {
                    ps <- loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & 
                                                              (state+1) %in% param_ss_clicked$point[[plot_index]],.(param=param+1,cov)]
                    ps <- merge(loading_ps_file()$params, ps, by.x="id", by.y="param")
                }
                suppressWarnings(setnames(ps,c(paste0("V",index_x*2-1),paste0("V",index_x*2)),c("x1","x2")))
                suppressWarnings(setnames(ps,c(paste0("V",index_y*2-1),paste0("V",index_y*2)),c("y1","y2")))
                ps[, cov:=1 ]
                
                #### Layers !!!!!!!!!
                ids <- ps$row_id    # all ids at first
                states <- copy(satisfiable_states())
                if(length(param_ss_clicked$point[[plot_index]]) == 0)    st_ids <- states$id     # all ids for states at first
                else                                                     st_ids <- param_ss_clicked$point[[plot_index]]
                for(x in 1:length(list_of_all_names)) {
                    name <- list_of_all_names[[x]]
                    if(!name %in% c(name_x,name_y) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]] ) {
                            if(name %in% variables) {
                                x <- x-length(params)
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(params))]] # right state value in dimension x
                                st_ids <- intersect(st_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, id])
                            } else {
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                                ids <- intersect(ids, ps[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, row_id])
                            }
                        }
                    } else {
                        if(name == name_x) {
                            ids <- intersect(ids, ps[x1 < range_x[2] & x2 >= range_x[2] |
                                                     x1 <= range_x[1] & x2 > range_x[1] |
                                                     x1 >= range_x[1] & x2 <= range_x[2], row_id])
                        } else {
                            ids <- intersect(ids, ps[y1 < range_y[2] & y2 >= range_y[2] |
                                                     y1 <= range_y[1] & y2 > range_y[1] |
                                                     y1 >= range_y[1] & y2 <= range_y[2], row_id])
                        }
                    }
                }        # incremental intersection of ids in order to get right ids
                ps <- ps[row_id %in% ids & id %in% loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & (state+1) %in% st_ids, param+1 ] ]
                
                if(input$coverage_check && nrow(ps) != 0) {
                    num <- input$density_coeficient
                    nesh <- meshgrid(seq(range_x[1],range_x[2],length.out = num),
                                     seq(range_y[1],range_y[2],length.out = num))
                    dt <- data.table(x1=unlist(as.list(nesh$X[1:(num-1),1:(num-1)])),x2=unlist(as.list(nesh$X[2:num,2:num])),
                                     y1=unlist(as.list(nesh$Y[1:(num-1),1:(num-1)])),y2=unlist(as.list(nesh$Y[2:num,2:num])))
                    dt[,x:=x1+(x2-x1)*0.5]
                    dt[,y:=y1+(y2-y1)*0.5]
        
                    if(nrow(dt) != 0) {
                        uniq_x <- unique(ps[,.(x1,x2)])
                        uniq_y <- unique(ps[,.(y1,y2)])
                        timing <- system.time({
                            rang_x <- range(uniq_x)
                            rang_y <- range(uniq_y)
                            dt <- dt[x <= rang_x[2] & x >= rang_x[1] & y <= rang_y[2] & y >= rang_y[1] ]
                            dt <- dt[ps,.(x1=x.x1,x2=x.x2,y1=x.y1,y2=x.y2,id=i.id),on=.(x>=x1,x<=x2,y>=y1,y<=y2),allow.cartesian=T,nomatch=0][,.(cov=length(unique(id))),by=.(x1,x2,y1,y2)]
                        })
                        print(timing)
                        print(paste0("uniq cov: ",paste0(unique(dt$cov),collapse = ", ")))
                    } else print(paste0("dt is empty"))
        
                    param_space$data[[plot_index]] <- dt
                } else {
                    param_space$data[[plot_index]] <- ps
                }
            }
        }
        
        plot(full_range_x, full_range_y, type="n", xlab=name_x, ylab=name_y, xaxs="i", yaxs="i",
             xlim=range_x, ylim=range_y)
        
        ps <- param_space$data[[plot_index]]

        if(nrow(ps) != 0) {
            range_cov <- range(ps$cov)
            ps[,rect(x1, y1, x2, y2, col=rgb(0,0.5,0,alpha = (cov/range_cov[2])*ifelse(input$coverage_check, grey_shade(), 1)), border=NA)]
        }

        ##======= draw point due to click inside a plot =========================
        if(length(param_space_clicked$point) >= plot_index && !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]])) ) {
            point <- param_space_clicked$point[[plot_index]]
            if(!F %in% (c(name_x,name_y) %in% names(point))) {
                points(point[[index_x]], point[[index_y]],
                       col=param_space_clicked_point$color, pch=param_space_clicked_point$type, ps=param_space_clicked_point$size, lwd=param_space_clicked_point$width)
            # rect(point[[name_x]][1], point[[name_y]][1], point[[name_x]][2], point[[name_y]][2], col=param_space_clicked_point$color, lwd=param_space_clicked_point$width)
            }
        }
        # this must be at the end
        param_space$globals[[plot_index]] <- checkpoint
    }
}
draw_1D_param_space <- function(name_x, plot_index, boundaries) {
    params    <- loading_ps_file()$param_names
    variables <- loading_ps_file()$var_names
    list_of_all_names <- c(params, variables)
    index_x <- match(name_x,params)
    full_range_y <- c(0,1) # temporary
    full_range_x <- param_ranges()[[name_x]]
    if(!is.null(boundaries[[name_x]]))  range_x <- boundaries[[name_x]]
    else                                range_x <- param_ranges()[[name_x]]
    
    # drawing of 1D parameter space =================================================
    if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$param_space) != 0) {
        
        # create current set of globals
        checkpoint <- list(selectors=list(x=input[[paste0("param_selector_x_",plot_index)]], y=input[[paste0("param_selector_y_",plot_index)]]),
                           boundaries=boundaries,
                           param_ss_clicked_point=param_ss_clicked$point[[plot_index]],
                           formula=chosen_ps_formulae_clean(),
                           coverage=input$coverage_check,
                           # counter=input$process_run,
                           density=input$density_coeficient,
                           sliders_checkbox=list(),
                           sliders=list() )
        for(x in 1:length(list_of_all_names)) {
            name <- list_of_all_names[[x]]
            if(!name %in% name_x ) {
                checkpoint$sliders_checkbox[[name]] <- input[[paste0("scale_switch_ps_",plot_index,"_",x)]]
                checkpoint$sliders[[name]] <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]]
            }
        }
        # check for any change in globals for particular plot
        if(is.na(param_space$globals[[plot_index]]) || !identical(param_space$globals[[plot_index]],checkpoint) ) {
            
            if(loading_ps_file()$type == "smt") {
                # symbolic type of parameters
                if(length(param_ss_clicked$point[[plot_index]]) == 0) {
                    ps <- copy(satisfiable_param_space_for_formula())
                } else {
                    ps <- loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & 
                                                            (state+1) %in% param_ss_clicked$point[[plot_index]],.(param=param+1,cov)]
                    ps <- merge(loading_ps_file()$params, ps, by.x="id", by.y="param")
                }
                ps <- unique(ps)
                setkey(ps,id)
                num <- input$density_coeficient
                dim_indices <- letters[-which(letters %in% c("x"))]
                input_params <- paste0("list(",name_x,"=x")
                
                dt <- data.table(x1=seq(range_x[1],range_x[2],length.out = num)[1:(num-1)],
                                 x2=seq(range_x[1],range_x[2],length.out = num)[2:num])
                dt[,x:=x1+(x2-x1)*0.5]
                for(p in 1:length(params)) {
                    name <- params[p]
                    if(!name %in% c(name_x)) {
                        data <- seq(param_ranges()[[name]][1],param_ranges()[[name]][2],length.out = num)
                        dt <- as.data.table(merge.default(dt, data.table(V1=data[1:(num-1)], V2=data[2:num]) ))
                        dt[, V3 := V1+(V2-V1)*0.5 ]
                        di <- dim_indices[p]
                        setnames( dt, c("V1","V2","V3"), c(paste0(di,1),paste0(di,2),di) )
                        input_params <- paste(input_params,paste0(name,"=",di),sep = ",")
                    }
                }
                dt[,cov:=0]
                dt[,id:=1:nrow(dt)]
                input_params <- paste0(input_params,")")
                
                #### Layers !!!!!!!!!
                ids <- dt$id    # all ids at first
                states <- copy(satisfiable_states())
                if(length(param_ss_clicked$point[[plot_index]]) == 0)    st_ids <- states$id     # all ids for states at first
                else                                                     st_ids <- param_ss_clicked$point[[plot_index]]
                for(x in 1:length(list_of_all_names)) {
                    name <- list_of_all_names[[x]]
                    if(!name %in% c(name_x) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]] ) {
                            if(name %in% variables) {
                                x <- x-length(params)
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(params))]] # right state value in dimension x
                                st_ids <- intersect(st_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, id])
                            } else {
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                                di <- dim_indices[x]
                                ids <- intersect(ids, dt[get(paste0(di,1)) <= sid & get(paste0(di,2)) >= sid, id])
                            }
                        }
                    }
                }
                ps <- ps[id %in% unique(loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & (state+1) %in% st_ids, param+1]) ]
                dt <- dt[id %in% ids ]
                
                time <- system.time(for(ex in ps$id) dt[,cov:=cov+ifelse(eval(parse(text=ps[id==ex,expr]))(eval(parse(text=input_params))),1,0)])
                print(time)
                
                if(input$coverage_check) {
                    dt <- dt[cov > 0,.(cov=max(.SD$cov)),by=.(x1,x2)]
                    param_space$data[[plot_index]] <- dt[cov > 0]
                } else {
                    dt[cov > 0, cov:=1]
                    param_space$data[[plot_index]] <- dt[cov > 0]
                }
                
            } else {
                # rectangular type of parameters
                if(length(param_ss_clicked$point[[plot_index]]) == 0) {
                    ps <- copy(satisfiable_param_space_for_formula())
                } else {
                    ps <- loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & 
                                                            (state+1) %in% param_ss_clicked$point[[plot_index]],.(param=param+1,cov)]
                    ps <- merge(loading_ps_file()$params, ps, by.x="id", by.y="param")
                }
                setnames(ps,c(paste0("V",index_x*2-1),paste0("V",index_x*2)),c("x1","x2"))
                ps[,cov:=1]
                
                #### Layers !!!!!!!!!
                ids <- ps$row_id    # all ids at first
                states <- copy(satisfiable_states())
                if(length(param_ss_clicked$point[[plot_index]]) == 0)    st_ids <- states$id     # all ids for states at first
                else                                                     st_ids <- param_ss_clicked$point[[plot_index]]
                for(x in 1:length(list_of_all_names)) {
                    name <- list_of_all_names[[x]]
                    if(!name %in% name_x ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]] ) {
                            if(name %in% variables) {
                                x <- x-length(params)
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(params))]] # right state value in dimension x
                                st_ids <- intersect(st_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, id])
                            } else {
                                sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                                ids <- intersect(ids, ps[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid, row_id])
                            }
                        }
                    } else {
                        ids <- intersect(ids, ps[x1 < range_x[2] & x2 >= range_x[2] |
                                                 x1 <= range_x[1] & x2 > range_x[1] |
                                                 x1 >= range_x[1] & x2 <= range_x[2], row_id])
                    }
                }        # incremental intersection of ids in order to get right ids
                ps <- ps[row_id %in% ids & id %in% loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & (state+1) %in% st_ids, param+1 ] ]
                
                if(input$coverage_check && nrow(ps) != 0) {
                    num <- input$density_coeficient
                    dt <- data.table(x1=seq(range_x[1],range_x[2],length.out = num)[1:(num-1)],
                                     x2=seq(range_x[1],range_x[2],length.out = num)[2:num])
                    dt[,x:=x1+(x2-x1)*0.5]
                    
                    if(nrow(dt) != 0) {
                        uniq_x <- unique(ps[,.(x1,x2)])
                        # timing <- system.time({
                        #     rang_x <- range(uniq_x)
                        #     dt <- dt[x <= rang_x[2] & x >= rang_x[1] ]
                        #     setkey(ps,x1,x2)
                        #     one <- foverlaps(dt[,.(x=x,xe=x)],ps,by.x = c("x","xe"),type="within")[,.(cov=length(unique(id))),by=.(x)]
                        #     dt <- merge(dt,one,by.x=c("x"),by.y=c("x"))
                        #     rm(one)
                        # })
                        # print(timing)
                        timing <- system.time({
                            rang_x <- range(uniq_x)
                            dt <- dt[x <= rang_x[2] & x >= rang_x[1] ]
                            dt <- dt[ps,.(x1=x.x1,x2=x.x2,id=i.id),on=.(x>=x1,x<=x2),allow.cartesian=T,nomatch=0][,.(cov=length(unique(id))),by=.(x1,x2)]
                        })
                        print(timing)
                        print(paste0("uniq cov: ",paste0(unique(dt$cov),collapse = ", ")))
                    } else print(paste0("dt is empty"))
                    
                    param_space$data[[plot_index]] <- dt
                } else {
                    param_space$data[[plot_index]] <- ps
                }
            }
        }     
        plot(full_range_x, full_range_y, type="n", xlab=name_x, ylab="", yaxt="n", xaxs="i", yaxs="i",
             xlim=range_x)
        
        ps <- param_space$data[[plot_index]]
        if(nrow(ps) != 0) {
            range_cov <- range(ps$cov)
            ps[,rect(x1, full_range_y[1], x2, full_range_y[2], col=rgb(0,0.5,0,alpha = (cov/range_cov[2])*ifelse(input$coverage_check, grey_shade(), 1) ), border=NA)]
        }
        
        ##======= draw point due to click inside a plot =========================
        if(length(param_space_clicked$point) >= plot_index && !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]])) ) {
            point <- param_space_clicked$point[[plot_index]]
            abline(v=point[[index_x]], col=param_space_clicked_point$color, lwd=param_space_clicked_point$width)
            # rect(point[[name_x]][1], full_range_y[1], point[[name_x]][2], full_range_y[2], col=param_space_clicked_point$color, lwd=param_space_clicked_point$width)
        }
        # this must be at the end
        param_space$globals[[plot_index]] <- checkpoint
    }
}


# observers providing zooming and unzooming of state-space plots
zoom_param_ss_ranges <- observe({
    if(!is.null(loading_ps_file()) ) {
        for(i in visible_ps_plots()) {
            if(!is.null(input[[paste0("param_ss_",i,"_brush")]])) isolate({
                brush <- input[[paste0("param_ss_",i,"_brush")]]
                cat("brush in param ss ",i,": ",brush$xmin,",",brush$xmax,",",brush$ymin,",",brush$ymax,"\n")
                param_ss_brushed$data[[i]] <- lapply(loading_ps_file()$var_names, function(x) {
                    if(x==input[[paste0("param_ss_selector_y_",i)]] || x==input[[paste0("param_ss_selector_x_",i)]]) {
                        if(x==input[[paste0("param_ss_selector_x_",i)]]) c(brush$xmin,brush$xmax)
                        else c(brush$ymin,brush$ymax)
                    } else range(loading_ps_file()$thresholds[[x]])
                })
                names(param_ss_brushed$data[[i]]) <- loading_ps_file()$var_names
            }) 
        }
    }
})
unzoom_param_ss_ranges <- observe({
    if(!is.null(loading_ps_file()) ) {
        for(i in visible_ps_plots()) {
            button <- input[[paste0("unzoom_plot_param_ss_",i)]]
            if(!is.null(button) && (button > param_ss_brushed$click_counter[[i]])) isolate({
                param_ss_brushed$data[[i]] <- lapply(loading_ps_file()$thresholds, range)
                names(param_ss_brushed$data[[i]]) <- loading_ps_file()$var_names
                param_ss_brushed$click_counter[[i]] <- button
            })
        }
    }
})

# observers providing zooming and unzooming of param-space plots
zoom_ps_ranges <- observe({
    if(!is.null(loading_ps_file()) ) {
        for(i in visible_ps_plots()) {
            list_of_all_names <- c(loading_ps_file()$param_names, loading_ps_file()$var_names)
            if(!is.null(input[[paste0("ps_",i,"_brush")]]) && !is.null(param_ranges())) isolate({
                brush <- input[[paste0("ps_",i,"_brush")]]
                cat("brush in ps ",i,": ",brush$xmin,",",brush$xmax,",",brush$ymin,",",brush$ymax,"\n") 
#                 if(input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names) {
                    # this part is for mixed Param-space plot (with 1 parameter and 1 variable)
                    ps_brushed$data[[i]] <- lapply(list_of_all_names, function(x) {
                        if(x==input[[paste0("param_selector_y_",i)]] || x==input[[paste0("param_selector_x_",i)]]) {
                            if(x==input[[paste0("param_selector_x_",i)]])   c(brush$xmin,brush$xmax)
                            else                                            c(brush$ymin,brush$ymax)
                        } else {
                            if(x %in% loading_ps_file()$param_names)    param_ranges()[[x]]
                            else                                        range(loading_ps_file()$thresholds[[x]])
                        }
                    })
                    names(ps_brushed$data[[i]]) <- list_of_all_names
#                 } else {
#                     # this part is for normal Param-space plot (with 2 parameters)
#                     ps_brushed$data[[i]] <- lapply(loading_ps_file()$param_names, function(x) {
#                         if(x==input[[paste0("param_selector_y_",i)]] || x==input[[paste0("param_selector_x_",i)]]) {
#                             if(x==input[[paste0("param_selector_x_",i)]]) c(brush$xmin,brush$xmax)
#                             else c(brush$ymin,brush$ymax)
#                         } else param_ranges()[[x]]
#                     })
#                     names(ps_brushed$data[[i]]) <- loading_ps_file()$param_names
#                 }
            })
        }
    }
})
unzoom_ps_ranges <- observe({
    if(!is.null(loading_ps_file()) ) {
        for(i in visible_ps_plots()) {
            button <- input[[paste0("unzoom_plot_ps_",i)]]
            if(!is.null(button) && (button > ps_brushed$click_counter[[i]]) && !is.null(param_ranges())) isolate({
                if(input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names) {
                    ps_brushed$data[[i]] <- c(lapply(param_ranges(), range), lapply(loading_ps_file()$thresholds, range))
                    names(ps_brushed$data[[i]]) <- c(loading_ps_file()$param_names, loading_ps_file()$var_names)
                } else {
                    ps_brushed$data[[i]] <- lapply(param_ranges(), range)
                    names(ps_brushed$data[[i]]) <- loading_ps_file()$param_names
                }
                ps_brushed$click_counter[[i]] <- button
            })
        }
    }
})

clicked_in_ps <- observe({
    if(!is.null(loading_ps_file()) ) {
        params <- loading_ps_file()$param_names
        list_of_all_names <- c(loading_ps_file()$param_names, loading_ps_file()$var_names)
        for(i in visible_ps_plots()) {
            clicked_point <- input[[paste0("ps_",i,"_dblclick")]]
            if(!is.null(clicked_point)) isolate({
                cat("clicked in ps ",i,": ",clicked_point$x,",",clicked_point$y,"\n")
                if(input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names) {
                    param_space_clicked$point[[i]] <- c(clicked_point$x, clicked_point$y)
                    names(param_space_clicked$point[[i]]) <- c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]])
                } else {
                    if(input[[paste0("param_selector_x_",i)]] == input[[paste0("param_selector_y_",i)]]) {
                        param_space_clicked$point[[i]] <- c(clicked_point$x)
                        names(param_space_clicked$point[[i]]) <- c(input[[paste0("param_selector_x_",i)]])
                    } else {
                        param_space_clicked$point[[i]] <- c(clicked_point$x, clicked_point$y)
                        names(param_space_clicked$point[[i]]) <- c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]])
                    }
                }
            })
        }
    }
})

erase_in_ps <- observe({
    if(!is.null(loading_ps_file()) ) {
        for(i in visible_ps_plots()) {
            button <- input[[paste0("clear_plot_ps_",i)]]
            if(!is.null(button) && (button > param_space_clicked$click_counter[[i]]) ) isolate({
                param_space_clicked$point[[i]] <- NA
                param_space_clicked$click_counter[[i]] <- button
            })
            if(length(param_space_clicked$point) >= i && !is.null(param_space_clicked$point[[i]]) && !is.na(param_space_clicked$point[[i]])) {
                if(length(unique(c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]]))) != length(names(param_space_clicked$point[[i]])) ||
                   F %in% (names(param_space_clicked$point[[i]]) %in% c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]]))) {
                    param_space_clicked$point[[i]] <- NA
                }
            }
        }
    }
})
apply_to_all_in_ps <- observe({
    if(!is.null(loading_ps_file()) ) {
        for(i in visible_ps_plots()) {
            button <- input[[paste0("apply_plot_ps_",i)]]
            if(!is.null(button) && (button > param_space_clicked$apply_to_all_click_counter[[i]])) isolate({
                if(length(param_space_clicked$point) >= i && !is.null(param_space_clicked$point[[i]]) && !is.na(param_space_clicked$point[[i]])) {
                    for(x in visible_ps_plots()) param_space_clicked$point[[x]] <- param_space_clicked$point[[i]]
                }
                param_space_clicked$apply_to_all_click_counter[[i]] <- button
            })
        }
    }
})

click_in_param_ss <- observe({
    if(!is.null(loading_ps_file()) ) { # && input$add_param_plot > 0) {
        states <- copy(satisfiable_states())
        
        for(ii in visible_ps_plots()) local({
            i <- ii
            if(!is.null(input[[paste0("param_ss_selector_x_",ii)]]) && input[[paste0("param_ss_selector_x_",ii)]] != empty_sign && 
               !is.null(input[[paste0("param_ss_selector_y_",ii)]]) && input[[paste0("param_ss_selector_y_",ii)]] != empty_sign) {
                
                index_x <- match(input[[paste0("param_ss_selector_x_",ii)]],loading_ps_file()$var_names)
                index_y <- match(input[[paste0("param_ss_selector_y_",ii)]],loading_ps_file()$var_names)
                
                ids <- states$id    # all ids at first
                for(x in 1:length(loading_ps_file()$var_names)) {
                    if(!x %in% c(index_x,index_y)) {
                        sid <- input[[paste0("scale_slider_param_ss_",i,"_",x)]] # right state value in dimension x
                        ids <- intersect(ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) >= sid,id])
                    }
                }        # incremental intersection of ids in order to get right ids
                states <- states[id %in% ids]
                
                point <- input[[paste0("param_ss_",i,"_dblclick")]]
                if(!is.null(point) ) isolate({
                    cat("param_ss_plot ",i,":",point$x,",",point$y,"\n")
                    if(input[[paste0("param_ss_selector_x_",ii)]] == input[[paste0("param_ss_selector_y_",ii)]])
                        picked_state <- states[get(paste0("V",index_x*2-1)) <= point$x & get(paste0("V",index_x*2)) >= point$x,id]
                    else
                        picked_state <- states[get(paste0("V",index_x*2-1)) <= point$x & get(paste0("V",index_x*2)) >= point$x &
                                               get(paste0("V",index_y*2-1)) <= point$y & get(paste0("V",index_y*2)) >= point$y,id]  # it obtains right state_id where user clicked
                    if(length(picked_state) != 0) {     # in case some unsatisfied state is double-clicked
                        if(picked_state %in% param_ss_clicked$point[[i]])
                            param_ss_clicked$point[[i]] <- setdiff(param_ss_clicked$point[[i]],picked_state)
                        else 
                            param_ss_clicked$point[[i]] <- union(param_ss_clicked$point[[i]],picked_state)
                    }
                })
            }
        })
    }
})
erase_in_param_ss <- observe({
    if(!is.null(loading_ps_file()) ) { # && input$add_param_plot > 0) {
        for(i in visible_ps_plots()) {
            button <- input[[paste0("clear_plot_param_ss_",i)]]
            if(!is.null(button) && (button > param_ss_clicked$click_counter[[i]])) isolate({
                #cat("param_ss_plot ",i,": cancled\n")
                param_ss_clicked$point[[i]] <- vector()
                param_ss_clicked$click_counter[[i]] <- button
            })
        }
    }
})
apply_to_all_in_param_ss <- observe({
    if(!is.null(loading_ps_file()) ) { # && input$add_param_plot > 0) {
        for(i in visible_ps_plots()) {
            button <- input[[paste0("apply_plot_param_ss_",i)]]
            if(!is.null(button) && (button > param_ss_clicked$apply_to_all_click_counter[[i]])) isolate({
                if(length(param_ss_clicked$point) >= i && length(param_ss_clicked$point[[i]]) != 0) {
                    for(x in visible_ps_plots()) param_ss_clicked$point[[x]] <- param_ss_clicked$point[[i]]
                }
                param_ss_clicked$apply_to_all_click_counter[[i]] <- button
            })
        }
    }
})


grey_shade <- reactive({
    return(input$color_alpha_coeficient)
})

satisfiable_param_space_for_formula <- reactive({
    if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$param_space) != 0) {
        sat <- loading_ps_file()$param_space[formula==chosen_ps_formulae_clean(),.(param=param+1,cov)]
        sat <- merge(loading_ps_file()$params, sat, by.x="id", by.y="param")
        return(sat)
    } else return(NULL)
})


param_ranges <- reactive({
    if(!is.null(loading_ps_file())) {
        return(loading_ps_file()$param_bounds)
    } else return(NULL)
})

satisfiable_states <- reactive({
    if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$param_space) != 0) {
        ss <- loading_ps_file()$states[id %in% loading_ps_file()$param_space[formula==chosen_ps_formulae_clean(),state+1]]
        return(ss)
    } else return(NULL)
})

})