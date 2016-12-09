# in linux install: r-base-dev

if(!require(shiny,quietly = T)) {install.packages("shiny", dependencies=T,quiet = T); library(shiny,quietly = T)}
if(!require(shinyBS,quietly = T)) {install.packages("shinyBS", dependencies=T,quiet = T); library(shinyBS,quietly = T)}
if(!require(pracma,quietly = T)) {install.packages("pracma", dependencies=T,quiet = T); require(pracma,quietly = T)}
if(!require(stringr,quietly = T)) {install.packages("stringr", dependencies=T,quiet = T); library(stringr,quietly = T)}
if(!require(data.table,quietly = T)) {install.packages("data.table", dependencies=T,quiet = T); library(data.table,quietly = T)}
if(!require(rjson,quietly = T)) {install.packages("rjson", dependencies=T,quiet = T); library(rjson,quietly = T)}
#if(!require(Rcpp,quietly = T)) {install.packages("Rcpp",quiet = T); library(Rcpp,quietly = T)}
#if(!require(httpuv,quietly = T)) {install.packages("httpuv",quiet = T); library(httpuv,quietly = T)}
#if(!require(shinyjs,quietly = T)) {install.packages("shinyjs",quiet = T); library(shinyjs,quietly = T)}
#if(!require(shinythemes,quietly=T)) install.packages("shinythemes",quiet=T); library(shinythemes,quietly=T)
#if(!require(ggplot2,quietly = T)) install.packages("ggplot2",quiet = T); library(ggplot2,quietly = T)

source("global.R")
#source("generator.R")

options(scipen=999)
options(shiny.maxRequestSize=1000*1024^2)
#gcinfo(TRUE)   # for periodically showing garbage collection stats

#jsResetCode <- "shinyjs.reset = function() {history.go(0);}"

shinyServer(function(input,output,session) {

#=============== GLOBALS ===============================================
session_random <- sample(1000^2,1)
    
# .Platform$OS.type=="windows"  or Sys.info()["sysname"]=="Windows"
files_path <- ifelse(.Platform$OS.type=="windows", paste0("..//Temp//"), ifelse(Sys.info()["nodename"]=="psyche05",paste0("..//Temp//"),paste0("~//skola//newbiodivine//") ))
# java_programs_path <- ifelse(Sys.info()["nodename"]=="psyche05","//mirror//new_new_biodivine//","~//skola//newbiodivine//")
new_programs_path <- ifelse(.Platform$OS.type=="windows", paste0("..//biodivine-ctl//build//install//biodivine-ctl//bin//"), 
                            ifelse(Sys.info()["nodename"]=="psyche05","..//biodivine-ctl//build//install//biodivine-ctl//bin//","~//skola//newbiodivine//"))

progressFileName <- paste0(files_path,"progress.",session_random,".txt")
file.create(progressFileName)
progressFile <- reactiveFileReader(100,session,progressFileName,readLines)

resultFileName <- paste0(files_path,"result.",session_random,".json")
file.create(resultFileName)
resultFile <- reactiveFileReader(1000,session,resultFileName,readLines)

configFileName <- paste0(files_path,"config.",session_random,".json")
file.create(configFileName)

#session$onSessionEnded(for(i in c(progressFileName,resultFileName,configFileName)) if(file.exists(i)) file.remove(i))
#session$onSessionEnded(stopApp)
#session$onSessionEnded(file.remove(configFileName,progressFileName,resultFileName))


# loaded_vf_file_name <<- "nothing"
loaded_prop_file    <- reactiveValues(data=NULL,filename=NULL)
loaded_vf_file      <- reactiveValues(data=NULL,filename=NULL)
loaded_ss_file      <- reactiveValues(data=NULL,filename=NULL)
loaded_ps_file      <- reactiveValues(data=NULL,filename=NULL)

vf_brushed          <- reactiveValues(data=list(),click_counter=list())
ss_brushed          <- reactiveValues(data=list(),click_counter=list())
ps_brushed          <- reactiveValues(data=list(),click_counter=list())
param_ss_brushed    <- reactiveValues(data=list(),click_counter=list())

vector_field_space  <- reactiveValues(data=list(),globals=list())
transition_state_space<- reactiveValues(data=list(),globals=list())
param_state_space   <- reactiveValues(data=list(),globals=list())
param_space         <- reactiveValues(data=list(),globals=list())
#satisfiable_ps      <- reactiveValues(data=list())

vector_field_clicked  <- reactiveValues(data=list(),point=list(),old_point=list(),click_counter=list(),apply_to_all_click_counter=list(),apply_to_tss_click_counter=list())
state_space_clicked   <- reactiveValues(data=list(),point=list(),old_point=list(),click_counter=list(),apply_to_all_click_counter=list())
param_ss_clicked      <- reactiveValues(data=list(),point=list(),old_point=list(),click_counter=list(),apply_to_all_click_counter=list())
param_space_clicked   <- reactiveValues(data=list(),point=list(),old_point=list(),click_counter=list(),apply_to_all_click_counter=list())

vf_chosen           <- reactiveValues(data=NULL,max=1)
param_chosen        <- reactiveValues(data=NULL,max=1)


#=============== TEST TAB ==============================================

# observeEvent(input$start_btn,{          # temporary
#     if(input$start_btn != 0){
#         #progress <<- shiny::Progress$new(session, min=1, max=30)
#         system2("C://Program Files//R//R-3.1.2//bin//Rscript.exe",c("C://Users//User//skola//pracovny//R//test_of_pipe//cycle.R",">",progressFileName),stdout="",wait=F)
#         #progress$set(message = "Runnnig...", detail = NULL)
#         #on.exit(file.remove(progressFileName))
#     }
# })
# 
# 
# output$text_area <- renderPrint({
#     if(length(progressFile()) > 0) {
#         progress$set(value = as.numeric(progressFile()[length(progressFile())]))
#         if(length(progressFile()) > progressMaxLength)
#             cat(paste0(progressFile()[(length(progressFile())-progressMaxLength+1):length(progressFile())],collapse="\n"))
#         else
#             cat(paste0(progressFile(),collapse="\n"))
#         #on.exit(file.remove(progressFileName))
#     }
# #     if(length(progressFile() > 0))
# #         for(i in 1:length(progressFile()))
# #             cat(progressFile()[i],"\n")
# },width=100)
    

#=============== PROCESS SETTINGS TAB ==================================
#=======================================================================

observeEvent(input$process_run,{
    if(!is.null(loaded_ss_file$data) && input$process_run != 0) {
        cat("Parameter synthesis is started\n",file=progressFileName)
        modelTempName  <- paste0(files_path,"model.",session_random,".abst.bio")
        writeLines(loaded_ss_file$data,modelTempName)
        propTempName   <- paste0(files_path,"prop.",session_random,".ctl")
        writeLines(loaded_prop_file$data,propTempName)
        system2(paste0(new_programs_path,"combine"),c(modelTempName, propTempName), stdout=configFileName, stderr=progressFileName, wait=T)
        # system2("java", c("-jar",paste0(java_programs_path,"combine.jar"), modelTempName, propTempName, ">", configFileName, "2>", progressFileName), wait=T)
        file.remove(modelTempName,propTempName)
        if(file.exists(configFileName) && length(readLines(configFileName)) > 0) {
            cat("config file is created\n")
            cat("Config file is created\n",file=progressFileName,append=T)
            updateButton(session,"process_run",style="default",disabled=T)
            updateButton(session,"process_stop",style="danger",disabled=F)
            checker_path <- ifelse(.Platform$OS.type=="windows", paste0("..//biodivine-ctl//build//install//biodivine-ctl//bin//"),
                                   ifelse(Sys.info()["nodename"]=="psyche05","..//biodivine-ctl//build//install//biodivine-ctl//bin//",
                                   "/home/demon/skola/newbiodivine/json-ode-model/target/release/"))     # it must be whole path or be a part of PATH
            system2(paste0(checker_path,"biodivine-ctl"), c(configFileName,">",resultFileName,"2>",progressFileName), wait=F)
            cat("Process has started\n",file=progressFileName)
            # system2(paste0(checker_path,"ode_model"), c(configFileName,">",resultFileName,"2>>",progressFileName), wait=F)
        } else cat("\nError: some error occured, because no config file was created!\n")
    }
})

observeEvent(input$process_stop,{
    if(!is.null(loaded_ss_file$data) && input$process_stop != 0) {
        if(file.exists(progressFileName) && !is.null(progressFile()) && length(progressFile()) > 0 && !T %in% grepl("^!!DONE!!$",progressFile())) {
            pid <- gsub("PID: ","",grep("^PID: [0-9]+$",progressFile(),value=T))
            command <- ifelse(.Platform$OS.type=="windows", paste0("taskkill /f /pid ",pid), paste0("kill -9 ",pid))
            system(command,wait=T)
            cat("Process was killed!\n",file=progressFileName)
            updateButton(session,"process_stop",style="default",disabled=T)
            updateButton(session,"process_run",style="success",disabled=F)
        }
    }
})

output$progress_output <- renderPrint({
    if(file.exists(progressFileName) && !is.null(progressFile()) && length(progressFile()) > 0) {
        # progress$set(value = as.numeric(progressFile()[length(progressFile())]))
        if(T %in% grepl("Model does not contain threshold",progressFile())) cat("chyba threshold\n")
        if(T %in% grepl("^!!DONE!!$",progressFile())) {
            # TODO: here should be call for message window
            updateButton(session,"process_stop",style="default",disabled=T)
            updateButton(session,"add_result_plot", disabled=F)
        }
        
        # if(length(progressFile()) > progressMaxLength)
        #     cat(paste0(progressFile()[(length(progressFile())-progressMaxLength+1):length(progressFile())],collapse="\n"))
        # else
        cat(paste0(progressFile(),collapse="\n"))
        #on.exit(file.remove(progressFileName))
    }
})

#=============== PROPERTY INPUT TAB ====================================
#=======================================================================

# reaction on event of loading .ctl file into tool putting a loaded model into text field
# observeEvent(c(loaded_prop_file$data,input$reset_prop),{
#     if(!is.null(loaded_prop_file$data)) {
#         updateTextInput(session,"prop_input_area",value = paste(loaded_prop_file$data,collapse="\n"))
#     }
# })


observeEvent(input$prop_input_area,{
    loaded_prop_file$data <- strsplit(input$prop_input_area,"\n",fixed=T)[[1]]
    if(!is.null(loaded_ss_file$data)) updateButton(session,"process_run",style="success",disabled=F)
})
# observeEvent(input$accept_prop_changes,{
#     if(!is.null(input$prop_input_area) && input$prop_input_area != "")
#         loaded_prop_file$data <- strsplit(input$prop_input_area,"\n",fixed=T)[[1]]
# })

observeEvent(c(input$prop_file,input$reset_prop),{
    if(!is.null(input$prop_file) && !is.null(input$prop_file$datapath)) {
#         if(!is.null(loaded_prop_file$filename) && loaded_prop_file$filename != input$prop_file$datapath) {
#             #session$reload()
#             reset_globals()
#         }
        loaded_prop_file$filename <- input$prop_file$datapath
        loaded_prop_file$data <- readLines(loaded_prop_file$filename)
    } else {
        # initial example file (temporary)
        loaded_prop_file$filename <- paste0(examples_dir,"//repressilator_2D//bistability.ctl")
        loaded_prop_file$data <- readLines(loaded_prop_file$filename)
    }
    updateTextInput(session,"prop_input_area",value = paste(loaded_prop_file$data,collapse="\n"))
})

output$save_prop_file <- downloadHandler(
    filename = function() { 
        if(!is.null(input$prop_file) && !is.null(input$prop_file$datapath))
            return(paste0(input$prop_file$datapath))
        else return("property.ctl")
    },
    content = function(file) {
        if(!is.null(loaded_prop_file$data))
            writeLines(loaded_prop_file$data, file)
        else writeLines("", file)
    }
)

#=============== MODEL INPUT TAB =======================================
#=======================================================================

reset_globals <- function() {
    for(i in vf_chosen$data) {
        vf_chosen$data <- vf_chosen$data[vf_chosen$data != i]
    }
}

# reaction on event of loading .bio file into tool putting a loaded model into text field
# observeEvent(c(loaded_vf_file$data,input$reset_model),{
#     if(!is.null(loaded_vf_file$data)) {
#         updateTextInput(session,"model_input_area",value = paste(loaded_vf_file$data,collapse="\n"))
#     }
# })

observeEvent(input$accept_model_changes,{
    if(!is.null(input$model_input_area) && input$model_input_area != "") {
        the_lines <- strsplit(input$model_input_area,"\n",fixed=T)[[1]]        
        its_ok <- F

lines <- the_lines[which(the_lines!="")]
name <- "[a-zA-Z][a-zA-Z0-9_]*"
rnum <- "[+-]?([0-9]*[.])?[0-9]+" # TODO: overit "[+-]?(([1-9][0-9]*)|(0))([.][0-9]+)?"
znum <- "[0-9]+"

line_id <- which(grepl(paste0("^VARS:",name,"(,",name,")*$"), gsub(" ","",lines)) )
if(length(line_id) == 0 || length(line_id) > 1) cat("'VARS:' line is missing or duplicated or contains syntax error!\n",file=progressFileName)
else {
    vars_num <- length(strsplit(gsub(" ","",sub("^VARS:","",lines[line_id])),",")[[1]])
    lines <- lines[-line_id]
    
    p_item <- paste0(name,",",rnum,",",rnum)
    line_id <- which(grepl(paste0("^PARAMS:",p_item,"(;",p_item,")*$"), gsub(" ","",lines)) )
    if(length(line_id) == 0 || length(line_id) > 1) cat("'PARAMS:' line is missing or duplicated or contains syntax error!\n",file=progressFileName)
    else {
        lines <- lines[-line_id]
        
        line_id <- which(grepl(paste0("^THRES:",name,":",rnum,"(,",rnum,")+$"), gsub(" ","",lines)) )
        if(length(line_id) != vars_num) cat("'THRES:' lines contain syntax error or there is different number of them than variables!\n",file=progressFileName)
        else {
            lines <- lines[-line_id]
            
#             fname <- "" # TODO:
#             func <- paste0(fname,"\\(",name,",",rnum,"(,",rnum,"){1:3}\\)")
            line_id <- which(grepl(paste0("^EQ:",name,"=",".+"), gsub(" ","",lines)) )
            if(length(line_id) == 0 || length(line_id) > vars_num) cat("'EQ:' lines contain syntax error or there is different number of them than variables!\n",file=progressFileName)
            else {
                lines <- lines[-line_id]
                
                line_id <- which(grepl("(^CONSTS:|^VAR_POINTS:)", gsub(" ","",lines)) )
                if(length(lines) != length(line_id)) cat("Line [",which(the_lines %in% ifelse(length(line_id)==0,lines,lines[-line_id])),"] is undefined or contains syntax error!\n",file=progressFileName)
                else {
                    line_id <- which(grepl(paste0("^CONSTS:"), gsub(" ","",lines)) )
                    if(length(line_id) > 1) cat("'CONSTS:' line is duplicated!\n",file=progressFileName)
                    else if(length(line_id) == 1) {
                        p_item <- paste0(name,",",rnum)
                        line_id <- which(grepl(paste0("^CONSTS:",p_item,"(;",p_item,")*$"), gsub(" ","",lines)) )
                        if(length(line_id) == 0 || length(line_id) > 1) cat("'CONSTS:' line contains syntax error!\n",file=progressFileName)
                        else {
                            lines <- lines[-line_id]
                            line_id <- which(grepl(paste0("^VAR_POINTS:"), gsub(" ","",lines)) )
                            if(length(line_id) > 1) cat("'VAR_POINTS:' line is duplicated!\n",file=progressFileName)
                            else if(length(line_id) == 1) {
                                p_item <- paste0(name,":",znum,",",znum)
                                line_id <- which(grepl(paste0("^VAR_POINTS:",p_item,"(;",p_item,"){",0,",",vars_num-1,"}$"), gsub(" ","",lines)) )
                                if(length(line_id) == 0 || length(line_id) > 1) cat("'VAR_POINTS:' line contains syntax error or contains more items than variables!\n",file=progressFileName)
                                else its_ok <- T
                            } else its_ok <- T # no VAR_POINTS line
                        }
                    } else {
                        # no CONSTS line
                        line_id <- which(grepl(paste0("^VAR_POINTS:"), gsub(" ","",lines)) )
                        if(length(line_id) > 1) cat("'VAR_POINTS:' line is duplicated!\n",file=progressFileName)
                        else if(length(line_id) == 1) {
                            p_item <- paste0(name,":",znum,",",znum)
                            line_id <- which(grepl(paste0("^VAR_POINTS:",p_item,"(;",p_item,"){",0,",",vars_num-1,"}$"), gsub(" ","",lines)) )
                            if(length(line_id) == 0 || length(line_id) > 1) cat("'VAR_POINTS:' line contains syntax error or contains more items than variables!\n",file=progressFileName)
                            else its_ok <- T
                        } else its_ok <- T # no VAR_POINTS line and no CONSTS line
                    }
                }
            }
        }
    }
}
        
        # grepl(paste0("^VARS:",name,"(,",name,")*$"), gsub(" ","",lines))
        # grepl(paste0("^PARAMS:",p_item,"(;",p_item,")*$"), gsub(" ","",lines))
        # grepl(paste0("^THRES:",name,":",rnum,"(,",rnum,")+$"), gsub(" ","",lines))
        # grepl(paste0("^EQ:",name,"=",rnum,"(,",rnum,")+$"), gsub(" ","",lines))
        
        if(its_ok) {
            cat("Syntax of model is good ;) You may proceed with generating approximation.",file=progressFileName)
            loaded_vf_file$data <- the_lines
        }
    }
})

observeEvent(input$model_input_area,{
    loaded_vf_file$data <- strsplit(input$model_input_area,"\n",fixed=T)[[1]]
    updateButton(session,"generate_abstraction", style="success", disabled=F)
    updateButton(session,"add_vf_plot", disabled=F)
})

observeEvent(c(input$vf_file,input$reset_model),{
    cat("Start with button 'generate approximation'.\n",file=progressFileName)
    if(!is.null(input$vf_file) && !is.null(input$vf_file$datapath)) {
        if(!is.null(loaded_vf_file$filename) && loaded_vf_file$filename != input$vf_file$datapath) {
            #session$reload()
            reset_globals()
        }
        loaded_vf_file$filename <- input$vf_file$datapath
        loaded_vf_file$data <- readLines(loaded_vf_file$filename)
    } else {
        # initial example file (temporary)
        loaded_vf_file$filename <- paste0(examples_dir,"//repressilator_2D//model_2D_1P_100R.bio")
        loaded_vf_file$data <- readLines(loaded_vf_file$filename)
    }
    updateTextInput(session,"model_input_area",value = paste(loaded_vf_file$data,collapse="\n"))
})

output$save_model_file <- downloadHandler(
    filename = ifelse(!is.null(input$vf_file) && !is.null(input$vf_file$datapath), paste0(input$vf_file$datapath), "model.bio"),
    content = function(file) {
        if(!is.null(loaded_vf_file$data))
            writeLines(loaded_vf_file$data, file)
        else writeLines("", file)
    }
)

#=======================================================================


observeEvent(input$generate_abstraction,{
    if(input$generate_abstraction != 0) {
        # loaded_ss_file$data <- readLines(paste0(examples_dir,"//model_2D_1P_400R.abst.bio"))
        cat("tractor is about to run\n")
        cat("Approximation is started\n",file=progressFileName)
        
        withProgress({
            model_temp_name  <- paste0(files_path,"model.",session_random,".bio")
            writeLines(loaded_vf_file$data,model_temp_name)
            abstracted_model_temp_name <- paste0(files_path,"model.",session_random,".abst.bio")
            system2(paste0(new_programs_path,"tractor"),c(model_temp_name,
                                                          ifelse(input$fast_approximation,"true","false"),
                                                          ifelse(input$thresholds_cut,"true","false")),
                                                          stdout = abstracted_model_temp_name,
                                                          stderr = progressFileName, wait=T)
            # system2("java", c("-jar",paste0(java_programs_path,"tractor.jar"), model_temp_name, 
            #                   ifelse(input$fast_approximation,"true","false"),
            #                   ifelse(input$thresholds_cut,"true","false"),
            #                   ">", abstracted_model_temp_name,
            #                   "2>",progressFileName), wait=T)
            file.remove(c(model_temp_name))
            if(file.exists(abstracted_model_temp_name)) {
                loaded_ss_file$filename <- abstracted_model_temp_name
                loaded_ss_file$data <- readLines(abstracted_model_temp_name)
                cat("abstracted file is loaded\n")
                cat("Approxition is finished\n",file=progressFileName,append=T)
                updateButton(session,"generate_abstraction",style="default",disabled=T)
                updateButton(session,"process_run",style="success",disabled=F)
            } else {
                cat("\nError: some error occured during the approximation process!\n")
                cat("Some error occured during abstraction generation!\n",file=progressFileName,append=T)
            }
        }, message="Model approximation is running...", value=0.5)
    }
})

# observeEvent(input$state_space_file,{
#     if(!is.null(input$state_space_file) && !is.null(input$state_space_file$datapath)) {
#         loaded_ss_file$data <- readLines(input$state_space_file$datapath)
#     } else {
#         # initial example file (temporary)
#         if(!is.null(loaded_vf_file$filename) && loaded_vf_file$filename == paste0(examples_dir,"//model_2D_1P_100R.bio"))
#             loaded_ss_file$data <- paste0(examples_dir,"//model_2D_1P_400R.ss.json")
#         else
#             loaded_ss_file$data <- NULL
#     }
# })
    

#===================== MODEL EXPLORER ==================================    
#=============== LOADING OF FILES ======================================
#=======================================================================
    
loading_vf_file <- reactive({
    biofile <- loaded_vf_file$data
    if(!is.null(biofile)) {
        
        # VARIABLES
        # result is vector of VAR NAMES
        var_line <- biofile[which(str_detect(biofile,"^VARS:"))]
        if(length(var_line) > 0) {
            var_names <- as.list(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",var_line)),",")[[1]] ) )
            names(var_names) <- var_names
        } else return(NULL)
        
        # CONSTANTS
        # result is named list of CONST VALUEs
        const_line <- biofile[which(str_detect(biofile,"^CONSTS:"))]
        if(length(const_line) > 0) {
            #consts <- str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",const_line)),";")[[1]]),",") 
            consts <- lapply(str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",const_line)),";")[[1]]),","),function(x) x[2] )
            names(consts) <- sapply(str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",const_line)),";")[[1]]),","),function(x) x[1] )
        } else consts <- NULL
        
        # PARAMETERS
        # result is names list of pairs PARAM FIRST VALUE and PARAM SECOND VALUE
        param_line <- biofile[which(str_detect(biofile,"^PARAMS:"))]
        if(length(param_line) > 0) {
            #params <- str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",param_line)),";")[[1]]),",") 
            params <- lapply(str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",param_line)),";")[[1]]),","),function(x) c(x[2],x[3]) )
            names(params) <- sapply(str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",param_line)),";")[[1]]),","),function(x) x[1] )
        } else params <- NULL
        
        # THRESHOLDS
        # result is names list of THRES VALUEs
        thr_lines <- gsub("[{}]","_",gsub("[ \t]","",biofile[which(str_detect(biofile,"^THRES:"))]))
        if(length(thr_lines) > 0 && length(thr_lines) == length(var_names)) {
            thres <- sapply(str_split(thr_lines,":"),function(x) str_split(x[3],",") )
            names(thres) <- sapply(str_split(thr_lines,":"),function(x) x[2] )
        } else return(NULL)
        
        # EQUATIONS
        # result is named list of EQUATIONs
        eq_lines <- gsub("^.*:","",gsub("[{}]","_",gsub("[ \t]","",biofile[which(str_detect(biofile,"^EQ:"))])))
        if(length(eq_lines) > 0 && length(eq_lines) == length(var_names)) {
            eqs <- lapply(str_split(eq_lines,"="),function(x) x[2] )
            names(eqs) <- sapply(str_split(eq_lines,"="),function(x) x[1] )
            
            for(e in 1:length(eqs)) {
                eq <- eqs[[e]]
                names <- strsplit(eq,"[-+*,() \t]+")[[1]]
                signs <- strsplit(eq,"[^-+*,() \t]+")[[1]]
                
                names <- sapply(names,function(x) ifelse(!is.null(var_names[[x]]) || !is.null(consts[[x]]) || !is.null(params[[x]]), paste0("ip$",x), x))
                
                new_eq <- ""
                if(names[1] == "") {
                    if(length(names) == length(signs)) {
                        for(i in 1:length(names)) new_eq <- paste0(new_eq,names[i],signs[i])
                    } else {
                        new_eq <- names[1]
                        for(i in 1:min(length(signs),length(names))) new_eq <- paste0(new_eq,signs[i],names[i+1])
                    }
                } else {
                    if(length(names) == length(signs)) {
                        for(i in 1:length(names)) new_eq <- paste0(new_eq,signs[i],names[i])
                    } else {
                        new_eq <- signs[1]
                        for(i in 1:min(length(signs),length(names))) new_eq <- paste0(new_eq,names[i],signs[i+1])
                    }
                }
                eqs[[e]] <- paste0("function(ip) ",new_eq)
            }
        } else return(NULL)
        
        ranges <- lapply(thres, function(x) range(as.numeric(x)))
        
        # setting of GLOBALS
        list_of_names <<- as.list(c(var_names, "Choose"=empty_sign))
        funcs <<- list()
        for(x in unlist(var_names)) {
            funcs[[x]] <<- parse(text=eqs[[x]])
        }
        
        return(list(vars=var_names, consts=consts, params=params, thres=thres, eqs=eqs, ranges=ranges))
    } else return(NULL)
})


# prepared for new .json format
loading_ss_file <- reactive({
    if(!is.null(loaded_ss_file$data)) {
        if(class(loaded_ss_file$data) == "character") {
            biofile <- loaded_ss_file$data
            
            # VARIABLES
            # result is vector of VAR NAMES
            var_line <- biofile[which(str_detect(biofile,"^VARS:"))]
            if(length(var_line) > 0) {
                var_names <- as.list(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",var_line)),",")[[1]] ) )
                names(var_names) <- var_names
            } else return(NULL)
            
            # PARAMETERS
            # result is names list of pairs PARAM FIRST VALUE and PARAM SECOND VALUE
            param_line <- biofile[which(str_detect(biofile,"^PARAMS:"))]
            if(length(param_line) > 0) {
                #params <- str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",param_line)),";")[[1]]),",") 
                params <- lapply(str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",param_line)),";")[[1]]),","),function(x) c(as.numeric(x[2]),as.numeric(x[3])))
                names(params) <- sapply(str_split(gsub("[{}]","_",str_split(gsub("[ \t]","",gsub("^.*:","",param_line)),";")[[1]]),","),function(x) x[1] )
            } else params <- NULL
            
            # THRESHOLDS
            # result is names list of THRES VALUEs
            thr_lines <- gsub("[{}]","_",gsub("[ \t]","",biofile[which(str_detect(biofile,"^THRES:"))]))
            if(length(thr_lines) > 0 && length(thr_lines) == length(var_names)) {
                thres <- sapply(str_split(thr_lines,":"),function(x) str_split(x[3],",") )
                thres <- lapply(thres,as.numeric)
                names(thres) <- sapply(str_split(thr_lines,":"),function(x) x[2] )
            } else return(NULL)
            
            # EQUATIONS
            # result is named list of EQUATIONs
            eq_lines <- gsub("^.*:","",gsub("[{}]","_",gsub("[ \t]","",biofile[which(str_detect(biofile,"^EQ:"))])))
            if(length(eq_lines) > 0 && length(eq_lines) == length(var_names)) {
                eqs <- lapply(str_split(eq_lines,"="),function(x) x[2] )
                names(eqs) <- sapply(str_split(eq_lines,"="),function(x) x[1] )
                
                for(e in 1:length(eqs)) {
                    eq <- eqs[[e]]
                    names <- strsplit(eq,"[-+*,() \t]+")[[1]]
                    signs <- strsplit(eq,"[^-+*,() \t]+")[[1]]
                    
                    names <- sapply(names,function(x) ifelse(!is.null(var_names[[x]]) || !is.null(params[[x]]), paste0("ip$",x), x))
                    
                    new_eq <- ""
                    if(names[1] == "") {
                        if(length(names) == length(signs)) {
                            for(i in 1:length(names)) new_eq <- paste0(new_eq,names[i],signs[i])
                        } else {
                            new_eq <- names[1]
                            for(i in 1:min(length(signs),length(names))) new_eq <- paste0(new_eq,signs[i],names[i+1])
                        }
                    } else {
                        if(length(names) == length(signs)) {
                            for(i in 1:length(names)) new_eq <- paste0(new_eq,signs[i],names[i])
                        } else {
                            new_eq <- signs[1]
                            for(i in 1:min(length(signs),length(names))) new_eq <- paste0(new_eq,names[i],signs[i+1])
                        }
                    }
                    # change of format 'Approx(x)([1,2],..,[3,4])' into 'Approx(x,list(c(1,2),..,c(3,4))'
                    new_eq <- gsub("\\]\\)","\\)\\)\\)",gsub("\\],\\[","\\),c\\(",gsub("\\)\\(\\[",",list\\(c\\(",new_eq)))
                    eqs[[e]] <- paste0("function(ip) ",new_eq)
                }
            } else return(NULL)
            
            ranges <- lapply(thres, function(x) range(as.numeric(x)))
            
            # setting of GLOBALS
            funcs_abst <<- list()
            for(x in unlist(var_names)) {
                funcs_abst[[x]] <<- parse(text=eqs[[x]])
            }
            
            return(list(var_names=var_names, params_num=length(params), param_names=names(params), params=params, thr=thres, eqs=eqs, ranges=ranges))
        } else {
            withProgress(message = paste0('Parsing .json file','...'), value = 1, {
                timing <- system.time(json_data <- fromJSON(file=loaded_ss_file$data))
                cat("-------------------------\n")
                cat("... time of loading file:\n")
                print(timing)
                
                ids <- length(json_data$transitions) # number of rows (all succesors of all states)
                var_num <- length(json_data$variables) # length(loading_vf_file()$var_names)
                param_num <- length(json_data$params$names) # length(loading_vf_file()$params)
                
                thr_data <- json_data$thresholds
                var_names <- json_data$variables
                
                dividers <- 1
                for(i in 2:var_num) dividers <- c(dividers[1]*(length(thr_data[[i]])-1),dividers)
                
                timing <- proc.time()
                udata <- as.data.table(as.matrix(unlist(json_data$transitions)))
                udata$id <- 1:nrow(udata)
                setkeyv(udata,"id")
                cat("... time of parsing json file:\n")
                print(proc.time() - timing)
                
                states <- data.table(id=1:ids)
                timing <- system.time({
                    indices <- udata[(id%%3)==(1), V1]
                    for(i in 1:var_num) {
                        states[[paste0("V",i)]] <- indices%/%dividers[i]
                        indices <- indices%%dividers[i]
                    }
                })
                cat("... time of parsing states:\n")
                print(timing)
                
                succs <- data.table(id=1:ids)
                timing <- system.time({
                    indices <- udata[(id%%3)==(2), V1]
                    for(i in 1:var_num) {
                        succs[[paste0("V",i)]] <- indices%/%dividers[i]
                        indices <- indices%%dividers[i]
                    }
                })
                cat("... time of parsing succs:\n")
                print(timing)
                
                if(param_num != 0) {
                    timing <- system.time({#for(i in 1:(2*np)) params[[paste0("V",i)]] <- udata[(id%%one_part)==((i+2*nv)%%one_part),V1])
                        params <- udata[(id%%3) == 0, as.data.table(t(matrix(unlist(json_data$params$rectangles[V1+1]),nrow=2*param_num))) ]
                    })
                    params[,id:=1:ids]
                    setkeyv(params,"id")
                    cat("... time of parsing params:\n")
                    print(timing)
                    #g_param_slider <<- as.numeric(params[1])[-1]
                    p_num <- (ncol(params)-1)/2
                } else {
                    params <- NULL
                    p_num <- 0
                }
                
                ranges <- lapply(thr_data, range)
                names(ranges) <- json_data$params$names
            })
            
            udata <- NULL
            return(NULL)
            #return(list(thr = thr_data, states = states, succs = succs, params = params, params_num = p_num, var_names = var_names, ranges = ranges))
        }
    } else return(NULL)
})

#=============== SETTING OF WIDGETS =======================================
#==========================================================================

output$param_sliders_bio <- renderUI({
    if(!is.null(loading_vf_file()) && !is.null(loading_vf_file()$params)) {
        lapply(1:length(loading_vf_file()$params), function(i) {
            label <- paste0("parameter ",names(loading_vf_file()$params)[i])
            name <- paste0("param_slider_vf_",i)
            values <- c(min(as.numeric(loading_vf_file()$params[[i]])),max(as.numeric(loading_vf_file()$params[[i]])))
            fluidRow(
                column(12,
            numericInput(name,label=label,min=values[1],max=values[2],value=((values[2]-values[1])*0.1),step=((values[2]-values[1])/1000))
            #,sliderInput(paste0("num_",name),NULL,min=values[1],max=values[2],value=((values[2]-values[1])*0.1),step=((values[2]-values[1])/1000))
                )
            )
        })
    }
    ####### NEW PART ########
#     else {
#         if(!is.null(loading_ss_file()) && !is.null(loading_ss_file()$params)) {
#             lapply(1:loading_ss_file()$params_num, function(i) {
#                 label <- paste0("parameter ",names(loading_vf_file()$params)[i])
#                 name <- paste0("param_slider_vf_",i)
#                 values <- c(min(as.numeric(loading_ss_file()$params[[paste0("V",2*i)]]),na.rm=T),max(as.numeric(loading_ss_file()$params[[paste0("V",2*i+1)]]),na.rm=T))
#                 numericInput(name,label=label,min=values[1],max=values[2],value=((values[2]-values[1])*0.1),step=((values[2]-values[1])/1000))
#             })
#         }
#     }
})
# update_param_sliders <- observe({
#     if(!is.null(loading_vf_file())) {
#         for(i in 1:length(loading_vf_file()$params)) {
#             change <- input[[paste0("param_slider_vf_",i)]]
#             updateSliderInput(session,paste0("num_param_slider_vf_",i),value=change)
#             print("changing slider\n")
#         }
#     }
# })
# update_param_numerics <- observe({
#     if(!is.null(loading_vf_file())) {
#         for(i in 1:length(loading_vf_file()$params)) {
#             change <- input[[paste0("num_param_slider_vf_",i)]]
#             updateNumericInput(session,paste0("param_slider_vf_",i),value=change)
#             print("changing numeric\n")
#         }
#     }
# })
# output$param_sliders <- renderUI({
#     if(!is.null(loading_ss_file()) && !is.null(loading_ss_file()$params)) {
#           lapply(0:(loading_ss_file()$params_num-1), function(i) {
#             label <- paste0("parameter ",names(loading_vf_file()$params)[i+1])
#             name <- paste0("param_slider_",i+1)
#             values <- c(min(as.numeric(loading_ss_file()$params[[paste0("V",2*i+1)]]),na.rm=T),max(as.numeric(loading_ss_file()$params[[paste0("V",2*i+2)]]),na.rm=T))
#             numericInput(name,label=label,min=values[1],max=values[2],value=((values[2]-values[1])*0.1),step=((values[2]-values[1])/1000))
#             #sliderInput(name,label=label,min=values[1],max=values[2],value=c(values[1],values[2]),step=((values[2]-values[1])/1000))
#         })
#     }
# })


output$selector <- renderUI({
#    input$add_vf_plot
    if(!is.null(loading_vf_file())) {
        variables <- loading_vf_file()$vars
        lapply(vf_update(),function(i) {
            idx <- paste0("vf_selector_x_",i)
            labelx <- paste0("horizontal")
            choicesx <- list_of_names
            selectedx <- ifelse(!is.null(input[[paste0("vf_selector_x_",i)]]), input[[paste0("vf_selector_x_",i)]], #empty_sign)
                                variables[[1]])
            
            idy <- paste0("vf_selector_y_",i)
            labely <- paste0("vertical")
            choicesy <- list_of_names
            selectedy <- ifelse(!is.null(input[[paste0("vf_selector_y_",i)]]), input[[paste0("vf_selector_y_",i)]], #empty_sign)
                                ifelse(length(variables) > 1, variables[[2]], variables[[1]]))
            
            fluidRow(
#                     column(2,
#                            helpText(paste0("Plot no. ",i))
#                     ),
                column(5,
                       selectInput(idx, labelx, choicesx, selectedx)
                ),
                column(5,
                       selectInput(idy, labely, choicesy, selectedy)
                ),
                column(2,
                       bsButton(paste0("cancel_vf_",i), "cancel"),
                       checkboxInput(paste0("hide_vf_",i),"hide", ifelse(!is.null(input[[paste0("hide_vf_",i)]]),input[[paste0("hide_vf_",i)]],F))
                       # bsButton(paste0("hide_vf_",i), type="toggle", "hide", value=ifelse(!is.null(input[[paste0("hide_vf_",i)]]),input[[paste0("hide_vf_",i)]],F))
                )
            )
        })
    }
})

# observe({
#     if(!is.null(loading_vf_file())) {
#         for(i in vf_update()) {
#             if(!is.null((input[[paste0("hide_vf_",i)]])))
#                 if(input[[paste0("hide_vf_",i)]])   updateButton(session,paste0("hide_vf_",i), label="show")
#                 else                                updateButton(session,paste0("hide_vf_",i), label="hide")
#         }
#     }
# })
# observer caring for hiding plot if some of selectors is invalidated
observe({
    if(!is.null(loading_vf_file())) {
        for(i in vf_update()) {
            if(!is.null(input[[paste0("vf_selector_x_",i)]]) && input[[paste0("vf_selector_x_",i)]] != empty_sign &&
                   !is.null(input[[paste0("vf_selector_y_",i)]]) && input[[paste0("vf_selector_y_",i)]] != empty_sign)
                   #(length(loading_vf_file()$vars) == 1 || !is.null(input[[paste0("vf_selector_y_",i)]]) && input[[paste0("vf_selector_y_",i)]] != empty_sign))
                next #updateCheckboxInput(session,paste0("hide_vf_",i),value=F)
                # updateButton(session,paste0("hide_vf_",i), disabled=F)
            else
                updateCheckboxInput(session,paste0("hide_vf_",i),value=T)
                # updateButton(session,paste0("hide_vf_",i), disabled=T)
        }
    }
})
# observer caring for showing last plot after selector(s) is(are) properly selected
observe({
    if(!is.null(loading_vf_file())) {
        i <- vf_update()[length(vf_update())]
        if(!is.null(input[[paste0("vf_selector_x_",i)]]) && input[[paste0("vf_selector_x_",i)]] != empty_sign &&
               !is.null(input[[paste0("vf_selector_y_",i)]]) && input[[paste0("vf_selector_y_",i)]] != empty_sign)
               #(length(loading_vf_file()$vars) == 1 || !is.null(input[[paste0("vf_selector_y_",i)]]) && input[[paste0("vf_selector_y_",i)]] != empty_sign))
            updateCheckboxInput(session,paste0("hide_vf_",i),value=F)
            # updateButton(session,paste0("hide_vf_",i), label="hide", value=F)
    }
})
# observer caring for canceling a plot
observe({
    if(!is.null(loading_vf_file())) {
        for(i in vf_chosen$data) {
            if(!is.null(input[[paste0("cancel_vf_",i)]]) && input[[paste0("cancel_vf_",i)]] > 0) {
                vf_chosen$data <- vf_chosen$data[vf_chosen$data != i]
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
                vf_brushed$data[[i]] <- NA
                vf_brushed$click_counter[[i]] <- NA
                
                print(gc())
            }
        }
    }
})
# observer providing new selector(s) and cancel button and hide checkbox after 'add plot' button is clicked
observeEvent(input$add_vf_plot,{
    if(!is.null(loading_vf_file())) {
        # very important initialisation of unzoom button click counters
        vf_brushed$click_counter[[vf_chosen$max]] <- -1
        ss_brushed$click_counter[[vf_chosen$max]] <- -1
        vector_field_clicked$old_point[[vf_chosen$max]] <- NA
        vector_field_clicked$click_counter[[vf_chosen$max]] <- -1
        vector_field_clicked$apply_to_all_click_counter[[vf_chosen$max]] <- -1
        vector_field_clicked$apply_to_tss_click_counter[[vf_chosen$max]] <- -1
        state_space_clicked$old_point[[vf_chosen$max]] <- NA
        state_space_clicked$click_counter[[vf_chosen$max]] <- -1
        state_space_clicked$apply_to_all_click_counter[[vf_chosen$max]] <- -1
        vector_field_space$globals[[vf_chosen$max]] <- NA
        transition_state_space$globals[[vf_chosen$max]] <- NA
        
        vf_chosen$data <- c(vf_chosen$data,vf_chosen$max)
        vf_chosen$max  <- vf_chosen$max + 1
    }
})
vf_update <- reactive({
    return(vf_chosen$data)
})
visible_vf_plots <- reactive({
    if(!is.null(loading_vf_file())) {
        local_result <- c()
        for(i in vf_update()) {
            if(!is.null(input[[paste0("vf_selector_x_",i)]]) && input[[paste0("vf_selector_x_",i)]] != empty_sign && !input[[paste0("hide_vf_",i)]] &&
                   !is.null(input[[paste0("vf_selector_y_",i)]]) && input[[paste0("vf_selector_y_",i)]] != empty_sign) {
                   #(length(loading_vf_file()$vars) == 1 || !is.null(input[[paste0("vf_selector_y_",i)]]) && input[[paste0("vf_selector_y_",i)]] != empty_sign))
                local_result <- c(local_result,i)
                # updateButton(session,paste0("hide_vf_",i), label="hide", disabled=F)
            }
        }
        return(local_result)
    } else return(NULL)
})


output$plots <- renderUI({
#    input$add_vf_plot
    if(!is.null(loading_vf_file())) {
        one_line <- lapply(visible_vf_plots(), function(i) {
            fluidRow(
                column(2,
                       if(!is.null(loading_vf_file())) {
                           #downloadButton(paste0("download_vf_",i),"Download image"),
                           actionButton(paste0("apply_plot_vf_",i),"Apply to all")
                       },
                       if(!is.null(loading_vf_file()) && !is.null(loading_ss_file())) {
                           actionButton(paste0("apply_to_tss_vf_",i),"Apply to TSS")
                       },
                       if(!is.null(loading_vf_file())) {
                           actionButton(paste0("clear_plot_vf_",i),"Clear plot")
                       },
                       if(!is.null(loading_vf_file())) {
                           actionButton(paste0("unzoom_plot_vf_",i),"Unzoom")
                       },
                       if(!is.null(loading_vf_file()) && !is.null(loading_ss_file())) {
                           conditionalPanel(
                               condition = "input.advanced == true",
                               checkboxInput(paste0("abst_vf_",i),"use PWA model",F)
                           )
                       },
                       if(!is.null(loading_vf_file()) && length(loading_vf_file()$vars) > 2) {
                           lapply(1:length(loading_vf_file()$vars), function(t) {
                               if(!loading_vf_file()$vars[[t]] %in% c(input[[paste0("vf_selector_x_",i)]],input[[paste0("vf_selector_y_",i)]])) {
                                   label <- paste0("continues scale in ",loading_vf_file()$vars[[t]])
                                   name <- paste0("scale_slider_vf_",i,"_",t)
                                   values <- range(as.numeric(loading_vf_file()$thres[[loading_vf_file()$vars[[t]]]]))
                                   sliderInput(name,label=label,min=values[1],max=values[2],step=zoom_granul,
                                               value=ifelse(is.null(input[[name]]),values[1],input[[name]]))
                               }
                           })
                       },
                       if(!is.null(loading_vf_file())) {
                           verbatimTextOutput(paste0("hover_text_vf_",i))
                       }
                ),
                column(4,
                       if(!is.null(loading_vf_file()))
                           helpText("vector field of the model"),
                       if(!is.null(loading_vf_file()))
                           imageOutput(paste0("plot_vf_",i),"auto","auto",click=paste0("vf_",i,"_click"),dblclick=paste0("vf_",i,"_dblclick"),
                                       brush=brushOpts(id=paste0("vf_",i,"_brush"),delayType="debounce",delay=brush_delay_limit,resetOnNew=T),
                                       hover=hoverOpts(id=paste0("vf_",i,"_hover"),delayType="debounce",delay=hover_delay_limit))
                ),
                column(4,
                       if(!is.null(loading_ss_file()))
                           helpText("transition-state space of the model")
                       else
                           h3("Approximation have to be generated before showing transition-state space"),
                       if(!is.null(loading_ss_file()))
                           imageOutput(paste0("plot_ss_",i),"auto","auto",click=paste0("ss_",i,"_click"),dblclick=paste0("ss_",i,"_dblclick"),
                                       hover=hoverOpts(id=paste0("ss_",i,"_hover"),delayType="debounce",delay=hover_delay_limit),
                                       brush=brushOpts(id=paste0("ss_",i,"_brush"),delayType="debounce",delay=brush_delay_limit,resetOnNew=T))
                ),
                column(2,
                       if(!is.null(loading_ss_file())) {
                           #downloadButton(paste0("download_ss_",i),"Download image")
                           actionButton(paste0("apply_plot_ss_",i),"Apply to all")
                       },
                       if(!is.null(loading_ss_file())) {
                           actionButton(paste0("clear_plot_ss_",i),"Clear plot")
                       },
                       if(!is.null(loading_ss_file())) {
                           actionButton(paste0("unzoom_plot_ss_",i),"Unzoom")
                       },
                       #                        if(!is.null(loading_ss_file())) {
                       #                            checkboxInput(paste0("log_scale_ss_",i),"Log-scale")
                       #                        },
                       if(!is.null(loading_ss_file()) && length(loading_ss_file()$var_names) > 2) {
                           lapply(1:length(loading_ss_file()$var_names), function(t) {
                               if(!loading_ss_file()$var_names[[t]] %in% c(input[[paste0("vf_selector_x_",i)]],input[[paste0("vf_selector_y_",i)]])) {
                                   label <- paste0("discrete scale in ",loading_ss_file()$var_names[[t]])
                                   name <- paste0("scale_slider_ss_",i,"_",t)
                                   values <- c(1,ifelse(max(loading_ss_file()$thr[[t]]) > loading_vf_file()$ranges[[t]][2],
                                                        length(loading_ss_file()$thr[[t]][which(loading_ss_file()$thr[[t]] <= loading_vf_file()$ranges[[t]][2])]),
                                                        length(loading_ss_file()$thr[[t]])-1))
                                   sliderInput(name,label=label,min=values[1],max=values[2],step=1,
                                               value=ifelse(is.null(input[[name]]),values[1],input[[name]]))
                               }
                           })
                       },
                       if(!is.null(loading_ss_file())) {
                           verbatimTextOutput(paste0("hover_text_ss_",i))
                       }
                )
            )
        })
        do.call(tagList, one_line)
    }
})

#=============== SETTING OF INTERNAL FUNCTIONS =========================
#=======================================================================

# observers providing zooming and unzooming of vector-field plots
zoom_vf_ranges <- observe({
    #arrows_number <- input$arrows_number
    if(!is.null(loading_vf_file()) ) { # && input$add_vf_plot > 0) {
        for(i in visible_vf_plots()) {
            if(!is.null(input[[paste0("vf_",i,"_brush")]])) isolate({
                brush <- input[[paste0("vf_",i,"_brush")]]
                cat("brush in vf ",i,": ",brush$xmin,",",brush$xmax,",",brush$ymin,",",brush$ymax,"\n")
                vf_brushed$data[[i]] <- lapply(loading_vf_file()$vars, function(x) {
                    if(x == input[[paste0("vf_selector_x_",i)]]) return(c(brush$xmin, brush$xmax))
                    if(x == input[[paste0("vf_selector_y_",i)]]) return(c(brush$ymin, brush$ymax))
                    return(loading_vf_file()$ranges[[x]])
                })
#                 vf_brushed$data[[i]] <- lapply(loading_vf_file()$vars, function(x) {
#                     if(x==input[[paste0("vf_selector_y_",i)]] || x==input[[paste0("vf_selector_x_",i)]]) {
#                         if(x==input[[paste0("vf_selector_x_",i)]]) seq(brush$xmin, brush$xmax, (brush$xmax-brush$xmin)/arrows_number)
#                         else seq(brush$ymin, brush$ymax, (brush$ymax-brush$ymin)/arrows_number)
#                     } else seq(loading_vf_file()$ranges[[x]][1], loading_vf_file()$ranges[[x]][2], 
#                                (loading_vf_file()$ranges[[x]][2]-loading_vf_file()$ranges[[x]][1])/arrows_number)
#                 })
            })
        }
    }
})
unzoom_vf_ranges <- observe({
    #arrows_number <- input$arrows_number
    if(!is.null(loading_vf_file()) ) { # && input$add_vf_plot > 0) {
        for(i in visible_vf_plots()) {
            button <- input[[paste0("unzoom_plot_vf_",i)]]
            if(!is.null(button) && (button > vf_brushed$click_counter[[i]])) isolate({
                vf_brushed$data[[i]] <- lapply(loading_vf_file()$vars, function(x) loading_vf_file()$ranges[[x]] )
#                 vf_brushed$data[[i]] <- lapply(loading_vf_file()$vars, function(x) {
#                     seq(loading_vf_file()$ranges[[x]][1], loading_vf_file()$ranges[[x]][2], 
#                         (loading_vf_file()$ranges[[x]][2]-loading_vf_file()$ranges[[x]][1])/arrows_number)
#                 })
                vf_brushed$click_counter[[i]] <- button
            })
        }
    }
})

# observers providing zooming and unzooming of state-space plots
zoom_ss_ranges <- observe({
    if(!is.null(loading_ss_file()) ) { # && input$add_vf_plot > 0) {
        for(i in visible_vf_plots()) {
            if(!is.null(input[[paste0("ss_",i,"_brush")]])) isolate({
                brush <- input[[paste0("ss_",i,"_brush")]]
                cat("brush in ss ",i,": ",brush$xmin,",",brush$xmax,",",brush$ymin,",",brush$ymax,"\n")
                ss_brushed$data[[i]] <- lapply(loading_ss_file()$var_names, function(x) {
                    if(x==input[[paste0("vf_selector_y_",i)]] || x==input[[paste0("vf_selector_x_",i)]]) {
                        if(x==input[[paste0("vf_selector_x_",i)]]) c(brush$xmin,brush$xmax)
                        else c(brush$ymin,brush$ymax)
                    } else loading_ss_file()$ranges[[x]]
                })
            })
        }
    }
})
unzoom_ss_ranges <- observe({
    if(!is.null(loading_ss_file()) ) { # && input$add_vf_plot > 0) {
        for(i in visible_vf_plots()) {
            button <- input[[paste0("unzoom_plot_ss_",i)]]
            if(!is.null(button) && (button > ss_brushed$click_counter[[i]])) isolate({
                ss_brushed$data[[i]] <- loading_ss_file()$ranges
                ss_brushed$click_counter[[i]] <- button
            })
        }
    }
})

set_input_params <- reactive({
    if(!is.null(loading_vf_file())) {
        inpar <- "list("
        if(!is.null(loading_vf_file()$consts))
            for(i in 1:length(loading_vf_file()$consts)) {
                inpar <- paste0(inpar,names(loading_vf_file()$consts)[i],"=",loading_vf_file()$consts[[i]],",")
            }
        if(!is.null(loading_vf_file()$params))
            for(i in 1:length(loading_vf_file()$params)) {
                inpar <- paste0(inpar,names(loading_vf_file()$params)[i],"=",ifelse(is.null(input[[paste0("param_slider_vf_",i)]]) | 
                                                                                     is.na(input[[paste0("param_slider_vf_",i)]]),
                                                                                     no_param_const, input[[paste0("param_slider_vf_",i)]]),",")
            }
        return(inpar)
    } else return(NULL)
})
set_abst_input_params <- reactive({
    if(!is.null(loading_ss_file())) {
        inpar <- "list("
#         if(!is.null(loading_vf_file()$consts))
#             for(i in 1:length(loading_vf_file()$consts)) {
#                 inpar <- paste0(inpar,names(loading_vf_file()$consts)[i],"=",loading_vf_file()$consts[[i]],",")
#             }
        if(!is.null(loading_ss_file()$params))
            for(i in 1:length(loading_ss_file()$params)) {
                inpar <- paste0(inpar,names(loading_ss_file()$params)[i],"=",ifelse(is.null(input[[paste0("param_slider_vf_",i)]]) | 
                                                                                        is.na(input[[paste0("param_slider_vf_",i)]]),
                                                                                    no_param_const, input[[paste0("param_slider_vf_",i)]]),",")
            }
        return(inpar)
    } else return(NULL)
})


# tohto by som sa rad zbavil (temporary)
change_height <- reactive({
    #input$execute_zoom
    #isolate(return(input$height))
    return(650)
})


clicked_in_vf <- observe({
    if(!is.null(loading_vf_file()) ) { # && input$add_vf_plot > 0) {
        variables <- loading_vf_file()$vars
        for(i in visible_vf_plots()) {
            clicked_point <- input[[paste0("vf_",i,"_dblclick")]]
            if(!is.null(clicked_point) ) isolate({ #&& is.null(input[[paste0("vf_",i,"_brush")]])) {
                cat("drawing curve in vf ",i,": ",clicked_point$x,",",clicked_point$y,"\n")
                vector_field_clicked$point[[i]] <- sapply(1:length(variables), function(t) {
                    if(variables[t] == input[[paste0("vf_selector_x_",i)]]) return(clicked_point$x)
                    if(variables[t] == input[[paste0("vf_selector_y_",i)]]) return(clicked_point$y)
                    return(input[[paste0("scale_slider_vf_",i,"_",t)]])
                })
                names(vector_field_clicked$point[[i]]) <- variables
            })
        }
    }
})
erase_in_vector_field <- observe({
    if(!is.null(loading_vf_file()) ) { # && input$add_vf_plot > 0) {
        for(i in visible_vf_plots()) {
            button <- input[[paste0("clear_plot_vf_",i)]]
            if(!is.null(button) && (button > vector_field_clicked$click_counter[[i]])) isolate({
                vector_field_clicked$point[[i]] <- NA
                vector_field_clicked$click_counter[[i]] <- button
            })
        }
    }
})
apply_to_all_in_vf <- observe({
    if(!is.null(loading_vf_file()) ) { # && input$add_vf_plot > 0) {
        for(i in visible_vf_plots()) {
            button <- input[[paste0("apply_plot_vf_",i)]]
            if(!is.null(button) && (button > vector_field_clicked$apply_to_all_click_counter[[i]])) isolate({
                if(length(vector_field_clicked$point) >= i && !is.null(vector_field_clicked$point[[i]]) && !is.na(vector_field_clicked$point[[i]])) {
                    for(x in visible_vf_plots()) vector_field_clicked$point[[x]] <- vector_field_clicked$point[[i]]
                }
                vector_field_clicked$apply_to_all_click_counter[[i]] <- button
            })
        }
    }
})
apply_to_tss_in_vf <- observe({
    if(!is.null(loading_vf_file()) ) {
        variables <- loading_vf_file()$vars
        for(i in visible_vf_plots()) {
            button <- input[[paste0("apply_to_tss_vf_",i)]]
            if(!is.null(button) && (button > vector_field_clicked$apply_to_tss_click_counter[[i]])) isolate({
                if(length(vector_field_clicked$point) >= i && !is.null(vector_field_clicked$point[[i]]) && !is.na(vector_field_clicked$point[[i]])) {
                    state_space_clicked$point[[i]] <- vector_field_clicked$point[[i]]
                }
                vector_field_clicked$apply_to_tss_click_counter[[i]] <- button
            })
        }
    }
})


clicked_in_state_space <- observe({
    if(!is.null(loading_ss_file()) ) {
        variables <- loading_ss_file()$var_names
        for(i in visible_vf_plots()) {
            clicked <- input[[paste0("ss_",i,"_dblclick")]]
            if(!is.null(clicked) ) isolate({
                cat("drawing reachable in ss ",i,": ",clicked$x,",",clicked$y,"\n")
                state_space_clicked$point[[i]] <- sapply(1:length(variables), function(t) {
                    if(variables[t] == input[[paste0("vf_selector_x_",i)]]) return(clicked$x)
                    if(variables[t] == input[[paste0("vf_selector_y_",i)]]) return(clicked$y)
                    return(loading_ss_file()$thr[[input[[paste0("scale_slider_ss_",i,"_",t)]] ]])
                })
                names(state_space_clicked$point[[i]]) <- variables
            })
        }
    }
})
erase_in_state_space <- observe({
    if(!is.null(loading_ss_file()) ) {
        for(i in visible_vf_plots()) {
            button <- input[[paste0("clear_plot_ss_",i)]]
            if(!is.null(button) && (button > state_space_clicked$click_counter[[i]])) isolate({
                state_space_clicked$point[[i]] <- NA
                state_space_clicked$click_counter[[i]] <- button
            })
        }
    }
})
apply_to_all_in_state_space <- observe({
    if(!is.null(loading_ss_file()) ) { # && input$add_vf_plot > 0) {
        for(i in visible_vf_plots()) {
            button <- input[[paste0("apply_plot_ss_",i)]]
            if(!is.null(button) && (button > state_space_clicked$apply_to_all_click_counter[[i]])) isolate({
                if(length(state_space_clicked$point) >= i && !is.null(state_space_clicked$point[[i]]) && !is.na(state_space_clicked$point[[i]])) {
                    for(x in visible_vf_plots()) state_space_clicked$point[[x]] <- state_space_clicked$point[[i]]
                }
                state_space_clicked$apply_to_all_click_counter[[i]] <- button
            })
        }
    }
})

#====================== OUTPUTS =========================================
#========================================================================

# velmi podobne spracovat aj downloadHandlery

draw_vf_plots <- observe({
    if(!is.null(loading_vf_file())) {
        for (i in visible_vf_plots()) {
            local({
                my_i <- i
                plotname <- paste0("plot_vf_",my_i)
                if(input[[paste0("vf_selector_x_",my_i)]] != input[[paste0("vf_selector_y_",my_i)]]) {
                #if(length(loading_vf_file()$vars) > 1) {
                    output[[plotname]] <- renderPlot({ 
                        draw_vector_field(isolate(input[[paste0("vf_selector_x_",my_i)]]), isolate(input[[paste0("vf_selector_y_",my_i)]]), my_i, vf_brushed$data[[my_i]])
                    },height=change_height() # input$height
                    )
                } else {
                    output[[plotname]] <- renderPlot({
                        draw_1D_vector_field(isolate(input[[paste0("vf_selector_x_",my_i)]]), my_i, vf_brushed$data[[my_i]])
                    },height=change_height() # input$height
                    )
                }
            })
        }
    }
})
hover_over_vf_plots <- observe({
    if(!is.null(loading_vf_file())) {
        for (i in visible_vf_plots()) {
            local({
                my_i <- i
                hover <- input[[paste0("vf_",my_i,"_hover")]]
                list_of_all_names <- loading_vf_file()$vars
                output[[paste0("hover_text_vf_",my_i)]] <- renderPrint({
                    cat(paste0(
                        sapply(1:length(list_of_all_names), function(x) {
                            name <- list_of_all_names[[x]]
                            if(!name %in% c(input[[paste0("vf_selector_x_",my_i)]],input[[paste0("vf_selector_y_",my_i)]])) {
                                paste0(name,": ",round(input[[paste0("scale_slider_vf_",my_i,"_",x)]],rounding_in_hover) )
                            } else {
                                ifelse(is.null(hover), paste0(name,": "),
                                       ifelse(name %in% input[[paste0("vf_selector_x_",my_i)]], paste0(name,": ",round(hover$x,rounding_in_hover)),
                                              paste0(name,": ",round(hover$y,rounding_in_hover)) ))
                            }
                        }), collapse="\n"))
                },width="100")
            })
        }
    }
})

draw_ss_plots <- observe({
    if(!is.null(loading_ss_file())) {
        for (i in visible_vf_plots()) {
            local({
                my_i <- i
                plotname <- paste0("plot_ss_",my_i)
                if(input[[paste0("vf_selector_x_",my_i)]] != input[[paste0("vf_selector_y_",my_i)]]) {
                #if(length(loading_ss_file()$var_names) > 1) {
                    output[[plotname]] <- renderPlot({
                        #draw_vector_field_2(isolate(input[[paste0("vf_selector_x_",my_i)]]), isolate(input[[paste0("vf_selector_y_",my_i)]]), my_i, vf_brushed$data[[my_i]])
                        draw_state_space_new_2(isolate(input[[paste0("vf_selector_x_",my_i)]]), isolate(input[[paste0("vf_selector_y_",my_i)]]), my_i, ss_brushed$data[[my_i]])
#                        draw_state_space(isolate(input[[paste0("vf_selector_x_",my_i)]]), isolate(input[[paste0("vf_selector_y_",my_i)]]), my_i, ss_brushed$data[[my_i]])
                    },height=change_height() )
                } else {
                    output[[plotname]] <- renderPlot({
                        draw_1D_state_space(isolate(input[[paste0("vf_selector_x_",my_i)]]), my_i, ss_brushed$data[[my_i]])
                    },height=change_height() )
                }
            })
        }
    }
})
hover_over_ss_plots <- observe({
    if(!is.null(loading_ss_file())) {
        for (i in visible_vf_plots()) {
            local({
                my_i <- i
                hover <- input[[paste0("ss_",my_i,"_hover")]]
                list_of_all_names <- loading_ss_file()$var_names
                output[[paste0("hover_text_ss_",my_i)]] <- renderPrint({
                    cat(paste0(
                        sapply(1:length(list_of_all_names), function(x) {
                            name <- list_of_all_names[[x]]
                            if(!name %in% c(input[[paste0("vf_selector_x_",my_i)]],input[[paste0("vf_selector_y_",my_i)]])) {
                                paste0(name,": ",round(loading_ss_file()$thr[[name]][input[[paste0("scale_slider_ss_",my_i,"_",x)]]  ],rounding_in_hover)," - ",
                                       round(loading_ss_file()$thr[[name]][input[[paste0("scale_slider_ss_",my_i,"_",x)]]+1],rounding_in_hover),
                                       " : layer (",input[[paste0("scale_slider_ss_",my_i,"_",x)]],")")
                            } else {
                                ifelse(is.null(hover), paste0(name,": "),
                                       ifelse(name %in% input[[paste0("vf_selector_x_",my_i)]], paste0(name,": ",round(hover$x,rounding_in_hover)),
                                              paste0(name,": ",round(hover$y,rounding_in_hover)) ))
                            }
                        }), collapse="\n"))
                },width="100")
            })
        }
    }
})

draw_vector_field <- function(name_x, name_y, plot_index, boundaries) {
    variables <- loading_vf_file()$vars
    index_x <- match(name_x,variables)
    index_y <- match(name_y,variables)
    
    # create current set of globals
    checkpoint <- list(selectors =list(x=input[[paste0("vf_selector_x_",plot_index)]], y=input[[paste0("vf_selector_y_",plot_index)]]),
                       boundaries=boundaries,
                       arrows_number=input$arrows_number,
                       color_variant=input$colVariant,
                       abst_model=input[[paste0("abst_vf_",plot_index)]],
                       counter=input$accept_model_changes,
                       parameters=list(),
                       sliders=list() )
    if(!is.null(loading_vf_file()$params)) {
        for(p in 1:length(loading_vf_file()$params))    checkpoint$parameters[[names(loading_vf_file()$params)[p] ]] <- input[[paste0("param_slider_vf_",p)]]
    }
    if(length(variables) > 2) {
        for(t in 1:length(variables)) {
            if(!variables[[t]] %in% c(input[[paste0("vf_selector_x_",plot_index)]],input[[paste0("vf_selector_y_",plot_index)]] )) {
                checkpoint$sliders[[variables[[t]] ]] <- input[[paste0("scale_slider_vf_",plot_index,"_",t)]]
            }
        }
    }
    # check for any change in globals for particular plot
    if(is.na(vector_field_space$globals[[plot_index]]) || !identical(vector_field_space$globals[[plot_index]],checkpoint)) {
        mesh <- meshgrid(seq(boundaries[[name_x]][1],boundaries[[name_x]][2],(boundaries[[name_x]][2]-boundaries[[name_x]][1])/input$arrows_number),
                         seq(boundaries[[name_y]][1],boundaries[[name_y]][2],(boundaries[[name_y]][2]-boundaries[[name_y]][1])/input$arrows_number))
        # drawing of vector field =================================================
        input_params <- ifelse(!is.null(input[[paste0("abst_vf_",plot_index)]]) && input[[paste0("abst_vf_",plot_index)]], set_abst_input_params(), set_input_params())
        for(i in 1:length(variables)) {
            if(variables[[i]] == name_x || variables[[i]] == name_y) {
                if(variables[[i]] == name_x)  input_params <- paste0(input_params,variables[[i]],"=","mesh$X",",")
                else                                        input_params <- paste0(input_params,variables[[i]],"=","mesh$Y",",")
            } else input_params <- paste0(input_params,variables[[i]],"=",input[[paste0("scale_slider_vf_",plot_index,"_",i)]],",")
        }
        substr(input_params,nchar(input_params),nchar(input_params)) <- ")"
        
        if(!is.null(input[[paste0("abst_vf_",plot_index)]]) && input[[paste0("abst_vf_",plot_index)]]) {
            fx = eval(funcs_abst[[name_x]])(eval(parse(text=input_params)))
            fy = eval(funcs_abst[[name_y]])(eval(parse(text=input_params)))
        } else {
            fx = eval(funcs[[name_x]])(eval(parse(text=input_params)))
            fy = eval(funcs[[name_y]])(eval(parse(text=input_params)))
        }
    
        direction <- switch(input$colVariant,
                            "both" = fx + fy,
                            "horizontal" = fx,
                            "vertical" = fy,
                            "none" = 0)
        vector_field_space$data[[plot_index]] <- list(mesh=mesh,
                                                      fx=fx,
                                                      fy=fy,
                                                      direction=direction)
    }
    data <- vector_field_space$data[[plot_index]]
    plot(range(boundaries[[name_x]]), range(boundaries[[name_y]]), type="n", xlab=name_x, ylab=name_y, xaxs="i", yaxs="i")
    suppressWarnings(quiver(data$mesh$X, data$mesh$Y, data$fx, data$fy,
                            scale=input$arrowSize, length=0.08, angle=30, lwd=input$transWidth,
                            col=ifelse(data$direction > input$colThr, positive_color, ifelse(data$direction < -input$colThr, negative_color, neutral_color))))
    
    #============== drawing of flow =================================================
    if(length(vector_field_clicked$point) >= plot_index && !(is.null(vector_field_clicked$point[[plot_index]]) || is.na(vector_field_clicked$point[[plot_index]]))) {
        point <- vector_field_clicked$point[[plot_index]]
        if(is.na(vector_field_clicked$old_point[[plot_index]]) || !identical(vector_field_clicked$old_point[[plot_index]],point) ||
               !identical(vector_field_space$globals[[plot_index]],checkpoint)) {
            flow_data <- matrix(,nrow=num_of_flow_points+1,ncol=length(variables))
            flow_data[1,] <- point
            for(r in seq(num_of_flow_points)) {
                input_params <- ifelse(!is.null(input[[paste0("abst_vf_",plot_index)]]) && input[[paste0("abst_vf_",plot_index)]], set_abst_input_params(), set_input_params())
                for(i in 1:length(variables)) {
                    input_params <- paste0(input_params,variables[i],"=",flow_data[r,i],",")
                }
                substr(input_params,nchar(input_params),nchar(input_params)) <- ")"
                new_move <- sapply(1:length(variables), function(i) {
                    if(!is.null(input[[paste0("abst_vf_",plot_index)]]) && input[[paste0("abst_vf_",plot_index)]])
                        eval(funcs_abst[[i]])(eval(parse(text=input_params)))
                    else
                        eval(funcs[[i]])(eval(parse(text=input_params)))
                    })
                flow_data[r+1,] <- flow_data[r,]+new_move
            }
            vector_field_clicked$data[[plot_index]] <- flow_data
        }
        flow_data <- vector_field_clicked$data[[plot_index]]
        xspline(flow_data[,index_x], flow_data[,index_y], shape=1, col="blue", border="blue", lwd=size_of_flow_points)
        # it has to be at the end
        vector_field_clicked$old_point[[plot_index]] <- point
    }
    # it has to be at the end
    vector_field_space$globals[[plot_index]] <- checkpoint
}
# draw_vector_field_2 <- function(name_x, name_y, plot_index, boundaries) {
#     variables <- loading_vf_file()$vars
#     index_x <- match(name_x,variables)
#     index_y <- match(name_y,variables)
#     
#     # create current set of globals
#     checkpoint <- list(selectors =list(x=input[[paste0("vf_selector_x_",plot_index)]], y=input[[paste0("vf_selector_y_",plot_index)]]),
#                        boundaries=boundaries,
#                        arrows_number=input$arrows_number,
#                        parameters=list(),
#                        sliders=list() )
#     if(!is.null(loading_vf_file()$params)) {
#         for(p in 1:length(loading_vf_file()$params))    checkpoint$parameters[[names(loading_vf_file()$params)[p] ]] <- input[[paste0("param_slider_vf_",p)]]
#     }
#     if(length(variables) > 2) {
#         for(t in 1:length(variables)) {
#             if(!variables[[t]] %in% c(input[[paste0("vf_selector_x_",plot_index)]],input[[paste0("vf_selector_y_",plot_index)]] )) {
#                 checkpoint$sliders[[variables[[t]] ]] <- input[[paste0("scale_slider_vf_",plot_index,"_",t)]]
#             }
#         }
#     }
#     # check for any change in globals for particular plot
#     if(is.na(vector_field_space$globals[[plot_index]]) || !identical(vector_field_space$globals[[plot_index]],checkpoint)) {
#         mesh <- meshgrid(seq(boundaries[[name_x]][1],boundaries[[name_x]][2],(boundaries[[name_x]][2]-boundaries[[name_x]][1])/input$arrows_number),
#                          seq(boundaries[[name_y]][1],boundaries[[name_y]][2],(boundaries[[name_y]][2]-boundaries[[name_y]][1])/input$arrows_number))
#         # drawing of vector field =================================================
#         input_params <- set_abst_input_params()
#         for(i in 1:length(variables)) {
#             if(variables[[i]] == name_x || variables[[i]] == name_y) {
#                 if(variables[[i]] == name_x)  input_params <- paste0(input_params,variables[[i]],"=","mesh$X",",")
#                 else                                        input_params <- paste0(input_params,variables[[i]],"=","mesh$Y",",")
#             } else input_params <- paste0(input_params,variables[[i]],"=",input[[paste0("scale_slider_vf_",plot_index,"_",i)]],",")
#         }
#         substr(input_params,nchar(input_params),nchar(input_params)) <- ")"
#         
#         fx = eval(funcs_abst[[name_x]])(eval(parse(text=input_params)))
#         fy = eval(funcs_abst[[name_y]])(eval(parse(text=input_params)))
#         
#         direction <- switch(input$colVariant,
#                             "both" = fx + fy,
#                             "horizontal" = fx,
#                             "vertical" = fy,
#                             "none" = 0)
#         vector_field_space$data[[plot_index]] <- list(mesh=mesh,
#                                                       fx=fx,
#                                                       fy=fy,
#                                                       direction=direction)
#         cat(paste0("computing vector-field for ",plot_index,"\n"))
#     }
#     data <- vector_field_space$data[[plot_index]]
#     plot(range(boundaries[[name_x]]), range(boundaries[[name_y]]), type="n", xlab=name_x, ylab=name_y, xaxs="i", yaxs="i")
#     suppressWarnings(quiver(data$mesh$X, data$mesh$Y, data$fx, data$fy,
#                             scale=input$arrowSize, length=0.08, angle=30, lwd=input$transWidth,
#                             col=ifelse(data$direction > input$colThr, positive_color, ifelse(data$direction < -input$colThr, negative_color, neutral_color))))
#     
#     #============== drawing of flow =================================================
#     if(length(vector_field_clicked$point) >= plot_index && !(is.null(vector_field_clicked$point[[plot_index]]) || is.na(vector_field_clicked$point[[plot_index]]))) {
#         point <- vector_field_clicked$point[[plot_index]]
#         if(is.na(vector_field_clicked$old_point[[plot_index]]) || !identical(vector_field_clicked$old_point[[plot_index]],point) ||
#                !identical(vector_field_space$globals[[plot_index]],checkpoint)) {
#             flow_data <- matrix(,nrow=num_of_flow_points+1,ncol=length(variables))
#             flow_data[1,] <- point
#             for(r in seq(num_of_flow_points)) {
#                 input_params <- set_abst_input_params()
#                 for(i in 1:length(variables)) {
#                     input_params <- paste0(input_params,variables[i],"=",flow_data[r,i],",")
#                 }
#                 substr(input_params,nchar(input_params),nchar(input_params)) <- ")"
#                 new_move <- sapply(1:length(variables), function(i) eval(funcs_abst[[i]])(eval(parse(text=input_params))))
#                 flow_data[r+1,] <- flow_data[r,]+new_move
#             }
#             vector_field_clicked$data[[plot_index]] <- flow_data
#         }
#         flow_data <- vector_field_clicked$data[[plot_index]]
#         xspline(flow_data[,index_x], flow_data[,index_y], shape=1, col="blue", border="blue", lwd=size_of_flow_points)
#         # it has to be at the end
#         vector_field_clicked$old_point[[plot_index]] <- point
#     }
#     # it has to be at the end
#     vector_field_space$globals[[plot_index]] <- checkpoint
# }
draw_1D_vector_field <- function(name_x, plot_index, boundaries) {
    variables <- loading_vf_file()$vars
    index_x <- match(name_x,variables)
    plot(range(boundaries[[name_x]]), c(0,1), type="n", xlab=name_x, ylab="", yaxt="n")
    
    #TODO:
}


draw_state_space_new_2 <- function(name_x, name_y, plot_index, boundaries) {
    variables <- loading_ss_file()$var_names
    index_x <- match(name_x,variables)
    index_y <- match(name_y,variables)
    range_x <- loading_ss_file()$ranges[[index_x]]
    range_y <- loading_ss_file()$ranges[[index_y]]
    thres_x <- loading_ss_file()$thr[[index_x]]
    thres_y <- loading_ss_file()$thr[[index_y]]
    
    if(!is.null(loading_ss_file())) {
        # create current set of globals
        checkpoint <- list(selectors =list(x=input[[paste0("vf_selector_x_",plot_index)]], y=input[[paste0("vf_selector_y_",plot_index)]]),
                           parameters=list(),
                           counter=input$accept_model_changes,
                           sliders=list() )
        if(!is.null(loading_vf_file()$params)) {
            for(p in 1:length(loading_vf_file()$params))    checkpoint$parameters[[names(loading_vf_file()$params)[p] ]] <- input[[paste0("param_slider_vf_",p)]]
        }
        if(length(variables) > 2) {
            for(t in 1:length(variables)) {
                if(!variables[[t]] %in% c(input[[paste0("vf_selector_x_",plot_index)]],input[[paste0("vf_selector_y_",plot_index)]] )) {
                    checkpoint$sliders[[variables[[t]] ]] <- input[[paste0("scale_slider_ss_",plot_index,"_",t)]]
                }
            }
        }
        # check for any change in globals for particular plot
        if(is.na(transition_state_space$globals[[plot_index]]) || !identical(transition_state_space$globals[[plot_index]],checkpoint)) {
            timing <- system.time(withProgress({
                
                dim_indices <- letters[-which(letters %in% c("x","y"))]
                input_params <- paste0(set_abst_input_params(),paste0(name_x,"=dt$x,"),paste0(name_y,"=dt$y"))
                
                mesh <- meshgrid(thres_x,thres_y)
                dt <- data.table(c(mesh$X)) # x-coordinates of vertices
                setnames(dt,"V1","x")       
                dt[,y:=c(mesh$Y)]           # y-coordinates of vertices
                for(i in 1:length(variables)) {
                    name <- variables[[i]]
                    if(!name %in% c(name_x,name_y)) {
                        data <- c(loading_ss_file()$thr[[name]][input[[paste0("scale_slider_ss_",plot_index,"_",i)]]  ],
                                  loading_ss_file()$thr[[name]][input[[paste0("scale_slider_ss_",plot_index,"_",i)]]+1])
                        dt <- as.data.table(merge.default(dt, data.table(V1=data) ))
                        di <- dim_indices[i]
                        setnames( dt, "V1", di)
                        input_params <- paste(input_params,paste0(name,"=dt$",di),sep = ",")
                    }
                }
                input_params <- paste0(input_params,")")
                incProgress(1/5)
                
                inds_x <- 1:length(thres_x)
                inds_y <- 1:length(thres_y)
                inds <- meshgrid(inds_x,inds_y)
                dt[,ix:=c(inds$X)]          # indices of thresholds in x-dimension
                dt[,iy:=c(inds$Y)]          # indices of thresholds in y-dimension
                dt[,id:=1:nrow(dt)]         # id of the record inside DT
                
                dt[,fx:=eval(funcs_abst[[name_x]])(eval(parse(text=input_params))) ]
                dt[,fy:=eval(funcs_abst[[name_y]])(eval(parse(text=input_params))) ]
                
                ################
                # mesh <- meshgrid(thres_x,thres_y)
                # 
                # input_params <- set_abst_input_params()
                # for(i in 1:length(variables)) {
                #     if(variables[[i]] == name_x || variables[[i]] == name_y) {
                #         if(variables[[i]] == name_x)  input_params <- paste0(input_params,variables[[i]],"=","mesh$X",",")
                #         else                                        input_params <- paste0(input_params,variables[[i]],"=","mesh$Y",",")
                #     } else input_params <- paste0(input_params,variables[[i]],"=",loading_ss_file()$thr[[i]][input[[paste0("scale_slider_ss_",plot_index,"_",i)]] ],",")
                # }
                # substr(input_params,nchar(input_params),nchar(input_params)) <- ")"
                # 
                # fx = eval(funcs_abst[[name_x]])(eval(parse(text=input_params)))
                # fy = eval(funcs_abst[[name_y]])(eval(parse(text=input_params)))
                # 
                # incProgress(1/5)
                # 
                # 
                # dt <- data.table(c(mesh$X)) # x-coordinates of vertices
                # setnames(dt,"V1","x")       
                # dt[,y:=c(mesh$Y)]           # y-coordinates of vertices
                # dt[,fx:=c(fx)]              # derivatives in x-dimension
                # dt[,fy:=c(fy)]              # derivatives in y-dimension
                # dt[,ix:=c(inds$X)]          # indices of thresholds in x-dimension
                # dt[,iy:=c(inds$Y)]          # indices of thresholds in y-dimension
                # dt[,id:=1:nrow(dt)]         # id of the record inside DT
                ##################
                
                incProgress(1/5)
                
                # NEW STYLE
                edge_x <- length(loading_ss_file()$thr[[index_x]])
                edge_y <- length(loading_ss_file()$thr[[index_y]])
                out2 <- unique(rbind(dt[fx>0, .(x=c(ix-1,ix-1), y=c(iy,iy-1),   sx=c(ix,ix),     sy=c(iy,iy-1))],
                                     dt[fx<0, .(x=c(ix,ix),     y=c(iy,iy-1),   sx=c(ix-1,ix-1), sy=c(iy,iy-1))],
                                     dt[fy>0, .(x=c(ix,ix-1),   y=c(iy-1,iy-1), sx=c(ix,ix-1),   sy=c(iy,iy))],
                                     dt[fy<0, .(x=c(ix,ix-1),   y=c(iy,iy),     sx=c(ix,ix-1),   sy=c(iy-1,iy-1))]
                ))
                incProgress(1/5)
                # out2[x == edge_x-1 & sx == edge_x, c("x","sx")] <- out2[x == edge_x-1 & sx == edge_x, c("sx","x")]
                # out2[y == edge_y-1 & sy == edge_y, c("y","sy")] <- out2[y == edge_y-1 & sy == edge_y, c("sy","y")]
                
                out2 <- out2[!x %in% c(0,edge_x) & !y %in% c(0,edge_y) & !sx %in% c(0,edge_x) & !sy %in% c(0,edge_y)]
                tmp_out <-  out2[,.(sx=x,sy=y,ssx=sx,ssy=sy)]
                
                # tmp_out <- out2[!x %in% c(0,edge_x) & !y %in% c(0,edge_y)]
                # setnames(tmp_out,c("sx","sy"),c("ssx","ssy"))
                # setnames(tmp_out,c("x","y"),c("sx","sy"))
                tmp_out <- merge(out2,tmp_out,by=c("sx","sy"),all=T,allow.cartesian=T)[!sx %in% c(0,edge_x) & 
                                                                                           !sy%in%c(0,edge_y)]
                incProgress(1/5)
                
                out2 <- unique(rbind(
                    out2[!x %in% c(0,edge_x) &
                         !y %in% c(0,edge_y) &
                         !sx %in% c(0,edge_x) &
                         !sy %in% c(0,edge_y)],
                    tmp_out[,ifelse(nrow(.SD[(x-1==sx & y==sy & sx-1==ssx & sy==ssy) | 
                                             (x+1==sx & y==sy & sx+1==ssx & sy==ssy) |
                                             (x==sx & y-1==sy & sx==ssx & sy-1==ssy) |
                                             (x==sx & y+1==sy & sx==ssx & sy+1==ssy)])==0,T,F),by=.(sx,sy)][V1==T,.(x=sx,y=sy,sx=sx,sy=sy)]
                ))
                incProgress(1/5)
                
                
                transition_state_space$data[[plot_index]] <- out2[,id := 1:nrow(out2)]
                out2 <- NULL
                tmp_out <- NULL   
                dt <- NULL
            }, message="waiting for transition-state space"))
            cat(paste0("computing state-space for ",plot_index,"\n"))
            print(timing)
        }
        
        plot(range_x, range_y, type="n", xlab=name_x, ylab=name_y, xaxs="i", yaxs="i", xlim=boundaries[[index_x]], ylim=boundaries[[index_y]])
        abline(v=thres_x, h=thres_y)
        
        tss <- transition_state_space$data[[plot_index]]
        suppressWarnings(arrows(
            (thres_x[tss$x]+thres_x[tss$x+1])*0.5,
            (thres_y[tss$y]+thres_y[tss$y+1])*0.5,
            ifelse(tss$x == tss$sx, (thres_x[tss$sx]+thres_x[tss$sx+1])*0.5, 
                   ifelse(thres_x[tss$x] < thres_x[tss$sx], thres_x[tss$sx], thres_x[tss$sx+1])),
            ifelse(tss$y == tss$sy, (thres_y[tss$sy]+thres_y[tss$sy+1])*0.5, 
                   ifelse(thres_y[tss$y] < thres_y[tss$sy], thres_y[tss$sy], thres_y[tss$sy+1])),
            angle=20,length=0.07,col=switch(input$colVariant,
                                            "none"       = "black",
                                            "horizontal" = ifelse(tss$x < tss$sx, positive_color, ifelse(tss$x > tss$sx,negative_color,neutral_color)),
                                            "vertical"   = ifelse(tss$y < tss$sy, positive_color, ifelse(tss$y > tss$sy,negative_color,neutral_color)),
                                            "both"       = ifelse(tss$x < tss$sx | tss$y < tss$sy, positive_color,
                                                                  ifelse(tss$x == tss$sx & tss$y == tss$sy, neutral_color, negative_color))
            ),#lwd=input$transWidth,lty=transitions_line_type
            lwd=ifelse(tss$x == tss$sx & tss$y == tss$sy, 3*input$transWidth, input$transWidth),lty=transitions_line_type
        ))
        #============== drawing reachable states =================================
        if(length(state_space_clicked$point) >= plot_index && !(is.null(state_space_clicked$point[[plot_index]]) || is.na(state_space_clicked$point[[plot_index]]))) {
            point <- state_space_clicked$point[[plot_index]]
            if(is.na(state_space_clicked$old_point[[plot_index]]) || !identical(state_space_clicked$old_point[[plot_index]],point) || 
                    !identical(transition_state_space$globals[[plot_index]],checkpoint)) {
                cat(paste0("computing reachable states for ",plot_index,"\n"))
                
                starting_state_x <- max(which(thres_x <= point[[name_x]]))
                starting_state_y <- max(which(thres_y <= point[[name_y]]))
                
                start <- tss[starting_state_x == x & starting_state_y == y,.(x,y)]
                reachable <- unique(rbind(start,tss[starting_state_x == x & starting_state_y == y,.(x=sx,y=sy)]))
                repeat {
                    new_states <- tss[paste(x,y,"_") %in% paste(reachable$x,reachable$y,"_"),.(x=sx,y=sy)]
                    if(nrow(unique(rbind(reachable,new_states))) != nrow(reachable))   reachable <- unique(rbind(reachable,new_states))
                    else    break
                }
                state_space_clicked$data[[plot_index]] <- unique(reachable[,.(thres_x[x],      # V1: xleft points for reachable rectangles
                                                                              thres_y[y],      # V2: ybottom points for reachable rectangles
                                                                              thres_x[x+1],    # V3: xright points for reachable rectangles
                                                                              thres_y[y+1])])  # V4: ytop points for reachable rectangles
            }
            reachable <- state_space_clicked$data[[plot_index]]
            rect(reachable$V1,reachable$V2,reachable$V3,reachable$V4,border="blue",lwd=2)
            # it has to be at the end
            state_space_clicked$old_point[[plot_index]] <- point
        }
        # it has to be at the end
        transition_state_space$globals[[plot_index]] <- checkpoint
    }
}
draw_state_space_new <- function(name_x, name_y, plot_index, boundaries) {
    variables <- loading_vf_file()$vars
    index_x <- match(name_x,variables)
    index_y <- match(name_y,variables)
    ranges <- loading_vf_file()$ranges
    thres <- loading_ss_file()$thr
    #thres <- lapply(1:length(variables),function(n) sort(c(ranges[[n]],runif(50,ranges[[n]][1],ranges[[n]][2]))))
    names(thres) <- variables
    #     mesh <- meshgrid(thres[[index_x]],thres[[index_y]])
    # drawing of state space =================================================
    #     input_params <- set_input_params()
    #     for(i in 1:length(variables)) {
    #         if(variables[[i]] == name_x || variables[[i]] == name_y) {
    #             if(variables[[i]] == name_x)  input_params <- paste0(input_params,variables[[i]],"=","mesh$X",",")
    #             else                                        input_params <- paste0(input_params,variables[[i]],"=","mesh$Y",",")
    #         } else input_params <- paste0(input_params,variables[[i]],"=",input[[paste0("scale_slider_vf_",plot_index,"_",i)]],",")
    #     }
    #     substr(input_params,nchar(input_params),nchar(input_params)) <- ")"
    
    # doplnit selfloops a reakcie na ostatne veci
    
    timing <- system.time({
        fx = eval(funcs[[name_x]])(eval(parse(text=input_params)))
        fy = eval(funcs[[name_y]])(eval(parse(text=input_params)))
        
        inds_x <- 1:length(thres[[index_x]])
        inds_y <- 1:length(thres[[index_y]])
        inds <- meshgrid(inds_x,inds_y)
        
        dt <- data.table(c(mesh$X))
        setnames(dt,"V1","x")
        dt[,y:=c(mesh$Y)]
        dt[,fx:=c(fx)]
        dt[,fy:=c(fy)]
        dt[,ix:=c(inds$X)]
        dt[,iy:=c(inds$Y)]
        dt[,ind:=1:nrow(dt)]
        
        plot(range(thres[[index_x]]), range(thres[[index_y]]), type="n", xlab=name_x, ylab=name_y,
             xaxs="i", yaxs="i", xlim=boundaries[[index_x]], ylim=boundaries[[index_y]])
        abline(v=thres[[index_x]],h=thres[[index_y]])
        
        # rbind(dt[fx>0 & !x %in% ranges[[index_x]],.(x=ix-1,y=iy,sx=ix,sy=iy) ],
        #       dt[fx<0 & !x %in% ranges[[index_x]],.(x=ix,y=iy,sx=ix-1,sy=iy) ])
        
        tmp1 <- dt[fy<0 & !y %in% ranges[[index_y]] & x != ranges[[index_x]][2],
                   .((thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5, # x-coordinates of states center
                     (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5, # y-coordinates of states center
                     (thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5, # x-coordinates of transition end
                     thres[[index_y]][iy])]                             # y-coordinates of transition end
        tmp2 <- dt[fy<0 & !y %in% ranges[[index_y]] & x != ranges[[index_x]][1],
                   .((thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5, # x-coordinates of states center
                     (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5, # y-coordinates of states center
                     (thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5, # x-coordinates of transition end
                     thres[[index_y]][iy])]                             # y-coordinates of transition end
        down <- merge(tmp1,tmp2,c("V1","V2","V3","V4"),all=T)
        #         tmp <- dt[fy<0 & !y %in% ranges[[index_y]],
        #                   .(ifelse(x==ranges[[index_x]][1], (thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5,
        #                            ifelse(x==ranges[[index_x]][2], (thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5,
        #                                   c((thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5, (thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5))),
        #                     ifelse(x==ranges[[index_x]][1], (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5,
        #                            ifelse(x==ranges[[index_x]][2], (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5,
        #                                   c((thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5, (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5))),
        #                     ifelse(x==ranges[[index_x]][1], (thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5,
        #                            ifelse(x==ranges[[index_x]][2], (thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5,
        #                                   c((thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5, (thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5))),
        #                     ifelse(x==ranges[[index_x]][1], thres[[index_y]][iy],
        #                            ifelse(x==ranges[[index_x]][2], thres[[index_y]][iy],
        #                                   c(thres[[index_y]][iy], thres[[index_y]][iy])))
        #                       )]
        #         identical(down,unique(tmp))
        suppressWarnings(arrows(down$V1, down$V2, down$V3, down$V4,                             
                                angle=20,length=0.07,col="red",lwd=input$transWidth, lty=transitions_line_type))
        tmp1 <- dt[fx<0 & !x %in% ranges[[index_x]] & y != ranges[[index_y]][2],
                   .((thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5, # x-coordinates of states center
                     (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5, # y-coordinates of states center
                     thres[[index_x]][ix],                              # x-coordinates of transition end
                     (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5)]# y-coordinates of transition end
        tmp2 <- dt[fx<0 & !x %in% ranges[[index_x]] & y != ranges[[index_y]][1],
                   .((thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5, # x-coordinates of states center
                     (thres[[index_y]][iy]+thres[[index_y]][iy-1])*0.5, # y-coordinates of states center
                     thres[[index_x]][ix],                              # x-coordinates of transition end
                     (thres[[index_y]][iy]+thres[[index_y]][iy-1])*0.5)]# y-coordinates of transition end
        left <- merge(tmp1,tmp2,c("V1","V2","V3","V4"),all=T)
        suppressWarnings(arrows(left$V1, left$V2, left$V3, left$V4,
                                angle=20,length=0.07,col="red",lwd=input$transWidth, lty=transitions_line_type))
        tmp1 <- dt[fy>0 & !y %in% ranges[[index_y]] & x != ranges[[index_x]][2],
                   .((thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5, # x-coordinates of states center
                     (thres[[index_y]][iy]+thres[[index_y]][iy-1])*0.5, # y-coordinates of states center
                     (thres[[index_x]][ix]+thres[[index_x]][ix+1])*0.5, # x-coordinates of transition end
                     thres[[index_y]][iy])]                             # y-coordinates of transition end
        tmp2 <- dt[fy>0 & !y %in% ranges[[index_y]] & x != ranges[[index_x]][1],
                   .((thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5, # x-coordinates of states center
                     (thres[[index_y]][iy]+thres[[index_y]][iy-1])*0.5, # y-coordinates of states center
                     (thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5, # x-coordinates of transition end
                     thres[[index_y]][iy])]                             # y-coordinates of transition end
        up <- merge(tmp1,tmp2,c("V1","V2","V3","V4"),all=T)
        suppressWarnings(arrows(up$V1, up$V2, up$V3, up$V4,
                                angle=20,length=0.07,col="green",lwd=input$transWidth, lty=transitions_line_type))
        tmp1 <- dt[fx>0 & !x %in% ranges[[index_x]] & y != ranges[[index_y]][2],
                   .((thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5, # x-coordinates of states center
                     (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5, # y-coordinates of states center
                     thres[[index_x]][ix],                              # x-coordinates of transition end
                     (thres[[index_y]][iy]+thres[[index_y]][iy+1])*0.5)]# y-coordinates of transition end
        tmp2 <- dt[fx>0 & !x %in% ranges[[index_x]] & y != ranges[[index_y]][1],
                   .((thres[[index_x]][ix]+thres[[index_x]][ix-1])*0.5, # x-coordinates of states center
                     (thres[[index_y]][iy]+thres[[index_y]][iy-1])*0.5, # y-coordinates of states center
                     thres[[index_x]][ix],                              # x-coordinates of transition end
                     (thres[[index_y]][iy]+thres[[index_y]][iy-1])*0.5)]# y-coordinates of transition end
        right <- merge(tmp1,tmp2,c("V1","V2","V3","V4"),all=T)
        suppressWarnings(arrows(right$V1, right$V2, right$V3, right$V4,
                                angle=20,length=0.07,col="green",lwd=input$transWidth, lty=transitions_line_type))
    })
    print(timing)
    ######   
}
draw_state_space <- function(name_x, name_y, plot_index, boundaries) {
    index_x <- match(name_x,loading_ss_file()$var_names)
    index_y <- match(name_y,loading_ss_file()$var_names)
    ranges <- loading_vf_file()$ranges
    thres <- loading_ss_file()$thr
    names(thres) <- loading_ss_file()$var_names
    
    #cat("I'm gonna draw SS\n")
    if(!is.null(loading_ss_file())) {
        timing <- proc.time()
        
        plot(ranges[[index_x]], rangea[[index_y]], type="n", xlab=name_x, ylab=name_y,
             xaxs="i", yaxs="i", xlim=boundaries[[index_x]], ylim=boundaries[[index_y]])
        
        abline(v=thres[[index_x]],h=thres[[index_y]])
        
        #cat("I'm gonna pick-up layers\n")
        one_layer <- 1:nrow(loading_ss_file()$states)
        for(i in 1:(ncol(loading_ss_file()$states)-1)) {
            if(!loading_ss_file()$var_names[[i]] %in% c(name_x,name_y)) {
                one_layer <- loading_ss_file()$states[one_layer][get(paste0("V",i)) == (ifelse(is.null(input[[paste0("scale_slider_ss_",plot_index,"_",i)]]),1,input[[paste0("scale_slider_ss_",plot_index,"_",i)]])-1),id]
                one_layer <- loading_ss_file()$succs[one_layer][get(paste0("V",i)) == (ifelse(is.null(input[[paste0("scale_slider_ss_",plot_index,"_",i)]]),1,input[[paste0("scale_slider_ss_",plot_index,"_",i)]])-1),id]
            }
        }
        
        #cat("I'm gonna pick-up param_sets\n")
        param_sets <- vector(length=loading_ss_file()$params_num*2)
        if(loading_ss_file()$params_num != 0) {
            for(i in 0:(loading_ss_file()$params_num -1)) {
#                param_sets[2*i+1] <- as.numeric(ifelse(is.null(input[[paste0("param_slider_",i+1)]]),g_param_slider[2*i+1],input[[paste0("param_slider_",i+1)]][1]))
#                param_sets[2*i+2] <- as.numeric(ifelse(is.null(input[[paste0("param_slider_",i+1)]]),g_param_slider[2*i+2],input[[paste0("param_slider_",i+1)]][1]))
                param_sets[2*i+1] <- as.numeric(ifelse(is.null(input[[paste0("param_slider_vf_",i+1)]]) | is.na(input[[paste0("param_slider_vf_",i+1)]]),
                                                       no_param_const,input[[paste0("param_slider_vf_",i+1)]][1]))
                param_sets[2*i+2] <- as.numeric(ifelse(is.null(input[[paste0("param_slider_vf_",i+1)]]) | is.na(input[[paste0("param_slider_vf_",i+1)]]),
                                                       no_param_const,input[[paste0("param_slider_vf_",i+1)]][1]))
                
            }
            coloring <- switch(as.character(length(param_sets)),
                               "2" = loading_ss_file()$params[one_layer, ifelse(V1 > param_sets[2] | V2 < param_sets[1],0,id)],
                               "4" = loading_ss_file()$params[one_layer, ifelse(V1 > param_sets[2] | V2 < param_sets[1] | V3 > param_sets[4] | V4 < param_sets[3],0,id)],
                               "6" = loading_ss_file()$params[one_layer, ifelse(V1 > param_sets[2] | V2 < param_sets[1] | V3 > param_sets[4] | V4 < param_sets[3] | V5 > param_sets[6] | V6 < param_sets[5],0,id)],
                               "8" = loading_ss_file()$params[one_layer, ifelse(V1 > param_sets[2] | V2 < param_sets[1] | V3 > param_sets[4] | V4 < param_sets[3] | V5 > param_sets[6] | V6 < param_sets[5] | V7 > param_sets[8] | V8 < param_sets[7],0,id)])
                            # TODO: more variants or more genral way
        } else {
            coloring <- loading_ss_file()$states[,id]
        }
        
        #cat("I'm gonna draw arrows\n")
        suppressWarnings(arrows(
            (thres[[index_x]][as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_x))])+1]+thres[[index_x]][as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_x))])+2])*0.5,
            (thres[[index_y]][as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_y))])+1]+thres[[index_y]][as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_y))])+2])*0.5,
            ifelse(thres[[index_x]][as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_x))])+1] == thres[[index_x]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))])+1], 
                   (thres[[index_x]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))])+1]+thres[[index_x]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))])+2])*0.5, 
                   ifelse(thres[[index_x]][as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_x))])+1] < thres[[index_x]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))])+1],
                          thres[[index_x]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))])+1],
                          thres[[index_x]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))])+2])),
            ifelse(thres[[index_y]][as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_y))])+1] == thres[[index_y]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))])+1], 
                   (thres[[index_y]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))])+1]+thres[[index_y]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))])+2])*0.5, 
                   ifelse(thres[[index_y]][as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_y))])+1] < thres[[index_y]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))])+1],
                          thres[[index_y]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))])+1],
                          thres[[index_y]][as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))])+2])),
            angle=20,length=0.07,col=switch(input$colVariant,
                                            "none"       = "black",
                                            "horizontal" = ifelse(as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_x))]) < as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))]),positive_color,
                                                                  ifelse(as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_x))]) > as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))]),negative_color,neutral_color)),
                                            "vertical"   = ifelse(as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_y))]) < as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))]),positive_color,
                                                                  ifelse(as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_y))]) > as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))]),negative_color,neutral_color)),
                                            "both"       = ifelse(as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_x))]) < as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))]) | 
                                                                      as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_y))]) < as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))]),positive_color,
                                                                  ifelse(as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_x))]) == as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_x))]) & 
                                                                             as.integer(loading_ss_file()$states[coloring,get(paste0("V",index_y))]) == as.integer(loading_ss_file()$succs[coloring,get(paste0("V",index_y))]),neutral_color,negative_color))
            ),
            lwd=ifelse(conj(loading_ss_file()$states[coloring] == loading_ss_file()$succs[coloring]),3*input$transWidth,input$transWidth),lty=transitions_line_type
        ))
        #cat("... drawing state space ",plot_index,":\n")
        #print(proc.time() - timing)

        #============== drawing reachable states =================================
        if(length(state_space_clicked$point) >= plot_index && !(is.null(state_space_clicked$point[[plot_index]]) || is.na(state_space_clicked$point[[plot_index]]))) {
            point <- state_space_clicked$point[[plot_index]]
            starting_state_x <- max(which(thres[[index_x]] <= point$x))
            starting_state_y <- max(which(thres[[index_y]] <= point$y))
            
            ids <- NULL
            new_ids <- loading_ss_file()$states[coloring & starting_state_x-1 == get(paste0("V",index_x)) & starting_state_y-1 == get(paste0("V",index_y)),id]
            ids <- unique(c(ids,new_ids))
            repeat {
                new_states <- loading_ss_file()$succs[new_ids]
                new_ids <- loading_ss_file()$states[coloring & paste(get(paste0("V",index_x)),get(paste0("V",index_y)),"_") %in% paste(new_states[,get(paste0("V",index_x))],new_states[,get(paste0("V",index_y))],"_"),id]
                if(length(unique(c(ids,new_ids))) != length(ids))   ids <- unique(c(ids,new_ids))
                else    break
            }
            reachable <- unique(loading_ss_file()$states[ids,.(thres[[index_x]][get(paste0("V",index_x))+1],    # V1: xleft points for reachable rectangles
                                                               thres[[index_y]][get(paste0("V",index_y))+1],    # V2: ybottom points for reachable rectangles
                                                               thres[[index_x]][get(paste0("V",index_x))+2],    # V3: xright points for reachable rectangles
                                                               thres[[index_y]][get(paste0("V",index_y))+2])])  # V4: ytop points for reachable rectangles
            rect(reachable$V1,reachable$V2,reachable$V3,reachable$V4,border="blue",lwd=2)
        }
    }
}
draw_1D_state_space <- function(name_x, plot_index, boundaries) {
    index_x <- match(name_x,loading_ss_file()$var_names)
    ranges <- loading_vf_file()$ranges
    thres <- loading_ss_file()$thr
    names(thres) <- loading_ss_file()$var_names
    
    plot(ranges[[index_x]], c(0,1), type="n", xlab=name_x, ylab="", yaxt="n", xlim=boundaries[[index_x]], xaxs="i", yaxs="i")
    
    #TODO:
}

#=============== RESULT TAB ============================================
#=======================================================================

reset_globals_param <- function() {
    for(i in param_chosen$data) {
        param_chosen$data <- param_chosen$data[param_chosen$data != i]
    }
}

observeEvent(c(resultFile()),{
    if(!is.null(resultFile()) && length(resultFile()) > 0) {
        loaded_ps_file$filename <- resultFileName
        loaded_ps_file$data <- fromJSON(file=loaded_ps_file$filename)
    }
})

observeEvent(input$ps_file,{
    if(!is.null(input$ps_file) && !is.null(input$ps_file$datapath)) {
        if(!is.null(loaded_ps_file$filename) && loaded_ps_file$filename != input$ps_file$datapath) {
            #session$reload()
            reset_globals_param()
        }
        loaded_ps_file$filename <- input$ps_file$datapath
        loaded_ps_file$data <- fromJSON(file=loaded_ps_file$filename)
    } else {
        # # initial example file (temporary)    
        # if(!is.null(loaded_vf_file$filename) && loaded_vf_file$filename == paste0(examples_dir,"//model_2D_1P_400R.bio")) {
        #     loaded_ps_file$filename <- paste0(examples_dir,"//model_2D_1P_400R.result.json")
        #     loaded_ps_file$data <- fromJSON(file=loaded_ps_file$filename)
        # } else {
        #     loaded_ps_file$filename <- NULL
        #     loaded_ps_file$data <- NULL
        # }
    }
})


output$save_result_file <- downloadHandler(
    filename = ifelse(!is.null(input$ps_file) && !is.null(input$ps_file$datapath), paste0(input$ps_file$datapath), "results.json"),
    content = function(file) {
        if(!is.null(loaded_ps_file$data))
            writeLines(toJSON(loaded_ps_file$data), file)
        else writeLines("", file)
    }
)


loading_ps_file <- reactive({
    file <- loaded_ps_file$data
    if(!is.null(file)) {
        if(class(file) == "list") {     # temporary
            
            # musi byt kontrola ci result file vobec nieco obsahuje !!!!
            
            table <- rbindlist(lapply(file$results,function(x) if(length(x$data)!=0) as.data.table(x))) # temporary measure: empty results are omitted
            setkeyv(table,"formula")
            # table[,state:=sapply(data,function(x)unlist(x$state))]
            # table[,param:=sapply(data,function(x)unlist(x$param))]
            table[,state:=sapply(data,function(x)unlist(x[1]))]
            table[,param:=sapply(data,function(x)unlist(x[2]))]
            table[,cov:=nrow(.SD), by=list(formula,param)]
            table[,data:=NULL]
            
            states <- as.data.table(t(sapply(lapply(1:length(file$states),function(x) file$states[[x]]$bounds), function(s) unlist(s))))
            states[, id:=1:nrow(states)]
            #states[, id:=sapply(1:length(file$states),function(x) file$states[[x]]$id+1)]
            
            if(file$type == "rectangular") {    
                # file$parameter_values [[1]] [[1]] [[1]] [1:2]
                #                        set  rect   dim  range
                # params <- as.data.table(t(sapply(chunk(unlist(file$parameter_values),2*length(file$parameters)),unlist)))
                # not_empty_ids <- 1:length(file$parameter_values)
                # times <- sapply(lapply(not_empty_ids,function(x) file$parameter_values[[x]]),length)
                params <- as.data.table(t(sapply(chunk(unlist(file$parameter_values),2*length(file$parameters)),unlist)))
                not_empty_ids <- 1:length(file$parameter_values)
                times <- sapply(lapply(not_empty_ids,function(x) file$parameter_values[[x]]),length)
                params[, id:=unlist(sapply(1:length(not_empty_ids),function(x) rep.int(not_empty_ids[x],times[x])))]
                params[, row_id:=1:nrow(params)]
            }
            if(file$type == "smt") {
                # TODO:
                # params <- as.data.table(t(sapply(lapply(1:length(file$params$rectangles),function(x) file$params$rectangles[[x]]), function(p) unlist(p))))
                # params[, id:=1:nrow(params)]
                
                # params <- data.table(expr=sapply(1:length(file$parameter_values),function(x) file$parameter_values[[x]]$Rexpression))
                params <- data.table(expr=paste0("function(ip)",gsub("||","|",gsub("&&","&", sapply(1:length(file$parameter_values),
                                                                                        function(x) file$parameter_values[[x]]$Rexpression),fixed=T),fixed=T)))
                # params <- data.table(expr=parse(text=paste0("function(ip)",gsub("([a-z][a-z0-9_]+)","ip$\\1",sapply(1:length(file$parameter_values),
                #                                                                                          function(x) file$parameter_values[[x]]$Rexpression),perl=T))))
                params[, id:=1:nrow(params)]
                params[, row_id:=1:nrow(params)]
                setkey(params,id)
                # eval(parse(text=paste0("function(ip) ",gsub("([a-z][a-z0-9_]+)","ip$\\1",params$expr[1:5],perl=T))))(ip=list(deg_x=0.1,k1=1))
                
                # num <- 50
                # nesh <- meshgrid(seq(0,1,length.out = num),
                #                  seq(0,2,length.out = num))
                # dt <- data.table(x1=unlist(as.list(nesh$X[1:(num-1),1:(num-1)])),x2=unlist(as.list(nesh$X[2:num,2:num])),
                #                  y1=unlist(as.list(nesh$Y[1:(num-1),1:(num-1)])),y2=unlist(as.list(nesh$Y[2:num,2:num])))
                # dt[,x:=x1+(x2-x1)*0.5]
                # dt[,y:=y1+(y2-y1)*0.5]
                # dt[,cov:=0]
                # 
                # system.time(for(ex in params$id) dt[,cov:=cov+ifelse(eval(parse(text=params[ex,expr]))(list(deg_x=x,k1=y)),1,0)])
                
                # tt <- as.data.table(merge.default(params,dt,all=T))
                # setkey(tt,id)
                # system.time(tt[,truth:=(eval(parse(text=expr))(list(deg_x=x,k1=y))),by=.(expr,x,y)])
                # tt[,.(cov=nrow(.SD[truth==T]),x1,x2,y1,y2),by=.(x,y)]
                
                # f <- function(e,l) eval(parse(text=e[1]))(list(deg_x=dtt$x,k1=dtt$y))
                # tt[, truth:=f(.SD), by=.(expr,x,y) ]
            }
            
            # time <- system.time({
            #     for(i in seq(0,1,length.out = 50))
            #         for(j in seq(0,2,length.out = 50))
            #             neco <- params[,sapply(parse(text=expr),function(x) eval(x)(ip<-list(deg_x=i,k1=j)))]
            # })
            # print(time)
            
            thrs <- file$thresholds
            names(thrs) <- file$variables
            
            param_bounds <- file$parameter_bounds
            names(param_bounds) <- file$parameters
            
            return(list(param_space=table,  # DT( formula:string, data:list_of_state_and_param_indices, state:numeric_index_to_states, param:numeric_index_to_params, 
                                            #     cov:numeric_with_states_count_for_this_param )
                        states=states,      # DT( V1,V2,... ) - each pair of columns (e.g., V1,V2) contains boundary threshodls of state (each row), 3D SS has 6 columns
                        formulae=unique(table$formula),     # vector of unique formulae from table 'table' or 'param_space'
                        param_names=file$parameters,        # vector of param names in order of appearence in bio file
                        params=params,                      # DT( V1,V2,... ) - each row is satisfied rectangle (of param set with same ID) in PS (each param has two columns: P1 has V1,V2 ...)
                        var_names=file$variables,           # vector of unique variable names in order of appearence in bio file
                        thresholds=thrs,                    # list of thresholds for each variable
                        type=file$type,
                        param_bounds=param_bounds
            ))
            
        } else return(NULL)
    } else return(NULL)
})


output$param_selector <- renderUI({
    if(!is.null(loading_ps_file())) {
        list_of_param_names <- as.list(c(loading_ps_file()$param_names, loading_ps_file()$var_names, "Choose"=empty_sign))
        #list_of_param_names <- as.list(c(loading_ps_file()$param_names, "Choose"=empty_sign))
        lapply(param_update(), function(i) {
            idx <- paste0("param_selector_x_",i)
            labelx <- paste0("horizontal axis in plot ",i)
            choicesx <- list_of_param_names
            selectedx <- ifelse(!is.null(input[[paste0("param_selector_x_",i)]]), input[[paste0("param_selector_x_",i)]], #empty_sign)
                                list_of_param_names[1])
            
            idy <- paste0("param_selector_y_",i)
            labely <- paste0("vertical axis in plot ",i)
            choicesy <- list_of_param_names
            selectedy <- ifelse(!is.null(input[[paste0("param_selector_y_",i)]]), input[[paste0("param_selector_y_",i)]], #empty_sign)
                                ifelse(length(list_of_param_names) > 1, list_of_param_names[2], list_of_param_names[1]))
            
            fluidRow(
                column(5,
                       selectInput(idx,labelx,choicesx,selectedx)
                ),
                column(5,
                       selectInput(idy,labely,choicesy,selectedy)
                ),
                column(2,
                       actionButton(paste0("cancel_ps_",i), "cancel"),
                       checkboxInput(paste0("hide_ps_",i), "hide", ifelse(!is.null(input[[paste0("hide_ps_",i)]]),input[[paste0("hide_ps_",i)]],F))
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
        for(i in param_chosen$data) {
            if(!is.null(input[[paste0("cancel_ps_",i)]]) && input[[paste0("cancel_ps_",i)]] > 0) {
                param_chosen$data <- param_chosen$data[param_chosen$data != i]
                # some kind of garbage collector would be very convenient in this phase
                # either gc() or rm()
                #rm(input[[paste0("vf_selector_x_",i)]], input[[paste0("vf_selector_y_",i)]]) #TODO: doplnit dalsie
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
                
                print(gc())
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
        
        param_chosen$data <- c(param_chosen$data,param_chosen$max)
        param_chosen$max <<- param_chosen$max + 1
    }
})
param_update <- reactive({
    return(param_chosen$data)
})
visible_ps_plots <- reactive({
    if(!is.null(loading_ps_file())) {
        local_result <- c()
        for(i in param_chosen$data) {
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
                h3("Please select at least one parameter.")
            else
            fluidRow(
                column(2,
                       #actionButton(paste0("apply_plot_ps_",i),"Apply to all"),
                       actionButton(paste0("clear_plot_ps_",i),"Deselect click"),
                       actionButton(paste0("unzoom_plot_ps_",i),"Unzoom"),
                       lapply(1:length(list_of_all_names), function(t) {
                          if(!list_of_all_names[[t]] %in% c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]])) {
                              is_mixed <- input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names
                              is_var <- list_of_all_names[[t]] %in% loading_ps_file()$var_names
                              label <- paste0("scale in ",list_of_all_names[[t]])
                              name <- paste0("scale_slider_ps_",i,"_",t)
                              if(is_var)    values <- range(loading_ps_file()$thresholds[[ which(loading_ps_file()$var_names == list_of_all_names[[t]]) ]])
                              else          values <- param_ranges()[[t]]
                              fluidRow(
                                  column(12,
                                         fluidRow(
                                             column(1,
                                                conditionalPanel(
                                                    condition = "input.advanced == true",
                                                    checkboxInput(paste0("scale_switch_ps_",i,"_",t), label=NULL,
                                                          ifelse(!is.null(input[[paste0("scale_switch_ps_",i,"_",t)]]), input[[paste0("scale_switch_ps_",i,"_",t)]],
                                                          ifelse(is_var, F, T))
                                                    )
                                                )
                                             ),
                                             column(11,
                                                if(input$advanced || !is_var)
                                                    helpText(label)
                                             )
                                         ),
                                         conditionalPanel(
                                             condition = paste0("input.scale_switch_ps_",i,"_",t," == true"),
                                             sliderInput(name,label=NULL,min=values[1],max=values[2],step=0.001,
                                                         value=ifelse(is.null(input[[name]]), values[1], input[[name]] ))
                                         )
                                  )
                              )
                          }
                       }),
#                        if(input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names) {
#                            lapply(1:length(list_of_all_names), function(t) {
#                                if(!list_of_all_names[[t]] %in% c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]])) {
#                                    if(list_of_all_names[[t]] %in% loading_ps_file()$var_names) {
#                                        label <- paste0("discrete scale in ",list_of_all_names[[t]])
#                                        name <- paste0("scale_slider_ps_",i,"_",t)
#                                        values <- range(loading_ps_file()$thresholds[[ which(loading_ps_file()$var_names == list_of_all_names[[t]]) ]])
#                                        fluidRow(
#                                            column(12,
#                                                   fluidRow(
#                                                       column(1,
#                                                          conditionalPanel(
#                                                              condition = "input.advanced == true",
#                                                              checkboxInput(paste0("scale_switch_ps_",i,"_",t), NULL, 
#                                                                    ifelse(is.null(input[[paste0("scale_switch_ps_",i,"_",t)]]), T, input[[paste0("scale_switch_ps_",i,"_",t)]]))
#                                                          )
#                                                       ),
#                                                       column(11,
#                                                          helpText(label)
#                                                       )
#                                                   ),
#                                                   conditionalPanel(
#                                                       condition = paste0("input.scale_switch_ps_",i,"_",t," == true"),
#                                                       sliderInput(name,label=NULL,min=values[1],max=values[2],step=0.001,
#                                                                   value=ifelse(is.null(input[[name]]), values[1], input[[name]] ))
#                                                   )
#                                            )
#                                        )
#                                    } else {
#                                        label <- paste0("discrete scale in ",list_of_all_names[[t]])
#                                        name  <- paste0("scale_slider_ps_",i,"_",t)
#                                        values <- param_ranges()[[t]]
#                                        fluidRow(
#                                            column(12,
#                                                   fluidRow(
#                                                       column(1,
#                                                              conditionalPanel(
#                                                                  condition = "input.advanced == true",
#                                                                  checkboxInput(paste0("scale_switch_ps_",i,"_",t), NULL, 
#                                                                        ifelse(is.null(input[[paste0("scale_switch_ps_",i,"_",t)]]), T, input[[paste0("scale_switch_ps_",i,"_",t)]]))
#                                                              )
#                                                       ),
#                                                       column(11,
#                                                              helpText(label)
#                                                       )
#                                                   ),
#                                                   conditionalPanel(
#                                                       condition = paste0("input.scale_switch_ps_",i,"_",t," == true"),
#                                                       sliderInput(name,label=NULL,min=values[1],max=values[2],step=0.001,
#                                                                   value=ifelse(is.null(input[[name]]), values[1], input[[name]] ))
#                                                   )
#                                            )
#                                        )
#                                        #
#                                    }
#                                }
#                            })
#                        } else {
#                            lapply(1:length(list_of_param_names), function(t) {
#                                if(!list_of_param_names[[t]] %in% c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]])) {
#                                    label <- paste0("scale in ",list_of_param_names[[t]])
#                                    name <- paste0("scale_slider_ps_",i,"_",t)
#                                    values <- param_ranges()[[t]]
#                                    #session$sendCustomMessage(type='scaleSliderHandler', list(name=name,
#                                    #                                                          values=param_ranges_sat_for_formula()[[t]]) )
#                                    fluidRow(
#                                        column(12,
#                                               fluidRow(
#                                                   column(1,
#                                                          conditionalPanel(
#                                                              condition = "input.advanced == true",
#                                                              checkboxInput(paste0("scale_switch_ps_",i,"_",t), NULL, 
#                                                                    ifelse(is.null(input[[paste0("scale_switch_ps_",i,"_",t)]]), T, input[[paste0("scale_switch_ps_",i,"_",t)]]))
#                                                          )
#                                                   ),
#                                                   column(11,
#                                                          helpText(label)
#                                                   )
#                                               ),
#                                               conditionalPanel(
#                                                   condition = paste0("input.scale_switch_ps_",i,"_",t," == true"),
#                                                   sliderInput(name,label=NULL,min=values[1],max=values[2],step=0.001,
#                                                               value=ifelse(is.null(input[[name]]), values[1], input[[name]] ))
#                                               )
#                                        )
#                                    )
#                                    # another variant is to use double-ended sliders with dragRange - but this needs to differently manage click inside of a PS (it could be
#                                    #    a range instead of a point in each dimension) and still use with a checkbox which instantly cancel slider's effect
# #                                    sliderInput(name,label=label,min=values[1],max=values[2],step=1,
# #                                                value=c(ifelse(is.null(input[[name]]), values[1],
# #                                                             input[[name]][1]),
# #                                                        ifelse(is.null(input[[name]]), values[2],
# #                                                               input[[name]][2])))
#                                }
#                            })
#                        },
                       verbatimTextOutput(paste0("hover_text_ps_",i))
                ),
                column(4,
                       if(input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names)
                           helpText("parameter-variable dependency diagram")
                       else
                           helpText("parameter space of the model"),
                       div(id = "plot-container",
                           tags$img(
                               #src="circle.png",
                               src = "spinner.gif",
                               id = "loading-spinner"),
                           imageOutput(paste0("param_space_plot_",i),"auto","auto", click=paste0("ps_",i,"_click"), dblclick=paste0("ps_",i,"_dblclick"),
                                       hover=hoverOpts(id=paste0("ps_",i,"_hover"),delayType="debounce",delay=hover_delay_limit),
                                       brush=brushOpts(id=paste0("ps_",i,"_brush"),delayType="debounce",delay=brush_delay_limit,resetOnNew=T))
                       )
                ),
                column(4,
                       helpText("... and corresponding transition-state space"),
                       if(!is.null(input[[paste0("param_ss_selector_x_",i)]]) && input[[paste0("param_ss_selector_x_",i)]] != empty_sign &&
                              !is.null(input[[paste0("param_ss_selector_y_",i)]]) && input[[paste0("param_ss_selector_y_",i)]] != empty_sign) {
                           imageOutput(paste0("param_ss_plot_",i),"auto","auto", click=paste0("param_ss_",i,"_click"), dblclick=paste0("param_ss_",i,"_dblclick"),
                                       hover=hoverOpts(id=paste0("param_ss_",i,"_hover"),delayType="debounce",delay=hover_delay_limit),
                                       brush=brushOpts(id=paste0("param_ss_",i,"_brush"),delayType="debounce",delay=brush_delay_limit,resetOnNew=T))
                       }
                ),
                column(2,
                       #actionButton(paste0("apply_plot_param_ss_",i),"Apply to all"),
                       actionButton(paste0("clear_plot_param_ss_",i),"Deselect all"),
                       actionButton(paste0("unzoom_plot_param_ss_",i),"Unzoom"),
                       fluidRow(
                           column(6,
                                  selectInput(paste0("param_ss_selector_x_",i),"horizontal axis",list_of_var_names,
                                              ifelse(!is.null(input[[paste0("param_ss_selector_x_",i)]]),input[[paste0("param_ss_selector_x_",i)]], list_of_var_names[[1]]))
                           ),
                           column(6,
                                  selectInput(paste0("param_ss_selector_y_",i),"vertical axis",list_of_var_names,
                                              ifelse(!is.null(input[[paste0("param_ss_selector_y_",i)]]),input[[paste0("param_ss_selector_y_",i)]], #empty_sign))
                                                     ifelse(length(loading_ps_file()$var_names) > 1, list_of_var_names[[2]], list_of_var_names[[1]])))
                           )
                       ),
                       if(!is.null(input[[paste0("param_ss_selector_x_",i)]]) && input[[paste0("param_ss_selector_x_",i)]] != empty_sign &&
                                !is.null(input[[paste0("param_ss_selector_y_",i)]]) && input[[paste0("param_ss_selector_y_",i)]] != empty_sign) {
                           lapply(1:length(loading_ps_file()$var_names), function(t) {
                               if(!loading_ps_file()$var_names[[t]] %in% c(input[[paste0("param_ss_selector_x_",i)]],input[[paste0("param_ss_selector_y_",i)]])) {
                                   label <- paste0("discrete scale in ",loading_ps_file()$var_names[[t]])
                                   name <- paste0("scale_slider_param_ss_",i,"_",t)
                                   #values <- c(1,length(loading_ps_file()$thresholds[[t]])-1)
                                   values <- range(loading_ps_file()$thresholds[[t]])
                                   sliderInput(name,label=label,min=values[1],max=values[2],step=0.001, #,step=1,
                                               value=ifelse(is.null(input[[paste0("scale_slider_param_ss_",i,"_",t)]]),values[1],input[[paste0("scale_slider_param_ss_",i,"_",t)]]))
                               }
                           })
                       },
                       verbatimTextOutput(paste0("hover_text_param_ss_",i))
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
                if(input[[paste0("param_ss_selector_x_",my_i)]] != input[[paste0("param_ss_selector_y_",my_i)]]) {
                    output[[plotname]] <- renderPlot({
                        draw_param_ss(isolate(input[[paste0("param_ss_selector_x_",my_i)]]), isolate(input[[paste0("param_ss_selector_y_",my_i)]]), my_i, param_ss_brushed$data[[my_i]])
                    },height=change_height() # input$height
                    )
                } else {
                    output[[plotname]] <- renderPlot({
                        draw_1D_param_ss(isolate(input[[paste0("param_ss_selector_x_",my_i)]]), my_i, param_ss_brushed$data[[my_i]])
                    },height=change_height() # input$height
                    )
                }
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
                            paste0(name,": ",round(input[[paste0("scale_slider_param_ss_",my_i,"_",x)]],rounding_in_hover))
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
                                    paste0(name,": ",round(input[[paste0("scale_slider_ps_",my_i,"_",x)]],rounding_in_hover))
                                else
                                    paste0(name,": ",round(range(loading_ps_file()$thresholds[[name]])[1],rounding_in_hover)," - ",
                                           round(range(loading_ps_file()$thresholds[[name]])[2],rounding_in_hover) )
                            } else {
                                if(!is.null(input[[paste0("scale_switch_ps_",my_i,"_",x)]]) && input[[paste0("scale_switch_ps_",my_i,"_",x)]] )
                                    paste0(name,": ",round(input[[paste0("scale_slider_ps_",my_i,"_",x)]],rounding_in_hover))
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
# output$hover_info <- renderUI({
#     for (i in visible_ps_plots()) {
#         #local({
#             my_i <- i
#             hover <- input[[paste0("ps_",my_i,"_hover")]]
#             if(!is.null(hover)) {
#                 style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
#                                 "left:", 35, "%; top:", 35, "%;")
#                 wellPanel(       
#                     style = style,       
#                     p(HTML(paste0("<b> x: </b>", round(hover$x,5), "<br/>",                     
#                                   "<b> y: </b>", round(hover$y,5), "<br/>")))
#                 )
#             }
#         #})
#     }
# })

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


draw_param_ss <- function(name_x, name_y, plot_index, boundaries) {
    variables <- loading_ps_file()$var_names
    index_x <- match(name_x,variables)
    index_y <- match(name_y,variables)
    thres <- loading_ps_file()$thresholds
    
    if(!is.null(loading_ps_file())) {
        # create current set of globals
        checkpoint <- list(selectors=list(x=input[[paste0("param_ss_selector_x_",plot_index)]], y=input[[paste0("param_ss_selector_y_",plot_index)]]),
                           formula=chosen_ps_formulae_clean(),
                           counter=input$process_run,
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
                    ids <- intersect(ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) > sid, id])
                }
            }        # incremental intersection of ids in order to get right ids
            states <- states[id %in% ids]
            states[, border:="green"]   # corresponding rectangles based on selected parameter point could have blue border
            states[, color:=ifelse(id %in% param_ss_clicked$point[[plot_index]],"darkgreen",NA)]         # selected rectangles could be in darkgreen
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
            ids <- ps$row_id    # all ids at first
            for(x in 1:length(loading_ps_file()$param_names)) {
                name <- loading_ps_file()$param_names[x]
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
            blue_ids <- loading_ps_file()$param_space[(param+1) %in% ids & formula==chosen_ps_formulae_clean(),state+1]
            if(input[[paste0("param_selector_x_",plot_index)]] %in% variables || input[[paste0("param_selector_y_",plot_index)]] %in% variables) {
                for(x in 1:length(loading_ps_file()$var_names)) {
                    name <- loading_ps_file()$var_names[x]
                    if(!name %in% c(input[[paste0("param_selector_x_",plot_index)]],input[[paste0("param_selector_y_",plot_index)]]) ) {
                        if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x+length(loading_ps_file()$param_names))]]) && 
                           input[[paste0("scale_switch_ps_",plot_index,"_",x+length(loading_ps_file()$param_names))]]) {
                            sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x+length(loading_ps_file()$param_names))]] # right state value in dimension x
                            blue_ids <- intersect(blue_ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) > sid, id])
                        }
                    # } else {
                    #     blue_ids <- intersect(blue_ids, states[get(paste0("V",x*2-1)) <= point[[name]] & get(paste0("V",x*2)) > point[[name]], id])
                    }
                }        # incremental intersection of ids in order to get right ids
            }
            states <- states[id %in% blue_ids]
            
            # if(is.na(param_space_clicked$old_point[[plot_index]]) || !identical(param_space_clicked$old_point[[plot_index]], point) ||
            #        !identical(param_state_space$globals[[plot_index]],checkpoint) || !identical(param_ss_clicked$point[[plot_index]],param_ss_clicked$old_point[[plot_index]])) {
            # 
            #     ids <- loading_ps_file()$params$row_id    # all ids at first
            #     for(x in 1:length(loading_ps_file()$param_names)) {
            #         name <- loading_ps_file()$param_names[x]
            #         ids <- intersect(ids, loading_ps_file()$params[get(paste0("V",x*2-1)) < point[[name]][2] & get(paste0("V",x*2)) >= point[[name]][2] |
            #                                                        get(paste0("V",x*2-1)) <= point[[name]][1] & get(paste0("V",x*2)) > point[[name]][1] |
            #                                                        get(paste0("V",x*2-1)) >= point[[name]][1] & get(paste0("V",x*2)) <= point[[name]][2], row_id])
            #     }        # incremental intersection of param ids in order to get right ids
            #     ids <- unique(loading_ps_file()$params[row_id %in% ids,id])
            #     blue_ids <- loading_ps_file()$param_space[(param+1) %in% ids & formula==chosen_ps_formulae_clean(),state+1]
            #     param_space_clicked$data[[plot_index]] <- states[id %in% blue_ids]
            # }
            # states <- param_space_clicked$data[[plot_index]]
            # # this has to be at the end
            # param_space_clicked$old_point[[plot_index]] <- point
            
            rect(states[[paste0("V",index_x*2-1)]], states[[paste0("V",index_y*2-1)]], states[[paste0("V",index_x*2)]], states[[paste0("V",index_y*2)]],
                 border="blue", col=NA, lwd=2)
        }
        # this has to be at the end
        param_state_space$globals[[plot_index]] <- checkpoint
        param_ss_clicked$old_point[[plot_index]] <- param_ss_clicked$point[[plot_index]]
    }
}
# TODO:
draw_1D_param_ss <- function(name_x, plot_index, boundaries) {
    index_x <- match(name_x,loading_ps_file()$var_names)
    plot(range(loading_ps_file()$thresholds[[index_x]]), c(0,1), type="n", xlab=name_x, ylab="", yaxt="n", xlim=boundaries[[index_x]], xaxs="i", yaxs="i")
    abline(v=loading_ps_file()$thresholds[[index_x]])
    
    states <- copy(satisfiable_states())
    states[, border:="green"]
    states[, color:=ifelse(id %in% param_ss_clicked$point[[plot_index]],"darkgreen",NA)]
    
    rect(states[[paste0("V",index_x*2-1)]], 0, states[[paste0("V",index_x*2)]], 1,
         border=states$border, col=states$color, lwd=1.5)
    
    ##======= reaction on click inside a PS plot =========================
    if(length(param_space_clicked$point) >= plot_index && !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]])) &&
           !F %in% (names(param_space_clicked$point[[plot_index]]) %in% c(input[[paste0("param_selector_x_",plot_index)]],input[[paste0("param_selector_y_",plot_index)]])) &&
           !is.null(param_space_clicked$data[[plot_index]]) && nrow(param_space_clicked$data[[plot_index]]) != 0) {
        
        point <- param_space_clicked$point[[plot_index]]
        
        if(input[[paste0("param_selector_x_",plot_index)]] %in% variables)
            ids <- param_space_clicked$data[[plot_index]][y1 <= point[1] & y2 > point[1], row_id]
        else
            ids <- param_space_clicked$data[[plot_index]][x1 <= point[1] & x2 > point[1], row_id]
        
        if(!input[[paste0("param_selector_x_",plot_index)]] %in% variables && !input[[paste0("param_selector_y_",plot_index)]] %in% variables &&
               input[[paste0("param_selector_x_",plot_index)]] != input[[paste0("param_selector_y_",plot_index)]]) {
            
            ids <- intersect(ids, param_space_clicked$data[[plot_index]][y1 <= point[2] & y2 > point[2], row_id])
        }
        ids <- unique(loading_ps_file()$params[row_id %in% ids,id])
        blue_ids <- loading_ps_file()$param_space[(param+1) %in% ids & formula==chosen_ps_formulae_clean(),state+1]
        states <- states[id %in% blue_ids]
        
        rect(states[[paste0("V",index_x*2-1)]], 0, states[[paste0("V",index_x*2)]], 1,
             border="blue", col=NA, lwd=1.5)
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
# TODO: for now I suppose that one dimension is variable and one is parameter
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
                           counter=input$process_run,
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
                                ids <- intersect(ids, dt[get(paste0(di,1)) <= sid & get(paste0(di,2)) >= sid, id])
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
                           counter=input$process_run,
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
                        # if(length(param_space_clicked$point) >= plot_index && !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]]))) {
                        #     if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) {
                        #         param_space_clicked$point[[plot_index]][[params[[x]] ]] <- c(input[[paste0("scale_slider_ps_",plot_index,"_",x)]],
                        #                                                                      input[[paste0("scale_slider_ps_",plot_index,"_",x)]])
                        #     } else {
                        #         param_space_clicked$point[[plot_index]][[params[[x]] ]] <- c(param_ranges()[[params[[x]] ]])
                        #     }
                        # }
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
                # param_space_clicked$data[[plot_index]] <- copy(ps)
                
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
                        # timing <- system.time({
                        #     rang_x <- range(uniq_x)
                        #     rang_y <- range(uniq_y)
                        #     dt <- dt[x <= rang_x[2] & x >= rang_x[1] & y <= rang_y[2] & y >= rang_y[1] ]
                        #     if(nrow(uniq_x) < nrow(uniq_y)) {    # merge over the axis which has less unique intervals: (x1,x2) or (y1,y2)
                        #         setkey(ps,x1,x2)
                        #         one <- foverlaps(dt[,.(x=x,y=y,xe=x,ye=y)],ps,by.x = c("x","xe"),type="within")[y1<=y & y2>=y,.(cov=length(unique(id))),by=.(x,y)]
                        #     } else {
                        #         setkey(ps,y1,y2)
                        #         one <- foverlaps(dt[,.(x=x,y=y,xe=x,ye=y)],ps,by.x = c("y","ye"),type="within")[x1<=x & x2>=x,.(cov=length(unique(id))),by=.(x,y)]
                        #     }
                        #     dt <- merge(dt,one,by.x=c("x","y"),by.y=c("x","y"))
                        #     rm(one)
                        # })
                        # print(timing)
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
            points(point[[index_x]], point[[index_y]],
                   col=param_space_clicked_point$color, pch=param_space_clicked_point$type, ps=param_space_clicked_point$size, lwd=param_space_clicked_point$width)
            # rect(point[[name_x]][1], point[[name_y]][1], point[[name_x]][2], point[[name_y]][2], col=param_space_clicked_point$color, lwd=param_space_clicked_point$width)
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
                           counter=input$process_run,
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
                        # else {
                        #     sid <- input[[paste0("scale_slider_ps_",plot_index,"_",x)]] # right param value in dimension x
                        #     ids <- intersect(ids, ps[get(paste0("V",x*2-1)) <= sid[1] & get(paste0("V",x*2)) > sid[1] |
                        #                              get(paste0("V",x*2-1)) >= sid[1] & get(paste0("V",x*2)) <= sid[2] |
                        #                              get(paste0("V",x*2-1)) < sid[2] & get(paste0("V",x*2)) >= sid[2], row_id])
                        # }
                        
                        # if(length(param_space_clicked$point) >= plot_index && !(is.null(param_space_clicked$point[[plot_index]]) || is.na(param_space_clicked$point[[plot_index]]))) {
                        #     if(!is.null(input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) && input[[paste0("scale_switch_ps_",plot_index,"_",x)]]) {
                        #         param_space_clicked$point[[plot_index]][[params[[x]] ]] <- c(input[[paste0("scale_slider_ps_",plot_index,"_",x)]],
                        #                                                                      input[[paste0("scale_slider_ps_",plot_index,"_",x)]])
                        #     } else {
                        #         param_space_clicked$point[[plot_index]][[params[[x]] ]] <- c(param_ranges()[[params[[x]] ]])
                        #     }
                        # }
                    } else {
                        ids <- intersect(ids, ps[x1 < range_x[2] & x2 >= range_x[2] |
                                                 x1 <= range_x[1] & x2 > range_x[1] |
                                                 x1 >= range_x[1] & x2 <= range_x[2], row_id])
                    }
                }        # incremental intersection of ids in order to get right ids
                ps <- ps[row_id %in% ids & id %in% loading_ps_file()$param_space[formula==chosen_ps_formulae_clean() & (state+1) %in% st_ids, param+1 ] ]
                # param_space_clicked$data[[plot_index]] <- copy(ps)
                
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


output$chosen_ps_states_ui <- renderUI({
    if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$param_space) != 0) {
        formulae_list <- loading_ps_file()$formulae
        selected_formula <- 1 #formulae_list[which(max(nchar(formulae_list)) == nchar(formulae_list))]     # initially selecting the longest formulae
        
        widgets <- list()
        widgets[[1]] <- selectInput("chosen_ps_formula","choose formula of interest:",formulae_list,selected_formula,selectize=F,size=1,width="100%")
        do.call(tagList,widgets)
    } else
        h3("Parameter synthesis has to be run or result file loaded before showing some results")
})


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
# clicked_in_ps <- observe({
#     if(!is.null(loading_ps_file()) ) {
#         params <- loading_ps_file()$param_names
#         list_of_all_names <- c(loading_ps_file()$param_names, loading_ps_file()$var_names)
#         for(i in visible_ps_plots()) {
#             clicked_point <- input[[paste0("ps_",i,"_dblclick")]]
#             if(!is.null(clicked_point)) isolate({
#                 cat("clicked in ps ",i,": ",clicked_point$x,",",clicked_point$y,"\n")
#                 if(input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names) {
#                     # this part is for mixed Param-space plot (with 1 parameter and 1 variable)
# #                     param_space_clicked$point[[i]] <- sapply(1:length(list_of_all_names), function(t) {
# #                         if(list_of_all_names[t] == input[[paste0("param_selector_x_",i)]]) return(clicked_point$x)
# #                         if(list_of_all_names[t] == input[[paste0("param_selector_y_",i)]]) return(clicked_point$y)
# #                         if(list_of_all_names[t] %in% params) return(input[[paste0("scale_slider_ps_",i,"_",t)]])
# #                         else return(input[[paste0("scale_slider_ps_",i,"_",t)]])
# #                     })
#                     param_space_clicked$point[[i]] <- lapply(1:length(list_of_all_names), function(t) {
#                         if(list_of_all_names[t] == input[[paste0("param_selector_x_",i)]]) return(c(clicked_point$x, clicked_point$x))
#                         if(list_of_all_names[t] == input[[paste0("param_selector_y_",i)]]) return(c(clicked_point$y, clicked_point$y))
#                         if(list_of_all_names[t] %in% params) {
#                             if(input[[paste0("scale_switch_ps_",i,"_",t) ]])    return(c(input[[paste0("scale_slider_ps_",i,"_",t)]],input[[paste0("scale_slider_ps_",i,"_",t)]]))
#                             else                                                return(param_ranges()[[list_of_all_names[t] ]])
#                         } else {
#                             if(input[[paste0("scale_switch_ps_",i,"_",t) ]])    return(c(input[[paste0("scale_slider_ps_",i,"_",t)]],input[[paste0("scale_slider_ps_",i,"_",t)]]))
#                             else                                                return(range(loading_ps_file()$thresholds[[list_of_all_names[t] ]]))
#                         }
#                     })
#                     names(param_space_clicked$point[[i]]) <- list_of_all_names
#                 } else {
#                     # this part is for normal Param-space plot (with 2 parameters)
# #                     param_space_clicked$point[[i]] <- sapply(1:length(params), function(t) {
# #                         if(params[t] == input[[paste0("param_selector_x_",i)]]) return(clicked_point$x)
# #                         if(params[t] == input[[paste0("param_selector_y_",i)]]) return(clicked_point$y)
# #                         return(input[[paste0("scale_slider_ps_",i,"_",t)]])
# #                     })
#                     param_space_clicked$point[[i]] <- lapply(1:length(params), function(t) {
#                         if(params[t] == input[[paste0("param_selector_x_",i)]]) return(c(clicked_point$x, clicked_point$x))
#                         if(params[t] == input[[paste0("param_selector_y_",i)]]) return(c(clicked_point$y, clicked_point$y))
#                         if(input[[paste0("scale_switch_ps_",i,"_",t) ]])    return(c(input[[paste0("scale_slider_ps_",i,"_",t)]],input[[paste0("scale_slider_ps_",i,"_",t)]]))
#                         else                                                return(param_ranges()[[params[t] ]])
#                     })
#                     names(param_space_clicked$point[[i]]) <- params
#                 }
#             })
# #             if(length(param_space_clicked$point) >= i && !is.null(param_space_clicked$point[[i]]) && !is.na(param_space_clicked$point[[i]])) {
# #                 if(input[[paste0("param_selector_y_",i)]] %in% loading_ps_file()$var_names || input[[paste0("param_selector_x_",i)]] %in% loading_ps_file()$var_names) {
# #                     for(t in 1:length(list_of_all_names)) {
# #                         if(!list_of_all_names[t] %in% c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]]) ) {
# #                             if(list_of_all_names[t] %in% params) {
# #                                 if(!is.null(input[[paste0("scale_switch_ps_",i,"_",t) ]]) && input[[paste0("scale_switch_ps_",i,"_",t) ]])
# #                                     param_space_clicked$point[[i]] <- (c(input[[paste0("scale_slider_ps_",i,"_",t)]],input[[paste0("scale_slider_ps_",i,"_",t)]]))
# #                                 else
# #                                     param_space_clicked$point[[i]] <- (param_ranges()[[list_of_all_names[t] ]])
# #                             } else {
# #                                 if(!is.null(input[[paste0("scale_switch_ps_",i,"_",t) ]]) && input[[paste0("scale_switch_ps_",i,"_",t) ]])
# #                                     param_space_clicked$point[[i]] <- (c(input[[paste0("scale_slider_ps_",i,"_",t)]],input[[paste0("scale_slider_ps_",i,"_",t)]]))
# #                                 else
# #                                     param_space_clicked$point[[i]] <- (range(loading_ps_file()$thresholds[[list_of_all_names[t] ]]))
# #                             }
# #                         }
# #                     }
# #                     names(param_space_clicked$point[[i]]) <- list_of_all_names
# #                 } else {
# #                     for(t in 1:length(params) ) {
# #                         if(!params[t] %in% c(input[[paste0("param_selector_x_",i)]],input[[paste0("param_selector_y_",i)]]) ) {
# #                             if(!is.null(input[[paste0("scale_switch_ps_",i,"_",t) ]]) && input[[paste0("scale_switch_ps_",i,"_",t) ]])
# #                                 param_space_clicked$point[[i]] <- (c(input[[paste0("scale_slider_ps_",i,"_",t)]],input[[paste0("scale_slider_ps_",i,"_",t)]]))
# #                             else
# #                                 param_space_clicked$point[[i]] <- (param_ranges()[[params[t] ]])
# #                         }
# #                     }
# #                     names(param_space_clicked$point[[i]]) <- params
# #                 }
# #             }
#         }
#     }
# })
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
                        ids <- intersect(ids, states[get(paste0("V",x*2-1)) <= sid & get(paste0("V",x*2)) > sid,id])
                    }
                }        # incremental intersection of ids in order to get right ids
                states <- states[id %in% ids]
                
                point <- input[[paste0("param_ss_",i,"_dblclick")]]
                if(!is.null(point) ) isolate({
                    cat("param_ss_plot ",i,":",point$x,",",point$y,"\n")
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


chosen_ps_formulae_clean <- reactive({
    return(input$chosen_ps_formula)
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

# param_ranges_sat_for_formula <- reactive({
#     if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$param_space) != 0) {
#         n <- length(loading_ps_file()$param_names)
#         sat <- satisfiable_param_space_for_formula()
#         list <- lapply(1:n,function(i) sort(unique(unlist(sat[,.(get(paste0("V",i*2-1)),get(paste0("V",i*2)))]))))
#         names(list) <- loading_ps_file()$param_names
#         return(list)
#     } else return(NULL)
# })

param_ranges <- reactive({
    if(!is.null(loading_ps_file()) && nrow(loading_ps_file()$params) != 0) {
        # if(loading_ps_file()$type == "smt") {
        #     return(list(deg_x=c(0,1),k1=c(0,2)))    # TODO: temporary
        # } else {
        #     n <- length(loading_ps_file()$param_names)
        #     list <- lapply(1:n,function(i) range(loading_ps_file()$params[,.(get(paste0("V",i*2-1)),get(paste0("V",i*2)))]))
        #     names(list) <- loading_ps_file()$param_names
        #     return(list)
        # }
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