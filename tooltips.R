source("config.R")
Tool_name <- "PITHYA: Parameter Investigation Tool"

#PROGRESS NOTIFICATIONS - in server.R
Starting_advice <- "" #"Start with button 'generate approximation'.\n"

Approximation_started <- "Approximation is started.\n"
Approximation_running <- "Model approximation in progress..."
Approximation_finished <- "Approximation is finished.\n"
Approximation_error <- "Some error occured during approximation.\n"

Parameter_synthesis_started <- "Parameter synthesis has started.\n"
Parameter_synthesis_stopped <- "Parameter synthesis process was stopped.\n"

TC_analysis_started <- "Attractor analysis has started.\n"
TC_analysis_stopped <- "Attractor analysis process was stopped.\n"

Waiting_for_state_space <- "Updating plots." #comment both in adding plot and delteing


#EDITOR
Editor_label <- "Editor" #"Edit & Check" # "Edit & Run" "Model Editor"
Editor_tooltip <- "" # "'Model editor' allows the user load, edit or save input model. Simple model is automatically loaded as example."

Editor_model_controlPanel_label <- "Model Editor Control Panel"

Editor_advancedSettings_label <- "Advanced Settings"
Editor_advancedSettings_tooltip <- "show/hide settings allowing advanced features" #"show/hide advanced settings"

Editor_selectExample_label <- "Select predefined example"
Editor_selectExample_tooltip <- "Select an example to investigate its model, the properties (if any) and the particular results (if any)"

Editor_model_Browse_label <- ""
Editor_model_Browse_tooltip <- "Load .bio model file."

Editor_model_resetChangesInModel_label <- "Reload Model"
Editor_model_resetChangesInModel_tooltip <- "Reload uploaded model file."

Editor_model_saveModel_label <- "Save Model"
Editor_model_saveModel_tooltip <- "Save model as .bio file."

Editor_generateApproximation_label <- "Generate Approximation"
Editor_generateApproximation_tooltip <- "Run the approximation method to compute the PMA model."

Editor_property_controlPanel_label <- "Properties Editor Control Panel"

Editor_property_Browse_label <- ""
Editor_property_Browse_tooltip <- "Load .ctl file containing properties."

Editor_property_resetChangesInProperties_label <- "Reload Properties"
Editor_property_resetChangesInProperties_tooltip <- "Reload uploaded properties file."

Editor_property_saveProperties_label <- "Save Properties"
Editor_property_saveProperties_tooltip <- "Save properties as .ctl file."

Editor_cutTresholds_label <- "Cut Thresholds"
Editor_cutTresholds_tooltip <- "Do not allow automatic adding of thresholds exceeding the maximal threshold set in the model file." #During the approximation of special functions used inside the model new thresholds are generated and some of them could exceed explicit ones. Check to not allow exceeding thresholds."

Editor_fastApproximation_label <- "Fast Approximation"
Editor_fastApproximation_tooltip <- "Enable coarser and less precise approximation method." # "Two versions of The-PWA-approximation are available. Slower one - more precise and computationally more demanding - and fast one - much faster but also less precise."

Editor_process_controlPanel_label <- "Analyses Control Panel" # "Parameter Synthesis"

Editor_numberOfThreads_label <- "Number of Threads"
Editor_numberOfThreads_tooltip <- "Set number of threads computing parameter synthesis."

Editor_runParameterSynthesis_label <- "Run Parameter Synthesis"
Editor_runParameterSynthesis_tooltip <- "Run parameter synthesis procedure."

Editor_stopParameterSynthesis_label <- "Stop Parameter Synthesis"
Editor_stopParameterSynthesis_tooltip <- "Stop parameter synthesis procedure."

Editor_progressBar_label <- "Progress Bar"
Editor_progressBar_tooltip <-  "" #"Shows progress and miscellaneous usefull information."

Editor_modelTextEditor_label <- "Model Editor"
Editor_modelTextEditor_tooltip <- ""

Editor_propertyTextEditor_label <- "Properties Editor"
Editor_propertyTextEditor_tooltip <- "" 

#TODO: check and fill
Editor_algorithmType_label    <- "Algorithm Type"
Editor_algorithmType_tooltip  <- "Specify the type of algorithm used in attractor analysis."

Editor_disableHeuristic_label   <- "Disable Heuristic"
Editor_disableHeuristic_tooltip <- "Use to disable the set size state choosing heuristic."

Editor_disableSelfloops_label   <- "Disable Self-Loops"
Editor_disableSelfloops_tooltip <- "Use to disable self-loop creation in transition system."

Editor_runTCAnalysis_label    <- "Run Attractor Analysis"
Editor_runTCAnalysis_tooltip  <- "Run Attractor analysis procedure."


#EXPLORER
Explorer_label <- "Explorer" # "Explore & Show" "Explorer" "Model Explorer"
Explorer_tooltip <- ""

#Explorer_nextExperiment_label <- "next"
#Explorer_nextExperiment_tooltip <- ""

#Explorer_previousExperiment_label <- "previous"
#Explorer_previousExperiment_tooltip <- ""

#Explorer_deleteExperiment_label <- "delete"
#Explorer_deleteExperiment_tooltip <- ""

#Explorer_saveExperimentModel_label <- "save current model"
#Explorer_saveExperimentModel_tooltip <- "This button saves model description in *.bio format of current experiment."

Explorer_countOfDirectionArrows_label <- "Arrows Count"
Explorer_countOfDirectionArrows_tooltip <- "Total number of arrows per dimension inside vector field(s)."

Explorer_lengthOfDirectionArrows_label <- "Arrows Length"
Explorer_lengthOfDirectionArrows_tooltip <- "Scaling factor for length of arrows inside vector field(s)."

Explorer_coloringThreshold_label <- "Colouring Threshold"
Explorer_coloringThreshold_tooltip <- "Vector magnitude value above which vector field arrows are coloured." #"Maximal magnitude of vector to be considered neutral (vector with black colour)." # "Maximal magnitude of neutral vector."

Explorer_widthOfAllArrows_label <- "Arrows Width"
Explorer_widthOfAllArrows_tooltip <- "Scaling factor for width of arrows inside vector field(s) and transition-state space(s)."

Explorer_coloringDirection_label <- "Colouring Orientation"
Explorer_coloringDirection_tooltip <- "Choose vector field arrow component(s) to which colouring is applied."#"Choose direction(s) to colour."

Explorer_flowPointsCount_label <- "Trajectory Points Number"  
#"no. of trajectory points"
Explorer_flowPointsCount_tooltip <- "Set number of points to plot the trajectory. Increase to prolong the trajectory." #TODO: "sets "

Explorer_flowPointsDensity_label <- "Trajectory Points Scaling Factor" 
#"trajectory point density"
Explorer_flowPointsDensity_tooltip <- "Set inverse value of trajectory points density. Decrease to make trajectory plot more precise."

Explorer_addPlot_label <- "Add Plot"
Explorer_addPlot_tooltip <- "Add new plot for vector field and the corresponding transition-state space."

#WARNING: these are used in server.R
# Explorer_experiment_label <- "Experiment number "

Explorer_parameter_label <- "Parameter " # + parameter.name
Explorer_parameter_tooltip <- "Set parameter value."

Explorer_horizontal_label <- "Horizontal Axis"
Explorer_horizontal_tooltip <- "Set the variable to show on horizontal axis."

Explorer_vertical_label <- "Vertical Axis"
Explorer_vertical_tooltip <- "Set the variable to show on vertical axis."

Explorer_cancel_label <- "Delete"
Explorer_cancel_tooltip <- "Delete this plot."

Explorer_hide_label <- "Hide"
Explorer_hide_tooltip <- "Hide/show this plot."

Explorer_VF_label <- "ODE Model Vector Field"
Explorer_VF_label_pwa <- "PMA Model Vector Field"
Explorer_VF_tooltip <- ""                                           #TODO: probably remove

Explorer_VF_ApplyToAll_label <- "Apply to All VF" # 
Explorer_VF_ApplyToAll_tooltip <- "Apply starting trajectory point of this plot to all vector field plots."

Explorer_VF_ApplyToTSS_label <- "Apply to TSS"
Explorer_VF_ApplyToTSS_tooltip <- "Apply starting trajectory point of this plot to the corresponding transition-state space plot on the right."

Explorer_VF_ClearPlot_label <- "Clear Plot"
Explorer_VF_ClearPlot_tooltip <- "Clear this vector field plot."

Explorer_VF_Unzoom_label <- "Unzoom"
Explorer_VF_Unzoom_tooltip <- "Unzoom this vector field plot."

#HIDDEN - advanced settings
Explorer_VF_UsePWAmodel_label <- "Use PMA Model"
Explorer_VF_UsePWAmodel_tooltip <- "Switch the vector field to approximation of the original ODE model."

#HIDDEN - for more at least 3 VARS model
Explorer_VF_ScaleSlider_label <- "Continuous Value of " 
Explorer_VF_ScaleSlider_tooltip <- "Set the respective variable value to which the plot is projected."

Explorer_VF_HoverTextArea_label <- "" #TODO: probably remove
Explorer_VF_HoverTextArea_tooltip <- "" #TODO

Explorer_SS_label <- "Transition-State Space" #"transition-state space of the model:"
Explorer_SS_tooltip <- "" #TODO: probably remove
Explorer_SS_error <- "Approximation has to be generated before showing transition-state space"

Explorer_SS_ApplyToAll_label <- "Apply to All TSS" #TODO
Explorer_SS_ApplyToAll_tooltip <- "Apply starting point of this plot to all transition-state space plots."

Explorer_SS_ClearPlot_label <- "Clear Plot"
Explorer_SS_ClearPlot_tooltip <- "Clear this transition-state space plot."

Explorer_SS_Unzoom_label <- "Unzoom"
Explorer_SS_Unzoom_tooltip <- "Unzoom this transition-state space plot."

Explorer_SS_ScaleSlider_label <- "Discrete Value of " #TODO
Explorer_SS_ScaleSlider_tooltip <- "Set the respective variable value to which the plot is projected."

Explorer_SS_HoverTextArea_label <- "" #TODO: probably remove
Explorer_SS_HoverTextArea_tooltip <- ""
#WARNING: these are used in server.R - END


#RESULT
Result_label <- "Results" # "Result Explorer" 
Result_tooltip <- ""

#Result_nextExperiment_label <- "next"
#Result_nextExperiment_tooltip <- ""

#Result_previousExperiment_label <- "previous"
#Result_previousExperiment_tooltip <- ""

#Result_deleteExperiment_label <- "delete"
#Result_deleteExperiment_tooltip <- ""

Result_Browse_label <- "" # "choose result '.json' file"
Result_Browse_tooltip <- "Import parameter synthesis result .json file."

#Deleted with experiments
#Result_BrowseReload_label <- "reload"
#Result_BrowseReload_tooltip <- ""

Result_loadImportedResults_label    <- "Imported Results"
Result_loadImportedResults_tooltip  <- "Load imported results as current one to display and examine. Green color means new results have been imported."
Result_importedResults_tag          <- "importedResult"

Result_loadSynthResults_label   <- "Synthesis Results"
Result_loadSynthResults_tooltip <- "Load parameter synthesis results as current one to display and examine. Green color means new results have been produced."
Result_synthResults_tag          <- "synthesisResult"

Result_loadTCAResults_label   <- "Attractor Results"
Result_loadTCAResults_tooltip <- "Load attractor analysis results as current one to display and examine. Green color means new results have been produced."
Result_TCAResults_tag          <- "attractorResult"

Result_saveResults_label <- "Save Loaded Results"
Result_saveResults_tooltip <- "Save currently loaded results as .json file."

Result_showParametersCoverage_label <- "Show Parameters Coverage"
Result_showParametersCoverage_tooltip <- "Enable/disable parameters coverage" #"Show Scaling factor for density of shown rectangles in parameter space."

Result_greyShadeDegree_label <- "Colour Shade Degree"
Result_greyShadeDegree_tooltip <- "Set the value in the range <0,1> for shade of colour representing results." #"Set the value in the range <0,1> for shade of colour representing parameters."

Result_parameterDensity_label <- "Resolution"
Result_parameterDensity_tooltip <- "Set value in the range <10,150> for resolution of parameter space plot(s)."

Result_addPlot_label <- "Add Plot"
Result_addPlot_tooltip <- "Add new plot for parameter space and the corresponding satisfying state space."

#WARNING: these are in server.R
#Result_experiment_label <- "Experiment no. "

Result_chooseFormulaOfInterest_label <- "Show results for:"
Result_chooseFormulaOfInterest_tooltip <- "Select the results which should be displayed."

Result_chooseFormulaOfInterest_error <- "An analysis has to be run or a results file imported before results can be shown."

Result_horizontal_label <- "Horizontal Axis"
Result_horizontal_tooltip <- "Set the variable to show on horizontal axis."

Result_vertical_label <- "Vertical Axis"
Result_vertical_tooltip <- "Set the variable to show on vertical axis."

Result_cancel_label <- "Delete"
Result_cancel_tooltip <- "Delete this plot."

Result_hide_label <- "Hide"
Result_hide_tooltip <- "Hide/show this plot."

Result_grid_label <- "Show grid"
Result_grid_tooltip <- "Show/hide grid in this plot."

Result_noParameterSelected_error <- "Please select at least one parameter."

Result_PS_label <- "Parameter Space"
Result_PSmixed_label <- "parameter-variable dependency plot"
Result_PS_tooltip <- ""                     #TODO: probably remove

Result_PS_DeselectClick_label <- "Clear Plot"
Result_PS_DeselectClick_tooltip <- "Clear this plot" #TODO #this plot #parameter space plot #bud alebo 

Result_PS_Unzoom_label <- "Unzoom"
Result_PS_Unzoom_tooltip <- "Unzoom this plot." #TODO #this plot #parameter space plot #bud alebo 

Result_PS_ScaleSlider_label <- "Projection in "
Result_PS_ScaleSlider_tooltip <- "Set the respective variable/parameter value to which the plot is projected." #TODO

Result_PS_ScaleSwitch_tooltip <- ""

Result_PS_HoverTextArea_label <- ""         #TODO: probably remove
Result_PS_HoverTextArea_tooltip <- ""

Result_SS_label <- "Satisfying State Space" # "state space of the model:"  "... and corresponding transition-state space"
Result_SS_tooltip <- ""                     #TODO: probably remove

Result_SS_DeselectAll_label <- "Clear Plot"
Result_SS_DeselectAll_tooltip <- "Clear this plot" #TODO

Result_SS_Unzoom_label <- "Unzoom"
Result_SS_Unzoom_tooltip <- "Unzoom this state space plot."

Result_SS_horizontal_label <- "Horizontal Axis"
Result_SS_horizontal_tooltip <- "Set the variable to show on horizontal axis."

Result_SS_vertical_label <- "Vertical Axis"
Result_SS_vertical_tooltip <- "Set the variable to show on vertical axis."

Result_SS_ScaleSlider_label <- "Value of "
Result_SS_ScaleSlider_tooltip <- "Set the respective variable value to which the plot is projected."

Result_SS_HoverTextArea_label <- ""         #TODO: probably remove
Result_SS_HoverTextArea_tooltip <- ""
#WARNING: it is used in server.R - END

#Result__label <- ""
#Result__tooltip <- ""

