Tool_name <- "PITHYA - Parameter Investigation Tool for HYbrid Analysis"

#PROGRESS NOTIFICATIONS - in server.R
Starting_advice <- "Start with button 'generate approximation'.\n"

Approximation_started <- "Approximation is started\n"
Approximation_running <- "Model approximation is running..."
Approximation_finished <- "Approximation is finished\n"
Approximation_error <- "Some error occured during approximation!\n"

Parameter_synthesis_started <- "Parameter synthesis has started\n"
Parameter_synthesis_stopped <- "Process was stopped!\n"

Waiting_for_state_space <- "Waiting for transition-state space"


#EDITOR
Editor_label <- "Edit & Check" # "Edit & Run" "Editor" "Model Editor"
Editor_tooltip <- "" # "'Model editor' allows the user load, edit or save input model. Simple model is automatically loaded as example."

Editor_advancedSettings_label <- "advanced settings"
Editor_advancedSettings_tooltip <- "show/hide advanced settings"

Editor_model_Browse_label <- ""
Editor_model_Browse_tooltip <- "Load .bio model file."

Editor_model_resetChangesInModel_label <- "reset changes in model"
Editor_model_resetChangesInModel_tooltip <- ""

Editor_model_saveModel_label <- "save model"
Editor_model_saveModel_tooltip <- "Save model description as .bio file."

Editor_property_Browse_label <- ""
Editor_property_Browse_tooltip <- "Load .bio file for properties."

Editor_property_resetChangesInProperties_label <- "reset changes in properties"
Editor_property_resetChangesInProperties_tooltip <- ""

Editor_property_saveProperties_label <- "save properties"
Editor_property_saveProperties_tooltip <- "Save properties description as file."

Editor_cutTresholds_label <- "cut tresholds"
Editor_cutTresholds_tooltip <- "During The-PWA-approximation of special functions used inside the model new thresholds are generated and some of them could exceed explicit ones. Check to not allow exceeding thresholds.
"

Editor_fastApproximation_label <- "fast approximation"
Editor_fastApproximation_tooltip <- "" # "Two versions of The-PWA-approximation are available. Slower one - more precise and computationally more demanding - and fast one - much faster but also less precise."

Editor_generateApproximation_label <- "generate approximation"
Editor_generateApproximation_tooltip <- "" #TODO

Editor_progressBar_label <- "progress bar:"
Editor_progressBar_tooltip <- "Shows progress and miscellaneous usefull information."

Editor_modelTextEditor_label <- "model editor:"
Editor_modelTextEditor_tooltip <- "" #TODO

Editor_propertyTextEditor_label <- "properties editor:"
Editor_propertyTextEditor_tooltip <- "" #TODO

Editor_numberOfThreads_label <- "no. of threads"
Editor_numberOfThreads_tooltip <- "Sets number of threads to compute model exploration with."

Editor_runParameterSynthesis_label <- "run parameter synthesis"
Editor_runParameterSynthesis_tooltip <- "Runs parameter synthesis."

Editor_stopParameterSynthesis_label <- "stop parameter synthesis"
Editor_stopParameterSynthesis_tooltip <- "Stops parameter synthesis."

#EXPLORER
Explorer_label <- "Explore" # "Explore & Show" "Explorer" "Model Explorer"
Explorer_tooltip <- ""

Explorer_nextExperiment_label <- "next"
Explorer_nextExperiment_tooltip <- ""

Explorer_previousExperiment_label <- "previous"
Explorer_previousExperiment_tooltip <- ""

Explorer_deleteExperiment_label <- "delete"
Explorer_deleteExperiment_tooltip <- ""

Explorer_saveExperimentModel_label <- "save current model"
Explorer_saveExperimentModel_tooltip <- "This button saves model description in *.bio format of current experiment."

Explorer_countOfDirectionArrows_label <- "count of direction arrows"
Explorer_countOfDirectionArrows_tooltip <- "Total number of direction arrows per dimension inside vector field(s)."

Explorer_lengthOfDirectionArrows_label <- "length of direction arrows"
Explorer_lengthOfDirectionArrows_tooltip <- "Scaling factor for length of direction arrows inside vector field(s)."

Explorer_coloringThreshold_label <- "coloring threshold"
Explorer_coloringThreshold_tooltip <- "Maximal magnitude of vector to be considered neutral (vector with black colour)." # "Maximal magnitude of neutral vector."

Explorer_widthOfAllArrows_label <- "width of all arrows"
Explorer_widthOfAllArrows_tooltip <- "Scaling factor for width of arrows inside vector field(s) and state space(s)."

Explorer_coloringDirection_label <- "coloring direction"
Explorer_coloringDirection_tooltip <- "Choose direction(s) to color."

Explorer_addPlot_label <- "add plot"
Explorer_addPlot_tooltip <- "Add new plot for vector field and state space."

#WARNING: these are used in server.R
Explorer_experiment_label <- "Experiment no. "

Explorer_parameter_label <- "parameter "
Explorer_parameter_tooltip <- "Sets parameter value."

Explorer_horizontal_label <- "horizontal axis"
Explorer_horizontal_tooltip <- "Set variable to show on horizontal axis."

Explorer_vertical_label <- "vertical axis"
Explorer_vertical_tooltip <- "Set variable to show on vertical axis."

Explorer_cancel_label <- "delete"
Explorer_cancel_tooltip <- "Delete this plot."

Explorer_hide_label <- "hide"
Explorer_hide_tooltip <- "Hide/show this plot."

Explorer_VF_label <- "vector field of model:"
Explorer_VF_tooltip <- ""                                           #TODO: probably remove

Explorer_VF_ApplyToAll_label <- "apply to all"
Explorer_VF_ApplyToAll_tooltip <- "Apply this starting point to all vector field plots."

Explorer_VF_ApplyToTSS_label <- "apply to SS"
Explorer_VF_ApplyToTSS_tooltip <- "Apply this starting point to corresponding state space plot."

Explorer_VF_ClearPlot_label <- "clear plot"
Explorer_VF_ClearPlot_tooltip <- "Clear this plot."

Explorer_VF_Unzoom_label <- "unzoom"
Explorer_VF_Unzoom_tooltip <- "Unzoom this vector field plot."

Explorer_VF_UsePWAmodel_label <- "use PWA model"
Explorer_VF_UsePWAmodel_tooltip <- "" #TODO

Explorer_VF_ScaleSlider_label <- "continues scale in "
Explorer_VF_ScaleSlider_tooltip <- "" 

Explorer_VF_HoverTextArea_label <- ""                               #TODO: probably remove
Explorer_VF_HoverTextArea_tooltip <- ""

Explorer_SS_label <- "state space:" #"state space of the model:"
Explorer_SS_tooltip <- ""                                           #TODO: probably remove
Explorer_SS_error <- "Approximation has to be generated before showing state space"

Explorer_SS_ApplyToAll_label <- "apply to all"
Explorer_SS_ApplyToAll_tooltip <- "Apply this starting point to all state space plots."

Explorer_SS_ClearPlot_label <- "clear plot"
Explorer_SS_ClearPlot_tooltip <- "Clear this plot."

Explorer_SS_Unzoom_label <- "unzoom"
Explorer_SS_Unzoom_tooltip <- "Unzoom this state space plot."

Explorer_SS_ScaleSlider_label <- "discrete scale in "
Explorer_SS_ScaleSlider_tooltip <- ""

Explorer_SS_HoverTextArea_label <- ""                               #TODO: probably remove
Explorer_SS_HoverTextArea_tooltip <- ""
#WARNING: these are used in server.R - END

#Explorer_SS__label <- ""
#Explorer_SS__tooltip <- ""

#Explorer__label <- ""
#Explorer__tooltip <- ""

#RESULT
Result_label <- "Result" # "Result Explorer" 
Result_tooltip <- ""

Result_nextExperiment_label <- "next"
Result_nextExperiment_tooltip <- ""

Result_previousExperiment_label <- "previous"
Result_previousExperiment_tooltip <- ""

Result_deleteExperiment_label <- "delete"
Result_deleteExperiment_tooltip <- ""

Result_Browse_label <- "" # "choose result '.json' file"
Result_Browse_tooltip <- "Load parameter synthesis result file."

Result_BrowseReload_label <- "reload"
Result_BrowseReload_tooltip <- ""

Result_saveResults_label <- "save results"
Result_saveResults_tooltip <- "Save parameter synthesis results as file."

Result_showParametersCoverage_label <- "show parameters coverage"
Result_showParametersCoverage_tooltip <- "Scaling factor for density of shown rectangles in parameter space."

Result_greyShadeDegree_label <- "colour shade degree"
Result_greyShadeDegree_tooltip <- "Scaling factor in the range <0,1> for shade of colour representing parameters."

Result_parameterDensity_label <- "density"
Result_parameterDensity_tooltip <- "Scaling factor in the range <10,150> for density of shown rectangles in parameter space."

Result_addPlot_label <- "add plot"
Result_addPlot_tooltip <- "Add new plot for parameter space and satisfying state space."

#WARNING: these are in server.R
Result_experiment_label <- "Experiment no. "

Result_chooseFormulaOfInterest_label <- "choose formula:"
Result_chooseFormulaOfInterest_tooltip <- "Choose formula to check."
Result_chooseFormulaOfInterest_error <- "Parameter synthesis has to be run or result file loaded before showing some results"

Result_horizontal_label <- "horizontal axis"
Result_horizontal_tooltip <- "Set variable to show on horizontal axis."

Result_vertical_label <- "vertical axis"
Result_vertical_tooltip <- "Set variable to show on vertical axis."

Result_cancel_label <- "delete"
Result_cancel_tooltip <- "Delete this plot."

Result_hide_label <- "hide"
Result_hide_tooltip <- "Hide/show this plot."

Result_noParameterSelected_error <- "Please select at least one parameter."

Result_PS_label <- "parameter space of the model"
Result_PSmixed_label <- "parameter-variable dependency plot:"
Result_PS_tooltip <- ""                     #TODO: probably remove

Result_PS_DeselectClick_label <- "deselect click"
Result_PS_DeselectClick_tooltip <- ""

Result_PS_Unzoom_label <- "unzoom"
Result_PS_Unzoom_tooltip <- "Unzoom this vector field plot."

Result_PS_ScaleSlider_label <- "scale in "
Result_PS_ScaleSlider_tooltip <- "" #TODO
Result_PS_ScaleSwitch_tooltip <- ""

Result_PS_HoverTextArea_label <- ""         #TODO: probably remove
Result_PS_HoverTextArea_tooltip <- ""

Result_SS_label <- "satisfying state space:" # "state space of the model:"  "... and corresponding transition-state space"
Result_SS_tooltip <- ""                     #TODO: probably remove

Result_SS_DeselectAll_label <- "deselect all"
Result_SS_DeselectAll_tooltip <- "" #TODO

Result_SS_Unzoom_label <- "unzoom"
Result_SS_Unzoom_tooltip <- "Unzoom this state space plot."

Result_SS_horizontal_label <- "horizontal axis"
Result_SS_horizontal_tooltip <- "Set variable to show on horizontal axis."

Result_SS_vertical_label <- "vertical axis"
Result_SS_vertical_tooltip <- "Set variable to show on vertical axis."

Result_SS_ScaleSlider_label <- "scale in "
Result_SS_ScaleSlider_tooltip <- ""

Result_SS_HoverTextArea_label <- ""         #TODO: probably remove
Result_SS_HoverTextArea_tooltip <- ""
#WARNING: it is used in server.R - END

#Result__label <- ""
#Result__tooltip <- ""




