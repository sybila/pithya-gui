source("config.R")          # global configuration
source("tooltips.R")        # texts

# Other parts of the UI
source("editor/ui.R")
source("explorer/ui.R")
source("result/ui.R")


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

shinyUI(
    fluidPage(
        useShinyjs(),
        tags$head(tags$script(HTML(JS.parameterSynthesisFinished))),
        tags$head(tags$script(HTML(JS.missingThreshold))),
        titlePanel(Tool_name),
        tags$hr(),
        tabsetPanel(id = "dimensions",
            editorTab(),
            explorerTab(),
            resultTab()
        )
    )
)
              
              
              