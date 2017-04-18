source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")

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
        fluidPage(
            fluidRow(
                column(10, titlePanel(Tool_name)),
                # column(2, class = "advanced_checkbox", 
                #     tooltip(tooltip = Editor_advancedSettings_tooltip,
                #         checkboxInput("advanced", Editor_advancedSettings_label, F)
                #     )
                # ),
                column(2, a(img(src = "logo_new.png", height = 60, width = 165, style = "float:right"), href = "http://sybila.fi.muni.cz/home"))
            )
        ),
        tags$hr(),
        fluidRow( 
            # Area prepared for Experiments management (Prev, Next, Save, Load, etc.)
            column(2,
                   tooltip(tooltip = Editor_advancedSettings_tooltip,
                           checkboxInput("advanced", Editor_advancedSettings_label, F)
                   ), offset = 0
            )
        ),
        tabsetPanel(id = "dimensions",
            editorTab(),
            explorerTab(),
            resultTab()
        )
    )
)
              
              
              