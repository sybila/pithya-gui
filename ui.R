source("config.R")          # global configuration
source("tooltips.R")        # texts
source("ui_global.R")
source("util.R")

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
        ### Working solution for re-loading of fileInput
        # tags$script('
        # Shiny.addCustomMessageHandler("loader_reset", function(x) {   
        #     var el = $("#" + x);
        #     el.replaceWith(el = el.clone(true));
        #     var id = "#" + x + "_progress";     
        #     $(id).css("visibility", "hidden");
        # });
        # '),
        useShinyjs(),
        # loading(file.path(mytempdir(), "image.Rmd")),
        # fluidPage(
        fluidRow(
            column(10, titlePanel(Tool_name)),
            # column(2, class = "advanced_checkbox", 
            #     tooltip(tooltip = Editor_advancedSettings_tooltip,
            #         checkboxInput("advanced", Editor_advancedSettings_label, F)
            #     )
            # ),
            column(2, a(img(src = "logo_new.png", height = 60, width = 165, style = "float:right"), href = "http://sybila.fi.muni.cz/home"))
        # )
        ),
        tags$hr(),
        fluidRow( 
            # Area prepared for Experiments management (Prev, Next, Save, Load, etc.)
            # column(1,bsButton("btn_prev","Prev")),
            # column(1,bsButton("btn_next","Next")),
            # column(1,bsButton("btn_load","Load")),
            # column(1,bsButton("btn_save","Save")),
            # column(1,bsButton("btn_remove","Remove")),
            
            # column(1,bsButton("btn_test_export","export")),
            # column(1,bsButton("btn_test_import","import")),
            
            column(2,
                   #style = "margin-top:5px;",
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
              
              
              