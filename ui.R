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
        tags$head(HTML("
                <script>
                  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

                  ga('create', 'UA-10610329-11', 'auto');
                  ga('send', 'pageview');

                </script>
        ")),
        useShinyjs(),
        # loading(file.path(mytempdir(), "image.Rmd")),
        # fluidPage(
        fluidRow(
            column(8, titlePanel(Tool_name)),
            # column(2, class = "advanced_checkbox", 
            #     tooltip(tooltip = Editor_advancedSettings_tooltip,
            #         checkboxInput("advanced", Editor_advancedSettings_label, F)
            #     )
            # ),
            column(2, a(img(src = "aec_badge_cav.svg", height = 60, width = 60, style = "float:left; padding: 5px 5px 5px 5px"), href = "http://cavconference.org/2017/accepted-papers/", target="_blank")),
            column(2, a(img(src = "logo_new.png", height = 60, width = 165, style = "float:right"), href = "http://sybila.fi.muni.cz/home", target="_blank"))
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
                   style = "visibility: hidden;",
                   tooltip(tooltip = Editor_advancedSettings_tooltip,
                           checkboxInput("advanced", Editor_advancedSettings_label, F)
                   ), offset = 0
            ),
            column(4, a(code(("! Click here for tutorial !")), target = "_blank", href = "http://biodivine.fi.muni.cz/docs/pithya/tutorial.pdf"), offset = 3)
        ),
        tabsetPanel(id = "dimensions",
            editorTab(),
            explorerTab(),
            resultTab()
        )
    )
)
              
              
              