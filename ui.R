ui <- dashboardPage(
  
  dashboardHeader(
    title = "Fantasy Map"), #end of header

  dashboardSidebar(
    #triggers modal for entering background image url
    actionButton("open_img_input","New background image URL"),
    
    #triggers modal for saving map as .Rdata
    actionButton("save","Save as Rdata"),
    
    #triggers modal for exporting map as .html
    actionButton("export","Export map as html"),
    
    switchInput("drawing",
                onLabel = "Draw Districts",
                offLabel = "Add Markers"),
    
    actionButton("confirm_districts","Confirm Districts"),

    #fileinput for loading map from Rdata
    fileInput("map_file", "Load saved map from Rdata",accept = ".Rdata"),
    
    #github link 
    HTML("
    <div id='my-box'>
    <a href='https://github.com/pieterseinen/fantasy_leaflet' class='fill-div'>
        <i class='fa-solid fa-code'></i> View Code
    </a>
    </div>

    "
    ),
    
    #buymeacoffee link
    HTML(
      "<a href='https://www.buymeacoffee.com/pieterseinen'>
      <img id='coffee-link' src='https://img.buymeacoffee.com/button-api/?text=
      Buy me a coffee&emoji=&slug=pieterseinen&button_colour=FFDD00&font_colour=000000&font_family=Cookie&outline_colour=000000&coffee_colour=ffffff'
      
      />
      </a>"
      )
    ), #end of sidebar
  
  dashboardBody(
    shinyjs::useShinyjs(),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = "custom.css")),
    
    tags$script(HTML("
    $(document).on('change', '#image_url', function() {
      var imgUrl = $(this).val();
      if (imgUrl) {
        var img = new Image();
        img.onload = function() {
          var dimensions = [img.width, img.height];
          Shiny.setInputValue('img_dimensions', dimensions);
        };
        img.src = imgUrl;
      }
    });
    ")),
    
    
    #leafletmap output
    leafletOutput("mymap", width = "100%", height = "100vh") %>%
      withSpinner(image = "spinner.svg")
    ),#end of body
)#end of ui
