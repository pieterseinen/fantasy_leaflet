ui <- dashboardPage(
  
  dashboardHeader(
    title = "Fantasy Map"), #end of header

  dashboardSidebar(
    #triggers modal for entering background image url
    actionButton("open_img_input","New background image URL"),
    
    #triggers modal for saving map as .Rdata
    actionButton("opslaan","Save as Rdata"),
    
    #triggers modal for exporting map as .html
    actionButton("export","Export map as html"),
    
    #fileinput for loading map from Rdata
    fileInput("map_file", "Load saved map from Rdata",accept = ".Rdata"),
    
    #buymeacoffee link
    HTML(
      '<a href="https://www.buymeacoffee.com/pieterseinen">
      <img src="https://img.buymeacoffee.com/button-api/?text=
      Buy me a coffee&emoji=&slug=pieterseinen&button_colour=FFDD00&font_colour=000000&font_family=Cookie&outline_colour=000000&coffee_colour=ffffff"
      width = "90%"
      />
      </a>'
      )
    ), #end of sidebar
  
  dashboardBody(
    shinyjs::useShinyjs(),
    #TODO move to .css file
    tags$style(
      "
      #edit{
      position: absolute;
      right:10%;
      z-index:1000;
      float: right;
      top: 0%;
      }
      
      .leaflet-popup-content{
      position: relative;
      }
      "
      ),
    
    #TODO move to .js file? 
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
