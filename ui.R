
#### Define UI
ui <- dashboardPage(
  # Somewhere in UI

  dashboardHeader(

    title = "Fantasy Map"),
  # Sidebar layout with input and output definitions 
  dashboardSidebar(
    actionButton("open_img_input","New background image URL"),
    actionButton("custom_icons","Custom Icons"),

    #schrijf_kaart_weg
    actionButton("opslaan","Save as Rdata"),
    actionButton("export","Export map as html"),
    
    #laad kaart
    fileInput("map_file", "Load saved map from Rdata",
              accept = ".Rdata"),
    
    HTML(
      '<a href="https://www.buymeacoffee.com/pieterseinen">
      <img src="https://img.buymeacoffee.com/button-api/?text=Buy me a coffee&emoji=&slug=pieterseinen&button_colour=FFDD00&font_colour=000000&font_family=Cookie&outline_colour=000000&coffee_colour=ffffff"
      width = "90%"
      />
      </a>')),
  
  # Main panel for displaying outputs 
  dashboardBody(
    shinyjs::useShinyjs(),
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

    leafletOutput("mymap", width = "100%", height = "100vh") %>%
      withSpinner(image = "spinner.svg")
      
  ),
)
