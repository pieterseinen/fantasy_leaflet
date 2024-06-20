library(shinyWidgets)
library(shiny)
library(sp)
library(shinydashboard)
library(leaflet)
library(stringr)
library(htmlwidgets)
library(glue)
library(png)
library(dplyr)
library(shinycssloaders)
library(colourpicker)


#Default aweseomeIconlist for iconinput
default_marker_icons <- awesomeIconList(
  kasteel = makeAwesomeIcon(icon = "fa-solid fa-chess-rook", markerColor = "white", iconColor = "brown", library = "fa"),
  dungeon = makeAwesomeIcon(icon = "fa-solid fa-dungeon", markerColor = "white", iconColor = "brown", library = "fa"),
  boom = makeAwesomeIcon(icon = "fa-solid fa-tree", markerColor = "white", iconColor = "darkgreen", library = "fa"),
  draak = makeAwesomeIcon(icon = "fa-dragon", markerColor = "white", iconColor = "darkred",library = "fa"),
  berg = makeAwesomeIcon(icon = "fa-mountain", markerColor = "white", iconColor = "navy", library = "fa"),
  monument = makeAwesomeIcon(icon = "fa-monument", markerColor = "white",iconColor = "black", library = "fa"),
  kampvuur = makeAwesomeIcon(icon = "fa-fire-alt", markerColor = "white",iconColor = "red", library = "fa"),
  kompas = makeAwesomeIcon(icon = "fa-compass", markerColor = "white", iconColor = "navy", library = "fa"),
  zon = makeAwesomeIcon(icon = "fa-solid fa-sun", markerColor = "white", iconColor = "purple", library = "fa"),
  schedel = makeAwesomeIcon(icon = "fa-skull", markerColor = "white", iconColor = "black", library = "fa"),
  anker = makeAwesomeIcon(icon = "fa-anchor",  markerColor = "white", iconColor = "royalblue", library = "fa"),
  schild = makeAwesomeIcon(icon = "fa-shield-alt", markerColor = "white",iconColor = "darkgreen", library = "fa"),
  hand = makeAwesomeIcon(icon = "fa-solid fa-hand", markerColor = "white", iconColor = "orange", library = "fa"),
  kroon = makeAwesomeIcon(icon = "fa-crown", markerColor = "white",iconColor = "gold", library = "fa"),
  tent = makeAwesomeIcon(icon = "fa-campground", markerColor = "white", iconColor = "darkgreen", library = "fa"),
  paard = makeAwesomeIcon(icon = "fa-horse", markerColor = "white", iconColor = "brown", library = "fa"),
  landmark = makeAwesomeIcon(icon = "fa-landmark", markerColor = "white", iconColor = "navy", library = "fa"),
  tempel = makeAwesomeIcon(icon = "fa-solid fa-place-of-worship", markerColor = "white", iconColor = "brown", library = "fa"),
  hamer = makeAwesomeIcon(icon = "fa-solid fa-hammer", markerColor = "white", iconColor = "brown", library = "fa"),
  ziekenhuis = makeAwesomeIcon(icon = "fa-solid fa-house-medical", markerColor = "white", iconColor = "red", library = "fa"),
  kroeg = makeAwesomeIcon(icon = "fa-solid fa-beer", markerColor = "white", iconColor = "gold", library = "fa")
)

#get al values/names from default iconlist
default_icon_values <- default_marker_icons %>% names()

#make html str for icon input
default_icon_html <- lapply(default_icon_values, function(icon_name){
  
  icon = default_marker_icons[[icon_name]]$icon
  iconColor = default_marker_icons[[icon_name]]$iconColor
  
  glue("<i class='fa {icon}' style='color: {iconColor}'></i>")
  
}) %>% unlist()

names(default_icon_values) <- default_icon_html

#get more fa icons for custom icon input
fa_iconlist = readLines("fontawesome_icons.txt") 

icon_names <- fa_iconlist %>% 
  str_remove("^.fa-")

icon_html <- icon_names

icon_css <- glue("<i class='fa fa-{icon_html}'></i> {icon_html} ")

names(icon_html) <- icon_css


#function that makes an AwesomeIconlist based in icon & color input
#so custom icons can be used as markers
maak_custom_marker_icons <- function(icons, color){
  
  lapply(icons, function(icon_name){
    
    iconlist = awesomeIconList(icon_name = makeAwesomeIcon(icon = paste0("fa-",icon_name), markerColor = "white", iconColor = color, library = "fa"))
    
    iconlist_names <- paste0(icon_name,color)
    
    names(iconlist) <- iconlist_names

    iconlist
    
  }) %>% do.call(c,.)
  
}

#js that adds an image to the leaflet map and maintains aspect ratio
#by calculation image bounds based in input image dimensions w correction for curvature of the earth.
js_leaflet_background_image <- function(image_src,image_dimensions){

glue::glue("
          
          function(el, x) {{
            
          var myMap = this;
          var imageUrl = '{image_src}';
          
          // Get image dimensions from R
          var imgDimensions = [{image_dimensions[2]}, {image_dimensions[1]}];
          
          // Calculate aspect ratio (width / height)
          var aspectRatio = imgDimensions[0] / imgDimensions[1];
          
          // Define the initial bounds
          var latMin = 40;
          var latMax = 50;
          var lngMin = -5;
          var lngMax = 5;
  
          // Calculate the center point
          var latCenter = (latMin + latMax) / 2;
          var lngCenter = (lngMin + lngMax) / 2;
  
          // Calculate the current bounds ranges
          var latRange = latMax - latMin;
          var lngRange = lngMax - lngMin;
  
          // Calculate the distance and correction factor for longitude
          var correctionFactor = Math.cos(latCenter * Math.PI / 180);
  
          // Adjust the bounds to maintain the aspect ratio
          if (latRange / (lngRange * correctionFactor) > aspectRatio) {{
              // Adjust the longitude range to maintain the aspect ratio
              var newLngRange = latRange / aspectRatio;
              lngMin = lngCenter - (newLngRange / 2) / correctionFactor;
              lngMax = lngCenter + (newLngRange / 2) / correctionFactor;
          }} else {{
              // Adjust the latitude range to maintain the aspect ratio
              var newLatRange = lngRange * aspectRatio * correctionFactor;
              latMin = latCenter - (newLatRange / 2);
              latMax = latCenter + (newLatRange / 2);
          }}
  
          var imageBounds = [[latMin, lngMin], [latMax, lngMax]];
  
          // Add the image overlay
          L.imageOverlay(imageUrl, imageBounds).addTo(myMap);
          
          // Adjust the map view to fit the image bounds
          myMap.fitBounds(imageBounds);
          }}")
}
