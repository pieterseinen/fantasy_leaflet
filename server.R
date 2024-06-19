server <- function(input, output,session) {

  #TODO server & global refactoren
  #TODO save map functie nakijken. zijn wat dingen aangepast.
  #TODO checken of excess aan observeEvent voor iedere popupinput verwijderd kan worden

# reactiveVals ------------------------------------------------------------
  
  #leaflet map object for saving to Rdata 
  map_reactive <- reactiveVal()
  
  #reactiveVal df to save marker data
  markers_df <- reactiveVal(data.frame(
    group = character(),
    lng = numeric(),
    lat = numeric(),
    label = character(),
    icon = character(),
    content = character(),
    url = character(),
    url_label = character(),
    popup_image_url = character(),
    popup_image_url_label = character(),
    stringsAsFactors = FALSE
  ))
  

  all_marker_icons <- reactiveVal(default_marker_icons)   #awesomeIconList that can be appended with custom icons
  all_marker_icons_values <- reactiveVal(default_icon_values) # named character with <icon> html for both default & custom icons
  custom_marker_icons_values <- reactiveVal() #named character with <icon> html for just custom icons so they can be removed easily
  
# marker customization -------------------------------------------------------------------

#when a marker icon is selected
  #TODO verwijderen en alles in 1x laten confirmen door confirm_marker 
  observeEvent(input$marker_icon,{
    if(!is.null(target_marker_coords())){
      
      #write the icon to the target marker
        markers_df_new <- markers_df()
        markers_df_new$icon[markers_df_new$group == target_marker_group()] <- input$marker_icon
        markers_df(markers_df_new)
      }
    })

#ui output for selecting marker icons
  output$icon_palette <- renderUI({

    radioGroupButtons(
      inputId = "marker_icon",
      label = "Custom Icons",
      choices = all_marker_icons_values())

  })

  #if custom icons are confirmed
  observeEvent(input$confirm_icons,{
    req(input$iconpicker)
    
    #custom icons need to be added to AwesomIconList all__marker_icons() in order to
    #use them in leaflet markers
    
    #create an awesomeIconList for custom markers
    new_custom_marker_icons <- maak_custom_marker_icons(input$iconpicker,input$kleur)
    
    #append new awesomeIconList to existing list
    combined_marker_icons <- do.call(c,list(all_marker_icons(),new_custom_marker_icons))
    
    #update all_marker_icons() with complete iconlist
    all_marker_icons(combined_marker_icons)
    
    #custom icons need to be added to a named chararcter vector all_marker_icons_values() in order
    #to list them in marker_icon input.
    
    new_marker_icon_values <- paste0(input$iconpicker, input$kleur)
    
    new_marker_icon_html <- glue("<i class='fa fa-{input$iconpicker}' style='color: {input$kleur}'></i>")
    
    names(new_marker_icon_values) <- new_marker_icon_html
    
    custom_marker_icons_values(c(custom_marker_icons_values(), new_marker_icon_values))
    
    all_marker_icons_values(c(all_marker_icons_values(),custom_marker_icons_values()))
    
    #set picker selection to emtpy
    updateMultiInput(session, inputId = "iconpicker", selected = "Niks")
    #select last picked icon
    updateRadioGroupButtons(session, inputId =  "marker_icon", selected = tail(new_marker_icon_values,1))


    
  })
  
  observeEvent(input$clear_custom_icons,{
    
    custom_marker_icons_values(NULL)
    all_marker_icons_values(default_icon_values)
    
  })
  
  
  output$saved_custom_icons <- renderUI({
    
    custom_icons <- custom_marker_icons_values() %>% names()
    
    icon_tags <- lapply(custom_icons, HTML)
    
    icon_tags
    
  })
  
  
  
  #### MAP IMAGE INPUT #####
  
  #img_input modal
  img_input <-  modalDialog(
        textInput("image_url","Paste an image url"),
        uiOutput("url_confirm") %>% withSpinner(color = "blue")
       
  )
  
  
  
  output$url_confirm <- renderUI({
    req(input$image_url)
    if(!str_extract(input$image_url,"\\.[a-zA-Z0-9]+$") %in% c(".png",".jpeg",".jpg",".gif",".bmp",".svg")){
      
      HTML(
        "<p>Not a valid URL; enter an url with one of the following file extensions</p>
        <li> png </li>
        <li> jpeg </li>
        <li> jpg </li>
        <li> gif </li>
        <li> bmp </li>
        <li> svg </li>"
        )
    } else if(is.null(img_dimensions())){
     # img(src=, width = "200px")
      
      HTML("<p>Getting image dimensions ...  <img src = 'spinner.svg' width = '10%'></p>")

    } else{
      actionButton("url_confirm","Confirm")
    }
  })
  
  observeEvent(input$image_url,{
    req(input$img_dimensions)
    img_dimensions(input$img_dimensions)
    
  })
  
  # Observe event to trigger on app start
  session$onFlushed(function() {
    
    isolate({
      showModal(img_input)
    })
  })
  
  observeEvent(input$open_img_input,{
    showModal(img_input)
  })
  
  # Reactive values to store the name and dimensions of the uploaded image
  # Reactive values to store the name and dimensions of the uploaded image
  img_src <- reactiveVal() # Image URL
  img_name <- reactiveVal() # Name of the image
  img_dimensions <- reactiveVal(NULL) # Image dimensions
  
  observe({
    req(input$image_url)
    img_src(input$image_url)
    img_name(str_extract(input$image_url, "[^/]*(?=\\.[:alpha:]*$)"))
    img_dimensions(input$img_dimensions) # Assuming this comes from JavaScript
  })
  
  

  
  # Observe the img_dimensions and enable/disable the confirm button
  #Als er een url wordt geconfirmed
  observeEvent(input$url_confirm,{
    req(img_dimensions())
    
    removeModal()

    # Clear existing values
    map_reactive(NULL)
    markers_df(data.frame(
      group = character(),
      lng = numeric(),
      lat = numeric(),
      label = character(),
      icon = character(),
      content = character(),
      url = character(),
      url_label = character(),
      popup_image_url = character(),
      popup_image_url_label = character(),
      stringsAsFactors = FALSE
    ))
    # img_src(NULL)
    # img_name(NULL)
    # img_dimensions(NULL)
    
    
    #Leafletmap renderen
    output$mymap <- renderLeaflet({
      
      if(!is.null(img_dimensions())){
        
        image_dimensions <- isolate(img_dimensions())
        image_src <- isolate(img_src())
        
        mymap = leaflet() %>%
          # addTiles() %>% 
          setView(lng = 0, lat = 45, zoom = 8) %>%  # Initial view settings, adjust as necessary
          htmlwidgets::onRender(glue::glue("
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
    }}
"))
        
        
        
        map_reactive(mymap)
        mymap
      }
    })
    
  })
  
  ##### LOAD MAP FILE
  #Als er een map file wordt ingeladen
  observeEvent(input$map_file, {
    req(input$map_file)
    
    # Clear existing values
    map_reactive(NULL)
    markers_df(data.frame(
      group = character(),
      lng = numeric(),
      lat = numeric(),
      label = character(),
      icon = character(),
      content = character(),
      url = character(),
      url_label = character(),
      stringsAsFactors = FALSE
    ))
    img_src(NULL)
    img_name(NULL)
    img_dimensions(NULL)
    
    
    
    load(input$map_file$datapath)
    
    
    
    # Check that necessary variables exist in the loaded environment
    if (exists("mymap") && exists("saved_img_src") && exists("saved_markers_df") && exists("saved_img_dimensions") && 
        exists("saved_custom_icons") && exists("saved_custom_marker_icons_values")) {
      
      #append custom_icons
      custom_marker_icons_values(c(custom_marker_icons_values(),saved_custom_marker_icons_values))
      all_marker_icons(c(all_marker_icons(), saved_custom_icons)) 
      
      
      map_reactive(mymap)
      img_src(saved_img_src)
      img_name(str_extract(saved_img_src, "[^/]*(?=\\.[:alpha:]*$)"))
      markers_df(saved_markers_df)
      img_dimensions(saved_img_dimensions)
      
      # Render the map with the image overlay and markers
      output$mymap <- renderLeaflet({
        leaflet() %>%
          setView(lng = 0, lat = 45, zoom = 8) %>%
          htmlwidgets::onRender(glue::glue("
          function(el, x) {{
              var myMap = this;
              var imageUrl = '{saved_img_src}';
              
              var imgDimensions = [{saved_img_dimensions[2]}, {saved_img_dimensions[1]}];
              
              var aspectRatio = imgDimensions[0] / imgDimensions[1];
              
              var latMin = 40;
              var latMax = 50;
              var lngMin = -5;
              var lngMax = 5;

              var latCenter = (latMin + latMax) / 2;
              var lngCenter = (lngMin + lngMax) / 2;

              var latRange = latMax - latMin;
              var lngRange = lngMax - lngMin;

              var correctionFactor = Math.cos(latCenter * Math.PI / 180);

              if (latRange / (lngRange * correctionFactor) > aspectRatio) {{
                  var newLngRange = latRange / aspectRatio;
                  lngMin = lngCenter - (newLngRange / 2) / correctionFactor;
                  lngMax = lngCenter + (newLngRange / 2) / correctionFactor;
              }} else {{
                  var newLatRange = lngRange * aspectRatio * correctionFactor;
                  latMin = latCenter - (newLatRange / 2);
                  latMax = latCenter + (newLatRange / 2);
              }}

              var imageBounds = [[latMin, lngMin], [latMax, lngMax]];

              L.imageOverlay(imageUrl, imageBounds).addTo(myMap);
              myMap.fitBounds(imageBounds);
          }}
          "))
      })
      
      # Use leafletProxy to add markers from the loaded data
      proxy <- leafletProxy("mymap")
      for (i in 1:nrow(markers_df())) {
        label <- markers_df()$label[i]
        url <- markers_df()$url[i]
        url_label <- markers_df()$url_label[i]
        content <- markers_df()$content[i]
        popup_content <- glue::glue("<h1>{label}</h1>")
        if (nzchar(url)) {
          url_label <- ifelse(nzchar(url_label), url_label, "read more")
          popup_content <- glue::glue("{popup_content}<a href=\"{url}\"  target='_PARENT'>{url_label}</a>")
        }
        if (nzchar(content)) {
          popup_content <- glue::glue("{popup_content}<p>{content}</p>")
        }
        
        proxy <- proxy %>%
          addAwesomeMarkers(
            group = markers_df()$group[i],
            lng = markers_df()$lng[i],
            lat = markers_df()$lat[i],
            label = label,
            icon = all_marker_icons()[[markers_df()$icon[i]]],
            popup = HTML(popup_content),
            options = markerOptions(draggable = T)
            )
      }
    } else {
      showNotification("Invalid .Rdata file")
    }
  })
  
  ##### MARKERS ####
  
  marker_inputs <- modalDialog(
    uiOutput("icon_palette"),
    #triggers modal for selecting custom marker icons
    dropdownButton(
      circle = F,
      status = "succes",
      label = "add custom icons",
      multiInput(inputId = "iconpicker",
                 label = "Icoon",
                 choices = icon_html),
      
      colourInput("kleur",label = "Color", value = "black"),
      
      actionButton("confirm_icons","Add"),
      actionButton("clear_custom_icons","Remove all custom icns"),
      uiOutput("saved_custom_icons")),
    textInput("label_marker","marker label"),
    textInput("url_marker","url"),
    textInput("url_label_marker","url Label"),
    #Image
    textInput("popup_image_url","popup image url"),
    textInput("popup_image_alt_text","popup image alt text"),
    textAreaInput("content_marker","Content"),
    actionButton("confirm_marker_inputs","Confirm"),
    #Verwijder marker
    actionButton("remove","Remove Marker"),
    
    easyClose = T

  )
  

  observeEvent(input$edit_popup, {

    showModal(marker_inputs)
    
    #Update alle inputs naar de content van de gekozen marker
    
    markers_df_new <- markers_df()
    selected_marker <- markers_df_new[markers_df_new$group == target_marker_group(),]
    
    
    isolate({
      updateTextInput(session,"label_marker",
                      value = selected_marker$label)
      updateTextInput(session,"url_marker",
                      value = selected_marker$url)
      updateTextInput(session,"url_label_marker",
                      value = selected_marker$url_label)
      updateTextAreaInput(session,"content_marker",
                          value = selected_marker$content)
      updateTextAreaInput(session,"popup_image_url",
                          value = selected_marker$popup_image_url)
      updateTextAreaInput(session,"popup_image_alt_text",
                          value = selected_marker$popup_image_url_label)
      updateRadioGroupButtons(session, "marker_icon", selected = selected_marker$icon)
      
    })
    
  })

  
  #Map click = create marker  
  observeEvent(input$mymap_click, {

    #Creer marker
    click <- input$mymap_click
    marker = c(click$lng,click$lat)
    
    #Update alle inputs naar leeg
    updateTextInput(session,"label_marker",
                    value = "")
    updateTextInput(session,"url_marker",
                    value = "")
    updateTextInput(session,"url_label_marker",
                    value = "")
    updateTextAreaInput(session,"content_marker",
                        value = "")
    updateTextInput(session, "popup_image_url",
                    value = "")
    updateTextInput(session, "popup_image_alt_text",
                    value = "")
    
    
    #target_marker variabelen toewijzen aan nieuwe marker
    target_marker_coords(marker)
    target_marker_group(paste0(marker, collapse = ","))
    
    default_icoon <- ifelse(is.null(input$marker_icon),"kasteel",input$marker_icon)

    #dataframe markers; bijwerken met nieuwe marker
    markers_df(rbind(markers_df(),
                     
                     data.frame(
                       "group" = target_marker_group(),
                       "lng" = click$lng,
                       "lat" = click$lat,
                       "label" = "",
                       "icon" = default_icoon, 
                       "content" = "",
                       "url" = "",
                       "url_label" = "",
                       "popup_image_url" = "",
                       "popup_image_url_label" = ""
                       
                     )))
    
    #Reactive bijwerken
    mymap <- map_reactive() %>% 
      addAwesomeMarkers(
        group = target_marker_group(),
        lng = target_marker_coords()[1],
        lat = target_marker_coords()[2],
        icon = all_marker_icons()[[default_icoon]],
        options = markerOptions(draggable = T)
        )
    
    map_reactive(mymap)
    
    #Proxy bijwerken
    proxy <- leafletProxy("mymap")
    
    ## This displays the pin drop circle
    proxy %>% 
      addAwesomeMarkers(
        group = target_marker_group(),
        lng = target_marker_coords()[1],
        lat = target_marker_coords()[2],
        icon = all_marker_icons()[[default_icoon]],
        options = markerOptions(draggable = T),
        popup = paste0(actionButton("edit", "Edit", onclick = 'Shiny.onInputChange(\"edit_popup\", Math.random())')))
    
    showModal(marker_inputs)

  })
  
  #Marker click = Bekijk en Bewerk marker
  observeEvent(input$mymap_marker_click,{
    
    click_marker <- input$mymap_marker_click
    
    coords_marker <- c(click_marker$lng,click_marker$lat)

    target_marker_coords(coords_marker)
    target_marker_group(click_marker$group)

  })
  

  #Marker Drag; verplaats marker update coordinates
  observeEvent(input$mymap_marker_dragend,{
    
    lat <- input$mymap_marker_dragend$lat
    lng <- input$mymap_marker_dragend$lng
    
    target_marker_coords(c(lng,lat))
    group <- input$mymap_marker_dragend$group 
    target_marker_group(group)
    
    markers_df_new <- markers_df()
    
    markers_df_new$lng[markers_df_new$group == target_marker_group()] <- lng
    markers_df_new$lat[markers_df_new$group == target_marker_group()] <- lat

    markers_df(markers_df_new)

  })
  
  #character met de id van de target marker
  target_marker_coords <- reactiveVal()
  target_marker_group <- reactiveVal()


  
  #TODO; al deze observeEVENTS verwijderen & direct de inputs voeden aan marker_df wanneer confirm_marker wordt geklickt
  #Geen enkele reden om dingen direct te updaten als we een confirm button hebben
  #Als er een label wordt ingevuld; pas label aan
  observeEvent(input$label_marker,{
    if(!is.null(target_marker_coords())){
      
      markers_df_new <- markers_df()
      
      markers_df_new$label[markers_df_new$group == target_marker_group()] <- input$label_marker
      
      markers_df(markers_df_new)
    }
  })
  
  #Als er een url wordt ingevuld; pas url aan
  observeEvent(input$url_marker,{
    
    markers_df_new <- markers_df()
    
    markers_df_new$url[markers_df_new$group == target_marker_group()] <- input$url_marker
    
    markers_df(markers_df_new)
    
  })
  
  #Als er een url label wordt ingevuld; pas url label aan
  observeEvent(input$url_label_marker,{
    
    markers_df_new <- markers_df()
    
    markers_df_new$url_label[markers_df_new$group == target_marker_group()] <- input$url_label_marker
    
    markers_df(markers_df_new)
    
  })
  
  #Als er content wordt ingevuld; pas content aan
  observeEvent(input$content_marker,{
    
    markers_df_new <- markers_df()
    
    markers_df_new$content[markers_df_new$group == target_marker_group()] <- input$content_marker
    
    markers_df(markers_df_new)
    
  })
  
  #Als er een image url wordt ingevuld; pas img url aan
  observeEvent(input$popup_image_url,{

    markers_df_new <- markers_df()
    
    markers_df_new$popup_image_url[markers_df_new$group == target_marker_group()] <- input$popup_image_url
    
    markers_df(markers_df_new)
    
  })
  
  #Als er een image_url alt text wordt ingevuld; pas img url alt text aan
  observeEvent(input$popup_image_alt_text,{
    
    markers_df_new <- markers_df()
    
    markers_df_new$popup_image_url_label[markers_df_new$group == target_marker_group()] <- input$popup_image_alt_text
    
    markers_df(markers_df_new)
    
  })
  
  #Als er een ander Icon wordt geselecteerd; verander het icoon van de geselecteerde marker
  observeEvent(input$marker_icon, {
    req(target_marker_coords())
    
    markers_df_new <- markers_df()
    markers_df_new$icon[markers_df_new$group == target_marker_group()] <- input$marker_icon
    markers_df(markers_df_new)
    
  }, ignoreNULL = TRUE)

  
  
  #Als er een wijziging van markers doorgevoerd wordt
  observeEvent(input$confirm_marker_inputs,{
                      
    removeModal()
                      
    if(!is.null(target_marker_coords())){
      
      markers_df_new <- markers_df() 
      
      marker_info <- markers_df_new[markers_df_new$group == target_marker_group(),]
      

      # icoon <- marker_info$icon
      icoon <- marker_info$icon
      
      label <- marker_info$label
      url <- marker_info$url
      url_label <- marker_info$url_label

      url_in_popup <- case_when(url == "" ~ "",
                                url != "" & url_label == "" ~ glue("<a href={url} target='_PARENT'>read more</a>"),
                                url != "" & url_label != "" ~ glue("<a href={url} target='_PARENT'>{url_label}</a>"),
                                TRUE ~ "")
      
      popup_image <- input$popup_image_url
      popup_image_alt_text <- input$popup_image_alt_text
      
      image_in_popup <- case_when(popup_image == "" ~ "",
                                  popup_image != "" & popup_image_alt_text == "" ~ glue("<img src='{popup_image}' width = '40%' height = '100%' style='margin-right: 10px; object-fit:cover;' ></img>"),
                                  popup_image != "" & popup_image_alt_text != "" ~ glue("<img src='{popup_image}' width = '40%' height = '100%' style='margin-right: 10px; object-fit:cover;' alt='{popup_image_alt_text}' width = '50%'></img>"),
                                  TRUE ~ ""
                                  )
      
      popup <- HTML(glue(

        "
        <style> div.leaflet-popup-content {{max-width:80vh; min-width:20vw; max-height:40vh;}}</style>
        <h2 style='word-break:break-word;'>{label}</h2>
        
        {url_in_popup}
        <br>
        <div style='display:flex; align-items:flex-start;'>
        {image_in_popup}
        <p style='min-width:50%; max-height:20vh; margin-top:0px; word-break:break-word; overflow-y:auto; overflow-x:hidden;'> {marker_info$content}</p>
        </div>"
        
      ))
      
      
      #Wijzigingen aanbrengen aan reactive voor html
      mymap <- map_reactive() %>%
        clearGroup(target_marker_group()) %>% 
        addAwesomeMarkers(
          group = target_marker_group(),
          lng = target_marker_coords()[1],
          lat = target_marker_coords()[2],
          label = label,
          icon = all_marker_icons()[[icoon]],
          popup = popup,
          options = markerOptions(draggable = T))
      
      
      map_reactive(mymap)
      
      #Wiizigingen aanbrengen aan proxy voor ui
      proxy <- leafletProxy("mymap")
      proxy %>%
        clearGroup(target_marker_group()) %>% 
        addAwesomeMarkers(
          group = target_marker_group(),
          lng = target_marker_coords()[1],
          lat = target_marker_coords()[2],
          label = label,
          icon = all_marker_icons()[[icoon]],
          popup =             paste0(actionButton("edit", "Edit", onclick = 'Shiny.onInputChange(\"edit_popup\", Math.random())'),
            popup),
          options = markerOptions(draggable = T)
        )
    }
  })
  
  
  
  #Als er op remove wordt geklikt; verwijder een marker
  observeEvent(input$remove,{
    
    if(!is.null(target_marker_coords())){
      
      #wijziging doorvoeren in reactive
      mymap <- map_reactive() %>% 
        clearGroup(target_marker_group())
      
      map_reactive(mymap)
      
      #wijzigingen doorvoeren in proxy
      proxy <- leafletProxy("mymap")
      proxy %>%
        clearGroup(target_marker_group())
      
      #dataframe met markers bijwerken en huidige marker verwijderen
      markers_df(markers_df()[markers_df()$group != target_marker_group(),])
      
      #Update alle inputs naar leeg
      updateTextInput(session,"label_marker",
                      value = "")
      updateTextInput(session,"url_marker",
                      value = "")
      updateTextInput(session,"url_label_marker",
                      value = "")
      updateTextAreaInput(session,"content_marker",
                          value = "")
      updateTextInput(session,"popup_image_url_label",
                      value = "")
      updateTextInput(session, "popup_image_alt_text",
                      value = "")
      
     removeModal() 
    }
    
  })
  
  ##### OPSLAAN EN LADEN ####

  observeEvent(input$opslaan, {
    output$downloadRdata <- downloadHandler(
      filename = function() {
        paste0(img_name(), ".Rdata")
      },
      content = function(file) {
        mymap <- map_reactive()
        saved_img_src <- img_src()
        saved_markers_df <- markers_df()
        saved_img_dimensions <- img_dimensions()
        saved_custom_icons <- all_marker_icons()
        saved_custom_marker_icons_values <- custom_marker_icons_values()
        
        save(mymap, saved_img_src, saved_markers_df, saved_img_dimensions, 
             saved_custom_icons, saved_custom_marker_icons_values, file = file)
      }
    )
    
    showModal(
      modalDialog(
      title = "Download .Rdata file",
      "Save your changes in an .Rdata file",
      downloadButton('downloadRdata', 'Download Rdata'),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  #EXPORTEREN
  observeEvent(input$export, {
    output$downloadHtml <- downloadHandler(
      filename = function() {
        paste0(img_name(), ".html")
      },
      content = function(file) {
        
        mymap <- map_reactive()
        
        calls = 1:length(mymap[["x"]][["calls"]])
        
        for(call_no in calls){
          if(mymap[["x"]][["calls"]][[call_no]]$method == "addAwesomeMarkers"){
            
            mymap[["x"]][["calls"]][[call_no]]$args[[6]][["draggable"]] <- FALSE
            
          }
          }
        
        saveWidget(mymap, file = file, selfcontained = TRUE)
        
        # Inject FontAwesome CSS link into saved HTML
        html_lines <- readLines(file)
        fa_css_link <- '<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css">'
        html_lines <- sub('</head>', paste0(fa_css_link, '\n</head>'), html_lines)
        writeLines(html_lines, file)
      }
    )
    
    showModal(modalDialog(
      title = "Download",
      "Download map as standalone .html widget",
      downloadButton('downloadHtml', 'Download HTML'),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  

  
}