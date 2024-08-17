server <- function(input, output,session) {

  #TODO polygons
  #TODO EDITIN VOLLEDIG VIA TOOLBAR; Geheel naar mymap reactive sturen voor
  #opslaan & wegschrijven
  #proxy eig niet nodig dan?
  
  #TODO switch tussen Marker & Poly editing!
  
  
  
# background image inputs --------------------------------------------------------------
  
  #reactive value for storing leaflet map object (for saving to Rdata) 
  map_reactive <- reactiveVal()
  
  #reactive values for storing url, name and dimensions of the uploaded image
  img_src <- reactiveVal() # Image URL
  img_name <- reactiveVal() # Name of the image
  img_dimensions <- reactiveVal(NULL) # Image dimensions
  
  
  #modal dialog with textinput for image url
  img_input <-  modalDialog(
    
    textInput("image_url","Paste an image url"),
    uiOutput("confirm_img_url"),
    easyClose = T
  )
  
  #show img_input modal on startup
  session$onFlushed(function() {
    
    isolate({
      showModal(img_input)
    })
  })
  
  #show img_input modal when actionbutton is clicked
  observeEvent(input$open_img_input,{
    showModal(img_input)
  })
  
  #output with confirm button. should only show confirm when the correct filetype
  #is entered in image_url and the image_dimensions are retrieved from client.
  output$confirm_img_url <- renderUI({
    req(input$image_url)
    if(!str_extract(input$image_url,"\\.[a-zA-Z0-9]+$") %in% c(".png",".jpeg",".jpg",".gif",".bmp",".svg")){
      
      HTML(
        "<p>Not a valid URL; enter an url with one of the following file extensions</p>
        <li> png </li>
        <li> jpeg </li>
        <li> jpg </li>
        <li> gif </li>
        <li> bmp </li>
        <li> svg </li>")
      } else if(is.null(img_dimensions())){
       
        HTML("<p>Getting image dimensions ...  <img src = 'spinner.svg' width = '10%'></p>")
        
      } else{
      actionButton("confirm_img_url","Confirm")
    }
  })
  
  #when image dimensions are retrieved from client; set image_dimensions()
  observeEvent(input$image_url,{
    req(input$img_dimensions)
    img_dimensions(input$img_dimensions)
    
  })
  
  #observe changes in the image_url input and update img values
  observe({
    req(input$image_url)
    img_src(input$image_url)
    img_name(str_extract(input$image_url, "[^/]*(?=\\.[:alpha:]*$)")) #last part of url between "/" and .filetype
    img_dimensions(input$img_dimensions) # Assuming this comes from JavaScript
  })
  
  #when image url is confirmed
  observeEvent(input$confirm_img_url,{
    req(img_dimensions())
    
    removeModal() #remove image url input modal
    
    #clear saved map & marker values
    map_reactive(NULL)
    df_markers(data.frame(
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
      popup = character(),
      stringsAsFactors = FALSE
    ))
    
    #render leafletmap
    output$mymap <- renderLeaflet({
      
      if(!is.null(img_dimensions())){
        
        image_dimensions <- isolate(img_dimensions())
        image_src <- isolate(img_src())
        
        mymap = leaflet() %>%
          # addTiles() %>% 
          setView(lng = 0, lat = 45, zoom = 8) %>%  # initial view; scaled to img_bounds
          
          #js function that gets image dimensions from the client and converts these to
          #image bounds for the leaflet map.
          htmlwidgets::onRender(js_leaflet_background_image(image_src, image_dimensions))
        
        map_reactive(mymap)
        mymap
      }
    })
    
  })

# marker customization -------------------------------------------------------------------
  
  #reactiveVal df for saving marker data
  df_markers <- reactiveVal(
    data.frame(
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
      popup = character(),
      stringsAsFactors = FALSE
      )
    )
  
  #reactiveVal for saving target marker coordinates and group id
  target_marker_coords <- reactiveVal()
  target_marker_group <- reactiveVal()
  
  #when the map is clicked, create a new marker   
  observeEvent(input$mymap_click, {
    
    #only when not drawing polygons
    req(!drawing())

    #save input and marker coordinates
    click <- input$mymap_click
    marker = c(click$lng,click$lat)
    
    #clear all marker inputs
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
    
    
    #update target_marker variables to new marker
    target_marker_coords(marker)
    target_marker_group(paste0(marker, collapse = ","))
    
    default_icoon <- "kasteel"
    
    #update df_markers with new marker info.
    df_markers(rbind(df_markers(),
                     
                     data.frame(
                       group = target_marker_group(),
                       lng = click$lng,
                       lat = click$lat,
                       label = "",
                       icon = default_icoon, 
                       content = "",
                       url = "",
                       url_label = "",
                       popup_image_url = "",
                       popup_image_url_label = "",
                       popup = ""
                       
                     )))
    
    #update map_reactive() with new marker
    mymap <- map_reactive() %>% 
      addAwesomeMarkers(
        group = target_marker_group(),
        lng = target_marker_coords()[1],
        lat = target_marker_coords()[2],
        icon = all_marker_icons()[[default_icoon]],
        options = markerOptions(draggable = T)
      )
    
    map_reactive(mymap)
    
    #update leafletproxy with new marker
    proxy <- leafletProxy("mymap")
    
    proxy %>% 
      addAwesomeMarkers(
        group = target_marker_group(),
        lng = target_marker_coords()[1],
        lat = target_marker_coords()[2],
        icon = all_marker_icons()[[default_icoon]],
        options = markerOptions(draggable = T),
        popup = paste0(
          div(id = "edit_remove",
              actionButton("edit", "Edit", onclick = 'Shiny.setInputValue(\"edit_popup\", 1)'),
              actionButton("remove", "Remove", onclick = 'Shiny.setInputValue(\"remove_popup\", 1)')
              )
          )
        )
    
    showModal(marker_inputs) #Show marker inputs
    
  })
  
  #Modal dialog for editing markers. Pops up when a marker is initially created or when "Edit" is clicked
  #on a marker popup
  marker_inputs <- modalDialog(
    uiOutput("icon_palette"),
    #triggers modal for selecting custom marker icons
    dropdownButton(
      inputId = "custom_icon_dropdown",
      circle = F,
      status = "succes",
      label = "add custom icons",
      multiInput(inputId = "iconpicker",
                 label = "Icoon",
                 choices = icon_html),
      colourpicker::colourInput("kleur",
                                label = "Color", value = "black",
                                #palette = "limited",
                                closeOnClick = T),
      
      actionButton("confirm_icons","Add"),
      actionButton("clear_custom_icons","Remove all")
      
    ),
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
  
  #When edit is clicked; show marker inputs 
  observeEvent(input$edit_popup, {
    
    #show modal
    showModal(marker_inputs)
    
    #update inputfields to saved data for selected marker
    new_df_markers <- df_markers()
    selected_marker <- new_df_markers[new_df_markers$group == target_marker_group(),]
    
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
    
    #reset input to null
    runjs('Shiny.setInputValue("edit_popup", null);')
    
  })
  
  #when a marker is clicked; update target_marker
  observeEvent(input$mymap_marker_click,{
    
    click_marker <- input$mymap_marker_click
    
    coords_marker <- c(click_marker$lng,click_marker$lat)
    
    target_marker_coords(coords_marker)
    target_marker_group(click_marker$group)
    
  })
  
  
  #when a marker is dragged; update target_marker, marker group and coordinates
  observeEvent(input$mymap_marker_dragend,{

    lat <- input$mymap_marker_dragend$lat
    lng <- input$mymap_marker_dragend$lng
    target_marker_coords(c(lng,lat))
    
    group <- input$mymap_marker_dragend$group 
    target_marker_group(group)
    
    new_df_markers <- df_markers()
    new_df_markers$lng[new_df_markers$group == target_marker_group()] <- lng
    new_df_markers$lat[new_df_markers$group == target_marker_group()] <- lat
    
    df_markers(new_df_markers)
    
  })
  

  #when changes to a markers are confirmed; update map with new marker
  observeEvent(input$confirm_marker_inputs,{
    
    removeModal() #remove marker input modal
    
    if(!is.null(target_marker_coords())){
      
      new_df_markers <- df_markers() 
      
      #get df row for selected marker
      selected_marker <- new_df_markers[new_df_markers$group == target_marker_group(),]
      
      #update popup values for selected marker
      selected_marker$icon <- input$marker_icon
      selected_marker$label <- input$label_marker
      selected_marker$url <- input$url_marker
      selected_marker$url_label <- input$url_label_marker
      selected_marker$popup_image_url <- input$popup_image_url
      selected_marker$popup_image_url_label <- input$popup_image_alt_text
      selected_marker$content <- input$content_marker
      
      #update popup variable with constructed html str from popup values
      
      #construct popup html
      label <- selected_marker$label
      url <- selected_marker$url
      url_label <- selected_marker$url_label
      
      url_in_popup <- case_when(url == "" ~ "",
                                url != "" & url_label == "" ~ glue("<a href={url} target='_PARENT'>read more</a>"),
                                url != "" & url_label != "" ~ glue("<a href={url} target='_PARENT'>{url_label}</a>"),
                                TRUE ~ "")
      
      popup_image <- input$popup_image_url
      popup_image_alt_text <- input$popup_image_alt_text
      
      image_in_popup <- case_when(popup_image == "" ~ "",
                                  popup_image != "" & popup_image_alt_text == "" ~ 
                                    glue("<img src='{popup_image}' class = 'popup-image'></img>"),
                                  popup_image != "" & popup_image_alt_text != "" ~ 
                                    glue("<img src='{popup_image}' class = 'popup-image' alt='{popup_image_alt_text}'></img>"),
                                  TRUE ~ ""
      )
      
      popup_html <- HTML(glue("
        <style> div.leaflet-popup-content {{max-width:80vh; min-width:20vw; max-height:40vh;}}</style>
        <h3>{label}</h3>
        
        {url_in_popup}
        <br>
        <div style='display:flex; align-items:flex-start;'>
        {image_in_popup}
        <p class = 'popup'> {selected_marker$content}</p>
        </div>"
                              
      ))
      
      #update popup var with constructed html
      selected_marker$popup <- popup_html

      #write updated selected marker to df_markers()
      new_df_markers[new_df_markers$group == target_marker_group(),] <- selected_marker
      
      df_markers(new_df_markers)
      
      #get marker icon
      icoon <- selected_marker$icon
      
      #update mymap 
      mymap <- map_reactive() %>%
        clearGroup(target_marker_group()) %>% 
        addAwesomeMarkers(
          group = target_marker_group(),
          lng = target_marker_coords()[1],
          lat = target_marker_coords()[2],
          label = label,
          icon = all_marker_icons()[[icoon]],
          popup = popup_html, #include added content; no edit button
          options = markerOptions(draggable = T))
      
      map_reactive(mymap)
      
      #update leafletProxy
      proxy <- leafletProxy("mymap")
      proxy %>%
        clearGroup(target_marker_group()) %>% 
        addAwesomeMarkers(
          group = target_marker_group(),
          lng = target_marker_coords()[1],
          lat = target_marker_coords()[2],
          label = label,
          icon = all_marker_icons()[[icoon]],
          popup = paste0(div(id = "edit_remove",
                             actionButton("edit", "Edit", onclick = 'Shiny.setInputValue(\"edit_popup\", 1)'),
                             actionButton("remove", "Remove", onclick = 'Shiny.setInputValue(\"remove_popup\", 1)'),
                           ),
                         popup_html), #include edit button and added content
          options = markerOptions(draggable = T)
        )
    }
  })
  

  #when remove marker is clicked in the popop; remove marker
  observeEvent(input$remove_popup,{
    print(input$remove_popup)
    
    if(!is.null(input$remove_popup) & !is.null(target_marker_coords())){
      #remove marker from mymap
      mymap <- map_reactive() %>% 
        clearGroup(target_marker_group())
      
      map_reactive(mymap)
      
      #remover marker from proxy
      proxy <- leafletProxy("mymap")
      proxy %>%
        clearGroup(target_marker_group())
      
      #update df_markers and remove marker row
      df_markers(df_markers()[df_markers()$group != target_marker_group(),])
      
      #set input$remove_popup to NULL
      runjs('Shiny.setInputValue("remove_popup", null);')
    }
  
      
  })
  
  #when remove marker is clicked in edit menu; remove marker
  observeEvent(input$remove, {
    
    if(!is.null(target_marker_coords())){
      
      #remove marker from mymap
      mymap <- map_reactive() %>% 
        clearGroup(target_marker_group())
      
      map_reactive(mymap)
      
      #remover marker from proxy
      proxy <- leafletProxy("mymap")
      proxy %>%
        clearGroup(target_marker_group())
      
      #update df_markers and remove marker row
      df_markers(df_markers()[df_markers()$group != target_marker_group(),])
      
      removeModal() #close marker input modal
    } 
  })

# icon customization ------------------------------------------------------

  all_marker_icons <- reactiveVal(default_marker_icons)   #awesomeIconList that can be appended with custom icons
  all_marker_icons_values <- reactiveVal(default_icon_values) # named character with <icon> html for both default & custom icons
  custom_marker_icons_values <- reactiveVal() #seperate named character with <icon> html for custom icons so they can be removed easily
  
  
  #ui output for selecting marker icons
  output$icon_palette <- renderUI({

    radioGroupButtons(
      inputId = "marker_icon",
      label = "Custom Icons",
      choices = all_marker_icons_values())

  })

  #when confirm_icons is clicked; add new marker icons 
  observeEvent(input$confirm_icons,{
    req(input$iconpicker)
    
    #custom icons need to be added to AwesomIconList all__marker_icons() in order to
    #use them in leaflet markers
    
    #create a new awesomeIconList for custom markers
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
    
    toggleDropdownButton("custom_icon_dropdown",session)

    
  })
  
  #hen remove all is clicked; remove all custom icons 
  observeEvent(input$clear_custom_icons,{
    
    custom_marker_icons_values(NULL)
    all_marker_icons_values(default_icon_values) #set to default

  })
  

# save as .Rdata -----------------------------------------------------------

  #when the save button is used; save map to .Rdata
  observeEvent(input$save, {
    
    #downloadbutton
    output$downloadRdata <- downloadHandler(
      filename = function() {
        paste0(img_name(), ".Rdata")
      },
      content = function(file) {
        mymap <- map_reactive()
        saved_img_src <- img_src()
        saved_df_markers <- df_markers()
        saved_img_dimensions <- img_dimensions()
        saved_custom_icons <- all_marker_icons()
        saved_custom_marker_icons_values <- custom_marker_icons_values()
        
        save(mymap, saved_img_src, saved_df_markers, saved_img_dimensions, 
             saved_custom_icons, saved_custom_marker_icons_values, file = file)
        
        
      }
    )
    
    #modal dialog
    showModal(
      modalDialog(
      title = "Download .Rdata file",
      "Save your changes in an .Rdata file",
      downloadButton('downloadRdata', 'Download Rdata'),
      easyClose = TRUE,
      footer = NULL
    ))
  })

# export as .html ---------------------------------------------------------

  #when the export button is clicked
  observeEvent(input$export, {
    
    #downloadbutton
    output$downloadHtml <- downloadHandler(
      filename = function() {
        paste0(img_name(), ".html")
      },
      content = function(file) {
        
        mymap <- map_reactive()
        
        #edit mymap so that markerOption draggable = FALSE
        calls = 1:length(mymap[["x"]][["calls"]])
        
        for(call_no in calls){
          
          if(mymap[["x"]][["calls"]][[call_no]]$method == "addAwesomeMarkers"){
            
            mymap[["x"]][["calls"]][[call_no]]$args[[6]][["draggable"]] <- FALSE
          }
          }
        
        # Save map as self-contained htmlwidget
        saveWidget(mymap, file = file, selfcontained = TRUE)
        
        # Read the HTML lines from the saved file
        html_lines <- readLines(file)
        
        # Define the FontAwesome CSS link
        fa_css_link <- '<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css">'
        
        # Define the custom CSS styles
        custom_css <- '
        <style>
        img.popup-image {
          width: 50%;
          height: 100%;
          margin-right: 10px;
          object-fit: cover;
        }
        p.popup {
          min-width: 50%;
          max-height: 20vh;
          margin-top: 0px;
          word-break: break-word;
          overflow-y: auto;
          overflow-x: hidden;
        }
        </style>'
    
    # Inject the FontAwesome CSS link and custom CSS into the <head> section
    html_lines <- sub('</head>', paste0(fa_css_link, '\n', custom_css, '\n</head>'), html_lines)
    
    # Write the modified HTML lines back to the file
    writeLines(html_lines, file)
      }
    )
    
    #modal dialog
    showModal(modalDialog(
      title = "Download",
      "Download map as standalone .html widget",
      downloadButton('downloadHtml', 'Download HTML'),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
# load from .Rdata --------------------------------------------------------

  #when an R.data file is uploaded
  observeEvent(input$map_file, {
    req(input$map_file)
    
    # clear existing map and marker values
    map_reactive(NULL)
    df_markers(data.frame(
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
      popup = character(),
      stringsAsFactors = FALSE
    ))
    
    img_src(NULL)
    img_name(NULL)
    img_dimensions(NULL)
    
    #load .Rdata file
    load(input$map_file$datapath)
    
    # Check that necessary variables exist
    if (exists("mymap") && exists("saved_img_src") && exists("saved_df_markers") && exists("saved_img_dimensions") && 
        exists("saved_custom_icons") && exists("saved_custom_marker_icons_values")) {
      
      #append custom_icons
      custom_marker_icons_values(c(custom_marker_icons_values(),saved_custom_marker_icons_values))
      all_marker_icons(c(all_marker_icons(), saved_custom_icons)) 
      
      #update map, image and marker data
      map_reactive(mymap)
      img_src(saved_img_src)
      img_name(str_extract(saved_img_src, "[^/]*(?=\\.[:alpha:]*$)"))
      df_markers(saved_df_markers)
      img_dimensions(saved_img_dimensions)
      
      #render map with image
      output$mymap <- renderLeaflet({
        leaflet() %>%
          setView(lng = 0, lat = 45, zoom = 8) %>% #initial view
          #js to add image to map without stretching it 
          htmlwidgets::onRender(js_leaflet_background_image(img_src(), img_dimensions()))
      })
      
      #add markers to leafletProxy
      proxy <- leafletProxy("mymap")
      
      for (i in 1:nrow(df_markers())) {
        
        label <- df_markers()$label[i]
        url <- df_markers()$url[i]
        url_label <- df_markers()$url_label[i]
        popup_image_url <- df_markers()$popup_image_url[i]
        popup_image_url_label <- df_markers()$popup_image_url_label[i]
        content <- df_markers()$content[i]
        popup_content <- df_markers()$popup[i]
        
        proxy <- proxy %>%
          addAwesomeMarkers(
            group = df_markers()$group[i],
            lng = df_markers()$lng[i],
            lat = df_markers()$lat[i],
            label = label,
            icon = all_marker_icons()[[df_markers()$icon[i]]],
            popup = HTML(paste0(
              div(id = "edit_remove",
                  actionButton("edit", "Edit", onclick = 'Shiny.setInputValue(\"edit_popup\", 1)'),
                  actionButton("remove", "Remove", onclick = 'Shiny.setInputValue(\"remove_popup\", 1)')
              ),
              popup_content)),
            options = markerOptions(draggable = T)
          )
      }
    } else {
      showNotification("Invalid .Rdata file")
    }
  })
  

  # draw polygons -----------------------------------------------------------
  show_toolbar <- reactiveVal(FALSE) #Toggle toolbar for drawing polygons
  drawing <- reactiveVal(FALSE) #monitor wheter a polygon is being drawn
  
  #reactiveVal df for saving polygon data
  df_polygons <- reactiveVal(
    data.frame(    
      id = character(),
      lng = numeric(),
      lat = numeric(),
      stringsAsFactors = FALSE
      ))
  
  
  #When the toggle button for the toolbar is clicked.
  observeEvent(input$draw_toolbaar,{
    
    #If show_toolbar was true: remove toolbar 
    if(show_toolbar()){
      leafletProxy("mymap") %>%
        removeDrawToolbar()
      
      show_toolbar(FALSE)
    } else {
      #show toolbar
      leafletProxy("mymap") %>% addDrawToolbar(
        targetGroup = "drawn", 
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions()),
        polylineOptions = FALSE,
        circleOptions = FALSE, 
        rectangleOptions = FALSE, 
        markerOptions = FALSE, 
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(),
        drag = TRUE
      ) %>% 
        addStyleEditor()
      
      show_toolbar(TRUE)
      
    }
    
  })  
  
  #When a polygon is being drawn or edited
  observeEvent(c(input$mymap_draw_start,input$mymap_draw_editstart),{
    
    drawing(TRUE)
    
  })
  
  #modal for editing features of polygon 'district'
  edit_district <-  modalDialog(
    
    textInput("district_name","District name"),
    actionButton("confirm_district","Confirm district"),
    easyClose = T
  )

  
  #When finished drawing
  observeEvent(input$mymap_draw_new_feature,{
    
    drawing(FALSE)
    
    new_feature <- input$mymap_draw_new_feature
    # Extract coordinates from the drawn feature (assuming it's a polygon)
    coords <- new_feature$geometry$coordinates[[1]]  # Assuming only one polygon is drawn
    
    # Convert to a matrix of longitudes and latitudes
    latlngs <- do.call(rbind, lapply(coords, function(x) c(x[2], x[1])))
    
    id <- new_feature$properties$`_leaflet_id`
    lng <- latlngs[, 2] %>% unlist()
    lat <- latlngs[, 1] %>% unlist()
    
    new_polygon_data <- data.frame(
      "id" = id,
      "lng" = lng,
      "lat" = lat
    )

    df_polygons(rbind(df_polygons(),new_polygon_data))

    showModal(edit_district) #show edit district modal
    
    sf_polygons <- df_polygons() %>%
      group_by(id) %>%                                    # Group by polygon ID
      nest() %>%                                          # Nest the lng/lat pairs within each group
      mutate(geometry = map(data, ~ st_polygon(list(as.matrix(.x))))) %>%  # Convert each group to a polygon
      select(id, geometry) %>%                            # Select only the relevant columns
      st_as_sf()                                          # Convert to sf object
    
    print(sf_polygons)
    
    #add polygons to proxy
    leafletProxy("mymap") %>% 
      clearGroup(group = "districts") %>% 
      addPolygons(data = sf_polygons,
                  fillColor = ~id,
                  group = "districts",
                  layerId = ~id,
                  color = "black", weight = 2)
    
    #add polygons to mymap reactive
    #remove marker from mymap
    mymap <- map_reactive() %>% 
      clearGroup(group = "districts") %>% 
      addPolygons(data = sf_polygons,
                  fillColor = ~id,
                  group = "districts",
                  layerId = ~id,
                  color = "black", weight = 2)
    
    map_reactive(mymap)
    
    #TGODO Write to reactive()
    
  })
  
  #when finished editing
  observeEvent(input$mymap_draw_edited_features,{

    
    edited_feature <- input$mymap_draw_edited_features
   # print(edited_feature)
    
    # Extract coordinates from the drawn feature (assuming it's a polygon)
    coords <- edited_feature$features[[1]]$geometry$coordinates[[1]] # Assuming only one polygon is drawn
    

    # Convert to a matrix of longitudes and latitudes
    latlngs <- do.call(rbind, lapply(coords, function(x) c(x[2], x[1])))
    
    id <- edited_feature$features[[1]]$properties$`_leaflet_id`
    lng <- latlngs[, 2] %>% unlist()
    lat <- latlngs[, 1] %>% unlist()
    
    new_polygon_data <- data.frame(
      "id" = id,
      "lng" = lng,
      "lat" = lat
    )


    #remove old polygon from df_polygons and add new polygon data
    df_polygons(
      rbind(
        df_polygons() %>% filter(id != unique(new_polygon_data$id)),
        new_polygon_data
        )
      )


    sf_polygons <- df_polygons() %>%
      group_by(id) %>%                                    # Group by polygon ID
      nest() %>%                                          # Nest the lng/lat pairs within each group
      mutate(geometry = map(data, ~ st_polygon(list(as.matrix(.x))))) %>%  # Convert each group to a polygon
      select(id, geometry) %>%                            # Select only the relevant columns
      st_as_sf()                                          # Convert to sf object
    
      
    #add edited polygons to proxy
    leafletProxy("mymap") %>%
      clearGroup(group = "districts") %>% 
      addPolygons(data = sf_polygons,
                  fillColor = ~id,
                  layerId = ~id,
                  group = "districts",
                  color = "black", weight = 2)
    
    #add edited polygons to mymap reactive
    mymap <- map_reactive() %>%
      clearGroup(group = "districts") %>% 
      addPolygons(data = sf_polygons,
                  fillColor = ~id,
                  layerId = ~id,
                  group = "districts",
                  color = "black", weight = 2)
    
    map_reactive(mymap)
      
        
    drawing(FALSE)
    showModal(edit_district) #show edit district modal

    
  })
  
  
}