server <- function(input, output,session) {

# reactiveVals ------------------------------------------------------------
  
  #leaflet map object for saving to Rdata 
  map_reactive <- reactiveVal()
  
  #reactiveVal df to save marker data
  df_markers <- reactiveVal(data.frame(
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
  

  all_marker_icons <- reactiveVal(default_marker_icons)   #awesomeIconList that can be appended with custom icons
  all_marker_icons_values <- reactiveVal(default_icon_values) # named character with <icon> html for both default & custom icons
  custom_marker_icons_values <- reactiveVal() #named character with <icon> html for just custom icons so they can be removed easily
  
# background image inputs --------------------------------------------------------------
  
  # Reactive values to store the url, name and dimensions of the uploaded image
  img_src <- reactiveVal() # Image URL
  img_name <- reactiveVal() # Name of the image
  img_dimensions <- reactiveVal(NULL) # Image dimensions
  
  
  #modal dialog with textinput for image url
  img_input <-  modalDialog(
    #TODO add upload file button
    textInput("image_url","Paste an image url"),
    uiOutput("confirm_img_url") %>% withSpinner(color = "blue")
    
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
        <li> svg </li>"
      )
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


# icon customization ------------------------------------------------------

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
  
#If remove all is clicked; remove all custom icons 
  observeEvent(input$clear_custom_icons,{
    
    custom_marker_icons_values(NULL)
    all_marker_icons_values(default_icon_values) #set to default
    
    #TODO optional remove icons from awesomeIconList as well as they wont be used anymore. Not removing them doesn't really impact UX. 
    
  })
  


  ##### MARKERS ####
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
      
      colourInput("kleur",
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
  

  observeEvent(input$edit_popup, {

    showModal(marker_inputs)
    
    #Update alle inputs naar de content van de gekozen marker
    
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
    
    new_df_markers <- df_markers()
    
    new_df_markers$lng[new_df_markers$group == target_marker_group()] <- lng
    new_df_markers$lat[new_df_markers$group == target_marker_group()] <- lat

    df_markers(new_df_markers)

  })
  
  #character met de id van de target marker
  target_marker_coords <- reactiveVal()
  target_marker_group <- reactiveVal()

  #Als er een wijziging van markers doorgevoerd wordt
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
                                    glue("<img src='{popup_image}' width = '40%' height = '100%' style='margin-right: 10px; object-fit:cover;' ></img>"),
                                  popup_image != "" & popup_image_alt_text != "" ~ 
                                    glue("<img src='{popup_image}' width = '40%' height = '100%' style='margin-right: 10px; object-fit:cover;' alt='{popup_image_alt_text}' width = '50%'></img>"),
                                  TRUE ~ ""
      )
      
      popup_html <- HTML(glue("
        <style> div.leaflet-popup-content {{max-width:80vh; min-width:20vw; max-height:40vh;}}</style>
        <h2 style='word-break:break-word;'>{label}</h2>
        
        {url_in_popup}
        <br>
        <div style='display:flex; align-items:flex-start;'>
        {image_in_popup}
        <p style='min-width:50%; max-height:20vh; margin-top:0px; word-break:break-word;
        overflow-y:auto; overflow-x:hidden;'> {selected_marker$content}</p>
        </div>"
        
      ))
      
      #update popup var with constructed html
      selected_marker$popup <- popup_html
      
      selected_marker <<- selected_marker
      
      print("Hier")
      
      #write updated selected marker to df_marers()
      new_df_markers[new_df_markers$group == target_marker_group(),] <- selected_marker

      df_markers(new_df_markers)

      #get marker icon
      icoon <- selected_marker$icon
      

      #Wijzigingen aanbrengen aan reactive voor html
      mymap <- map_reactive() %>%
        clearGroup(target_marker_group()) %>% 
        addAwesomeMarkers(
          group = target_marker_group(),
          lng = target_marker_coords()[1],
          lat = target_marker_coords()[2],
          label = label,
          icon = all_marker_icons()[[icoon]],
          popup = popup_html,
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
          popup = paste0(actionButton("edit", "Edit", onclick = 'Shiny.onInputChange(\"edit_popup\", Math.random())'),
                         popup_html),
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
      df_markers(df_markers()[df_markers()$group != target_marker_group(),])
      
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
        saved_df_markers <- df_markers()
        saved_img_dimensions <- img_dimensions()
        saved_custom_icons <- all_marker_icons()
        saved_custom_marker_icons_values <- custom_marker_icons_values()
        
        save(mymap, saved_img_src, saved_df_markers, saved_img_dimensions, 
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
  
  
  
  #when an R.data map file is uploaded
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
    
    #load the rdata file
    load(input$map_file$datapath)
    
    # Check that necessary variables exist in the loaded environment
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
      
      # Render the map with image overlay and markers
      output$mymap <- renderLeaflet({
        leaflet() %>%
          setView(lng = 0, lat = 45, zoom = 8) %>% #initial view
          #js to add image to map without stretching it 
          htmlwidgets::onRender(js_leaflet_background_image(img_src(), img_dimensions()))
      })
      
      # Use leafletProxy to add markers from the loaded data
      proxy <- leafletProxy("mymap")
      
      for (i in 1:nrow(df_markers())) {
        
        label <- df_markers()$label[i]
        url <- df_markers()$url[i]
        url_label <- df_markers()$url_label[i]
        popup_image_url <- df_markers()$popup_image_url[i]
        popup_image_url_label <- df_markers()$popup_image_url_label[i]
        content <- df_markers()$content[i]
        popup_content <- df_markers()$popup
        

        
        proxy <- proxy %>%
          addAwesomeMarkers(
            group = df_markers()$group[i],
            lng = df_markers()$lng[i],
            lat = df_markers()$lat[i],
            label = label,
            icon = all_marker_icons()[[df_markers()$icon[i]]],
            popup = HTML(paste0(
              actionButton("edit",
                           "Edit",
                           onclick = 'Shiny.onInputChange(\"edit_popup\", Math.random())'),#edit button. mathrandom to trigger oninputchange each time
              popup_content)),
            options = markerOptions(draggable = T)
          )
      }
    } else {
      showNotification("Invalid .Rdata file")
    }
  })

  
}