required_packages <- c("shiny", "leaflet", "DT", "httr", "jsonlite", 
                       "dplyr", "googleway", "markdown", "shinythemes", 
                       "shinyjs", "shinyWidgets")


for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}


source("./tableau-in-shiny-v1.2.R")
#loading packages
lapply(required_packages, library, character.only = TRUE)
# Load the dataset
landmarks_data <- read.csv("landmarks.csv")
landmarks_data$Lat <- as.numeric(sapply(strsplit(landmarks_data$ordinates, ","), `[`, 1))
landmarks_data$Lng <- as.numeric(sapply(strsplit(landmarks_data$ordinates, ","), `[`, 2))


# Extract landmark names
landmark_choices <- unique(landmarks_data$Feature_Name)  # Replace 'Feature_Name' with the actual column name for landmark names
default_categories <- c("Music", "Sports", "Arts & Theatre")
# Function to get landmark hours
get_landmark_hours <- function(place_name, lat, lng, api_key) {
  places_url <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", 
                       lat, ",", lng, "&radius=500&keyword=", URLencode(place_name), "&key=", api_key)
  response <- GET(places_url)
  data <- content(response, "parsed")
  
  if (length(data$results) > 0) {
    place_id <- data$results[[1]]$place_id
    details_url <- paste0("https://maps.googleapis.com/maps/api/place/details/json?place_id=", 
                          place_id, "&fields=opening_hours&key=", api_key)
    details_response <- GET(details_url)
    details_data <- content(details_response, "parsed")
    
    if (!is.null(details_data$result$opening_hours)) {
      return(details_data$result$opening_hours$weekday_text)
    }else{
      return("Unknown")
    }
  }
  
  return("Unknown")
}

get_autocomplete_suggestions <- function(input_text, api_key) {
  autocomplete_url <- paste0("https://maps.googleapis.com/maps/api/place/autocomplete/json?input=", 
                             URLencode(input_text), "&key=", api_key)
  
  # Print the URL for debugging
  print(paste("Request URL:", autocomplete_url))
  
  response <- GET(autocomplete_url)
  data <- content(response, "parsed")
  
  # Check and print the response status
  print(paste("Response Status:", data$status))
  
  if (data$status == "OK") {
    suggestions <- sapply(data$predictions, function(prediction) prediction$description)
    return(suggestions)
  } else {
    # Log the error message
    print(paste("Error:", data$error_message))
    return(NULL)
  }
}

get_travel_plan_response <- function(prompt, api_key) {
  url <- "https://api.openai.com/v1/chat/completions"
  
  headers <- add_headers(
    `Content-Type` = "application/json",
    `Authorization` = paste("Bearer", api_key)
  )
  
  body <- list(
    model = "gpt-4o-mini",
    messages = list(list(role = "user", content = prompt))
  )
  
  response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
  content <- content(response, "parsed")
  
  if (!is.null(content$choices)) {
    return(content$choices[[1]]$message$content)
  } else {
    print(content)
    return("Error: Unable to get response.")
  }
}

ui <- fluidPage(
  useShinyjs(),
  setUpTableauInShiny(),
  theme = shinytheme("cerulean"),
  
  # Add all necessary CSS and JavaScript
  tags$head(
    # Font Awesome
    tags$link(rel = "stylesheet", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
    
    # Custom CSS
    tags$style(HTML("
            /* Tableau wrapper styles */
            .tableau-wrapper {
                padding: 20px;
                margin: 20px;
                background: white;
                border-radius: 8px;
            }
            
            .tableauPlaceholder {
                margin: 0 auto;
                max-width: 100%;
            }
            
            /* Responsive adjustments */
            @media screen and (max-width: 800px) {
                .tableau-wrapper {
                    padding: 10px;
                    margin: 10px;
                }
            }
            
            :root {
                --primary-color: #1a73e8;      /* Google Blue */
                --primary-dark: #1557b0;       /* Darker Blue */
                --secondary-color: #4285f4;    /* Light Blue */
                --accent-color: #e8f0fe;       /* Very Light Blue */
                --text-primary: #202124;       /* Dark Gray */
                --surface-color: #ffffff;      /* White */
                --success-color: #34a853;      /* Google Green */
                --warning-color: #fbbc04;      /* Google Yellow */
                --error-color: #ea4335;        /* Google Red */
            }
            
            body {
                background-color: #f8f9fa;
                color: var(--text-primary);
            }
            
            /* Box shadow styles */
            .box-shadow {
                box-shadow: 0 2px 6px rgba(26, 115, 232, 0.1);
                border-radius: 8px;
                padding: 20px;
                background: var(--surface-color);
                margin-bottom: 20px;
            }
            
            /* Button styles */
            .btn-primary {
                background-color: var(--primary-color);
                border-color: var(--primary-color);
                transition: all 0.3s ease;
            }
            
            .btn-primary:hover {
                background-color: var(--primary-dark);
                border-color: var(--primary-dark);
                transform: translateY(-1px);
            }

            /* Map styles */
            #map {
                height: 600px !important;
                border-radius: 8px;
                box-shadow: 0 2px 6px rgba(26, 115, 232, 0.1);
            }

            /* Responsive iframe handling */
            iframe {
                width: 100%;
                height: 80vh;
                border: 0;
            }
        ")),
    
    # Add viewport meta tag to ensure responsiveness
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  # Navbar
  navbarPage(
    
    title = div(
      "Melbourne Travel Planner"
    ),
    id = "nav",
    
    # First tab: Plan Your Trip
    tabPanel("Plan Your Trip",
             fluidRow(
               # Left control panel
               column(4,
                      # Location settings
                      div(class = "box-shadow",
                          h4(class = "section-title", icon("map-marker"), "Location Settings"),
                          textInput("address_input", "Enter Your Address", 
                                    placeholder = "Type your address...",
                                    width = "100%"),
                          uiOutput("autocomplete_suggestions"),
                          actionButton("save_location", "Save Location", 
                                       class = "btn-primary btn-block", 
                                       icon = icon("map-marker-alt"))
                      ),
                      
                      # Trip parameters
                      div(class = "box-shadow",
                          h4(class = "section-title", icon("calendar"), "Trip Parameters"),
                          dateRangeInput("date_range", "Select Date Range",
                                         start = Sys.Date(),
                                         end = Sys.Date() + 3),
                          pickerInput("transport_mode", "Transportation Mode",
                                      choices = c("Public Transport", "Driving"),
                                      options = list(`live-search` = TRUE),
                                      multiple = FALSE)
                      ),
                      
                      # Preferences
                      div(class = "box-shadow",
                          h4(class = "section-title", icon("heart"), "Preferences"),
                          pickerInput("landmarks", "Select Landmarks",
                                      choices = landmark_choices,
                                      options = list(`live-search` = TRUE,
                                                     `actions-box` = TRUE),
                                      multiple = TRUE),
                          pickerInput("category_filter", "Event Categories",
                                      choices = default_categories,
                                      options = list(`actions-box` = TRUE),
                                      multiple = TRUE)
                      ),
                      
                      # Action center
                      div(class = "box-shadow",
                          h4(class = "section-title", icon("tasks"), "Action Center"),
                          
                          # Step 1: Search Events
                          div(class = "action-step",
                              h5(class = "step-title", icon("search"), "Step 1: Search Events"),
                              actionButton("plan_trip", "Find Events", 
                                           class = "btn-primary btn-block")
                          ),
                          
                          # Step 2: Save Events
                          div(class = "action-step",
                              h5(class = "step-title", icon("save"), "Step 2: Save Events"),
                              actionButton("save_event", "Save Selected Events", 
                                           class = "btn-info btn-block")
                          ),
                          
                          # Step 3: Create Plan
                          div(class = "action-step",
                              h5(class = "step-title", icon("route"), "Step 3: Create Plan"),
                              actionButton("create_trip_plan", "Create Trip Plan", 
                                           class = "btn-success btn-block")
                          ),
                          
                          # Status message
                          div(id = "status_message", class = "status-message",
                              textOutput("status_text")
                          )
                      )
               ),
               
               # Right map and table
               column(8,
                      # Map
                      div(class = "box-shadow",
                          leafletOutput("map")
                      ),
                      
                      # Tabset panel for events and landmarks
                      div(class = "box-shadow",
                          tabsetPanel(
                            tabPanel("Events",
                                     DTOutput("events_table")
                            ),
                            tabPanel("Landmarks",
                                     DTOutput("landmarks_table")
                            ),
                            tabPanel("Saved Events",
                                     DTOutput("saved_events_table")
                            )
                          )
                      )
               )
             )
    ),
    
    # Second tab: Plan Recommendation
    tabPanel("Plan Recommendation",
             fluidRow(
               column(12,
                      div(class = "box-shadow",
                          h3(class = "section-title", 
                             icon("route"), "Your Custom Itinerary"),
                          uiOutput("itinerary_table")
                      )
               )
             )
    ),
    
    # Third tab: Parking (Tableau)
    tabPanel(
      title = "Parking",
      div(class = "tableau-viz",
          HTML(sprintf('<iframe src="%s?:showVizHome=no&:embed=true&:device=desktop"   
                     width="100%%" height="80vh" frameborder="0"></iframe>',
                       "https://public.tableau.com/views/ZehongTan/Dashboard1?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link"
          ))
      )
    ),
    
    # Fourth tab: Restaurants (Tableau)
    tabPanel("Restaurants",
             div(class = "tableau-viz",
                 HTML(sprintf('<iframe src="%s?:showVizHome=no&:embed=true&:device=desktop"  
                     width="100%%" height="80vh" frameborder="0"></iframe>',
                              "https://public.tableau.com/views/GEOM90007_Restauramt/Dashboard12?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link"
                 ))
             )
    )
  )
)


server <- function(input, output, session) {
  user_location <- reactiveValues(lat = NULL, lng = NULL)
  saved_events <- reactiveVal(data.frame(Name = character(), Date = character(), Venue = character(), stringsAsFactors = FALSE))
  # status management
    status <- reactiveVal("ready")
    events_found <- reactiveVal(FALSE)
    events_saved <- reactiveVal(FALSE)
    
    # disable buttons when the app is ready
    observe({
        shinyjs::disable("save_event")
        shinyjs::disable("create_trip_plan")
    })

    output$status_text <- renderText({
        switch(status(),
            "ready" = "Ready to start planning your trip",
            "searching" = "Searching for events...",
            "events_found" = "Events found! Select events to save",
            "events_saved" = "Events saved! Ready to create trip plan",
            "creating_plan" = "Creating your trip plan...",
            "plan_ready" = "Trip plan created!"
        )
    })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/chenhsuanw/cm2l3yuh700cu01pp5x3n6hum/tiles/{z}/{x}/{y}?access_token=MAPBOX_API_KEY",
        attribution = 'Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, ' ,
                      '<a href="https://www.mapbox.com/about/maps/">Mapbox</a>'
      ) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 12)  # Set view to Melbourne
  })

  observeEvent(input$address_input, {
    api_key <- "GOOGLE_API_KEY" 
    suggestions <- get_autocomplete_suggestions(input$address_input, api_key)
    
    updateSelectInput(session, "autocomplete_select", choices = suggestions)
  })
  
  output$autocomplete_suggestions <- renderUI({
    selectInput("autocomplete_select", "Suggestions", choices = NULL)
  })
  
  observeEvent(input$save_location, {
    showNotification("Location updated!", type = "message")
    api_key <- "GOOGLE_API_KEY"
    selected_address <- input$autocomplete_select
    geocode_url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", URLencode(selected_address), "&key=", api_key)
    geocode_response <- GET(geocode_url)
    geocode_data <- content(geocode_response, "parsed")
    
    if (length(geocode_data$results) > 0) {
      user_location$lat <- geocode_data$results[[1]]$geometry$location$lat
      user_location$lng <- geocode_data$results[[1]]$geometry$location$lng
      showNotification("Location saved successfully!", type = "message")
      # shinyjs::enable("plan_trip")
      # shinyjs::enable("save_event")
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = user_location$lng, lat = user_location$lat, zoom = 14) %>%
        addMarkers(lng = user_location$lng, lat = user_location$lat, popup = input$address_input)
    } else {
      showNotification("Address not found", type = "error")
    }
  })

  observeEvent(input$plan_trip, {
    withProgress(message = 'Finding events...', value = 0, {
      showModal(modalDialog(
              title = "Processing",
              "Please wait while we plan your trip...",
              easyClose = FALSE,
              footer = NULL
          ))

          # Simulate some processing time with a delay (remove this in production)
          #Sys.sleep(3)  # This is just for demonstration purposes

          # After processing is complete, remove the modal
          #removeModal()
      api_key <- "OPENAI_API_KEY"
      address <- input$address_input
      days <- as.numeric(difftime(input$date_range[2], input$date_range[1], units = "days")) + 1
      mode <- input$transport_mode
      events <- paste(input$category_filter, collapse = ", ")
      landmarks <- paste(input$landmarks, collapse = ", ")
      
      # Get selected events
      selected_events <- saved_events()
      event_list <- paste(selected_events$Name, collapse = ", ")
      
      prompt <- paste("I am traveling in Melbourne, staying at", address, 
                      "for", days, "days. I will be using", mode, 
                      "for transportation. I plan to attend events like", event_list, 
                      "and visit landmarks such as", landmarks, 
                      ". Can you suggest an itinerary?")
      
      response <- get_travel_plan_response(prompt, api_key)
      
      # Format response for better readability
      formatted_response <- paste("<h3>Suggested Itinerary</h3>", gsub("\n", "<br>", response))
      
      output$itinerary_response <- renderUI({
        HTML(formatted_response)
      })
      removeModal()
      incProgress(1/2)
      Sys.sleep(1)
      incProgress(1/2)
      shinyjs::enable("plan_trip")
      # shinyjs::enable("save_event")
      # shinyjs::enable("create_trip_plan")
    })
  })
  
  filtered_events <- reactive({
    if (!is.null(user_location$lat) && !is.null(user_location$lng)) {
      start_date <- input$date_range[1]
      end_date <- input$date_range[2]

      # Fetch events from Ticketmaster with 5km radius
      ticketmaster_url <- paste0("https://app.ticketmaster.com/discovery/v2/events.json?apikey=TICKETMASTER_API_KEY&latlong=",
                                 user_location$lat, ",", user_location$lng, "&radius=5&startDateTime=", 
                                 start_date, "T00:00:00Z&endDateTime=", end_date, "T23:59:59Z")
      ticketmaster_response <- GET(ticketmaster_url)
      ticketmaster_data <- content(ticketmaster_response, "parsed")

      # Process and display events
      events <- data.frame(Name = character(), Date = character(), Time = character(), Venue = character(), Category = character(), Lat = numeric(), Lng = numeric(), Link = character(), stringsAsFactors = FALSE)

      # Extract Ticketmaster events
      if (!is.null(ticketmaster_data$`_embedded`$events)) {
        for (event in ticketmaster_data$`_embedded`$events) {
          category <- ifelse(!is.null(event$classifications[[1]]$segment$name), event$classifications[[1]]$segment$name, "Unknown")
          link <- paste0("<a href='", event$url, "' target='_blank'><button>Ticketmaster</button></a>")
          events <- rbind(events, data.frame(
            Name = event$name,
            Date = event$dates$start$localDate,
            Time = event$dates$start$localTime,
            Venue = event$`_embedded`$venues[[1]]$name,
            Category = category,
            Lat = as.numeric(event$`_embedded`$venues[[1]]$location$latitude),
            Lng = as.numeric(event$`_embedded`$venues[[1]]$location$longitude),
            Link = link
          ))
        }
      }
      
      # Remove duplicate events
      events <- distinct(events, Name, Date, Venue, .keep_all = TRUE)

      # Filter events based on selected categories
      events[events$Category %in% input$category_filter, ]
    } else {
      data.frame(Name = character(), Date = character(), Time = character(), Venue = character(), Link = character(), stringsAsFactors = FALSE)
    }
  })
  
  observeEvent(input$plan_trip, {
    status("searching")
    selected_landmarks <- landmarks_data[landmarks_data$Feature_Name %in% input$landmarks, ]
    
    # get landmark opening hours
    api_key <- "GOOGLE_API_KEY"
    selected_landmarks$Hours <- sapply(1:nrow(selected_landmarks), function(i) {
      print(123)
      get_landmark_hours(selected_landmarks$Feature_Name[i], selected_landmarks$Lat[i], selected_landmarks$Lng[i], api_key)
    })

    # Render the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/chenhsuanw/cm2l3yuh700cu01pp5x3n6hum/tiles/{z}/{x}/{y}?access_token=MAPBOX_API_KEY",
        attribution = 'Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, ' ,
                      '<a href="https://www.mapbox.com/about/maps/">Mapbox</a>'
      ) %>%
        # Add event markers with blue icon
        addAwesomeMarkers(data = filtered_events(), lat = ~Lat, lng = ~Lng, icon = awesomeIcons(icon = 'info-sign', markerColor = 'blue'), popup = ~paste(Name, "<br>", Date, "<br>", Time, "<br>", Venue)) %>%
        # Add landmark markers with red icon
        addAwesomeMarkers(data = selected_landmarks, lat = ~Lat, lng = ~Lng, icon = awesomeIcons(icon = 'info-sign', markerColor = 'red'), popup = ~paste(Feature_Name, "<br>", Hours)) %>%
        # Add user location marker with green icon
        addAwesomeMarkers(lng = user_location$lng, lat = user_location$lat, icon = awesomeIcons(icon = 'home', markerColor = 'green'), popup = input$address_input)
    })
    status("events_found")
    events_found(TRUE)
    shinyjs::enable("save_event")
    shinyjs::enable("create_trip_plan")

    output$events_table <- renderDT({
      datatable(filtered_events(), selection = 'multiple', options = list(
        pageLength = 5,
        dom = "tip",
        columnDefs = list(list(visible = FALSE, targets = c(6,7,8)))))
    })
    output$landmarks_table <- renderDT({ datatable(selected_landmarks, options = list(
      pageLength = 5,
      dom = "tip",
      columnDefs = list(list(visible = FALSE, targets = c(1,4, 5, 6))))) })
  })
  observeEvent(input$save_event, {
    selected_rows <- input$events_table_rows_selected
    
    if (length(selected_rows) > 0) {
        withProgress(message = 'Saving selected events...', value = 0, {
            # get current selected events
            events_to_save <- filtered_events()[selected_rows, ]
            
            # get previous saved events
            current_saved <- saved_events()
            
            # merge new and old events
            combined_events <- rbind(current_saved, events_to_save)
            
            # remove duplicate events (based on name, date, venue and time)
            unique_events <- distinct(combined_events, Name, Date, Venue, Time, .keep_all = TRUE)
            
            # update saved events
            saved_events(unique_events)
            
            incProgress(0.5)
            
            incProgress(0.5)
            
            #  update status and enable create trip plan button
            status("events_saved")
            events_saved(TRUE)
            shinyjs::enable("create_trip_plan")
            
            # show success notification
            showNotification(
                sprintf("Successfully saved %d events!", nrow(events_to_save)),
                type = "message"
            )
        })
    } 
  })
output$saved_events_table <- renderDT({
    saved_data <- saved_events()
    if (nrow(saved_data) > 0) {
        # add action column
        saved_data$Actions <- sapply(1:nrow(saved_data), function(i) {
            sprintf('<button class="btn btn-danger btn-sm delete-btn" data-row="%d" onclick="Shiny.setInputValue(\'delete_row\', %d, {priority: \'event\'});">
                      <i class="fa fa-trash"></i> Delete
                    </button>', 
                    i, i)
        })
        
        datatable(
            saved_data,
            selection = 'none',  # disable row selection
            options = list(
                pageLength = 5,
                dom = "tip",
                columnDefs = list(
                    list(visible = FALSE, targets = c(5, 6)), # hide some columns
                    list(orderable = FALSE, targets = "Actions"), # disable action column sorting
                    list(className = 'dt-center', targets = "_all") # center align
                )
            ),
            escape = FALSE  # allow HTML
        )
    } else {
        datatable(
            data.frame(
                Message = "No saved events",
                stringsAsFactors = FALSE
            ),
            options = list(
                dom = "t",
                ordering = FALSE
            )
        )
    }
})

# listen to delete button click event
observeEvent(input$delete_row, {
    row_to_delete <- input$delete_row
    saved_data <- saved_events()
    
    if (row_to_delete <= nrow(saved_data)) {
        # delete specified row
        saved_data <- saved_data[-row_to_delete, ]
        saved_events(saved_data)
        
        # show notification
        showNotification("Event removed successfully", type = "message")
    }
})

  # create trip plan
observeEvent(input$create_trip_plan, {
    withProgress(message = 'Creating your personalized trip plan...', value = 0, {
        # update status
        status("creating_plan")
        
        # prepare data for trip plan
        address <- input$address_input
        days <- as.numeric(difftime(input$date_range[2], input$date_range[1], units = "days")) + 1
        mode <- input$transport_mode
        landmarks <- paste(input$landmarks, collapse = ", ")
        
        # get saved events (if any)
        saved_events_data <- saved_events()
        event_list <- ""
        
        if (nrow(saved_events_data) > 0) {
            # if there are saved events, add them to the prompt
            event_details <- apply(saved_events_data, 1, function(event) {
                sprintf("%s at %s on %s at %s",
                       event["Name"],
                       event["Venue"],
                       format(as.Date(event["Date"]), "%A, %B %d"),
                       format(strptime(event["Time"], "%H:%M:%S"), "%I:%M %p"))
            })
            event_list <- paste("\nI have booked the following events:", 
                              paste(event_details, collapse = "; "))
        }
        
        incProgress(0.3)
        
        # build prompt
        # use different prompt templates based on whether there are saved events
        if (nrow(saved_events_data) > 0) {
            prompt <- sprintf(
                "I am traveling in Melbourne, staying at %s for %d days. 
                I will be using %s for transportation.%s
                I also want to visit these landmarks: %s
                
                Please create a detailed day-by-day itinerary that:
                1. Incorporates all booked events at their scheduled times
                2. Includes visits to the landmarks during suitable times
                3. Suggests optimal transport routes
                4. Includes meal suggestions near the venues
                5. Accounts for travel time between locations
                6. Adds interesting nearby attractions when time permits
                
                Format the itinerary by day, with times and estimated durations for each activity.",
                address, days, mode, event_list, landmarks
            )
        } else {
            # if there are no saved events, use a simplified prompt
            prompt <- sprintf(
                "I am traveling in Melbourne, staying at %s for %d days. 
                I will be using %s for transportation.
                I want to visit these landmarks: %s
                
                Please create a detailed day-by-day itinerary that:
                1. Includes visits to the landmarks at suitable times
                2. Suggests optimal transport routes
                3. Recommends local restaurants and cafes
                4. Includes popular tourist attractions
                5. Accounts for travel time between locations
                6. Suggests activities that match the local atmosphere
                
                Format the itinerary by day, with times and estimated durations for each activity.",
                address, days, mode, landmarks
            )
        }
        
        incProgress(0.3)
        
        # call OpenAI API to generate trip plan
        api_key <- "OPENAI_API_KEY"
        response <- get_travel_plan_response(prompt, api_key)
        
        incProgress(0.2)
        
        # format response
        formatted_response <- response %>%
            # Process titles
            gsub("###\\s*", "<h3>", .) %>%
            gsub("Day (\\d+):", "<h4 style='color: #1a73e8;'>Day \\1:</h4>", .) %>%
            
            # Process times and locations
            gsub("\\*\\*(\\d{1,2}:\\d{2}\\s*(AM|PM))\\*\\*", 
                 "<span style='font-weight: bold; color: #34a853;'>\\1</span>", .) %>%
            gsub("\\*\\*Location:\\*\\*", 
                 "<span style='font-weight: bold; color: #ea4335;'>Location:</span>", .) %>%
            gsub("\\*\\*Travel:\\*\\*", 
                 "<span style='font-weight: bold; color: #fbbc04;'>Travel:</span>", .) %>%
            
            # Process other bold text
            gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", .) %>%
            
            # Process line breaks
            gsub("\n- ", "<br>• ", .) %>%
            gsub("\n", "<br>", .)
        
        # create complete HTML structure
        html_response <- sprintf('
            <div class="itinerary-container">
                <h2 style="color: #1a73e8; margin-bottom: 20px;">Your Melbourne Travel Itinerary</h2>
                
                <div class="itinerary-content">
                    %s
                </div>
                
                <div class="itinerary-notes">
                    <h4 style="color: #1a73e8;">Travel Notes:</h4>
                    <ul style="list-style-type: none; padding-left: 0; color: #666;">
                        <li style="margin-bottom: 8px;">
                            <i class="fa fa-info-circle" style="color: #1a73e8;"></i>
                            All times are approximate and may need adjustment based on actual conditions
                        </li>
                        <li style="margin-bottom: 8px;">
                            <i class="fa fa-clock-o" style="color: #34a853;"></i>
                            Consider checking venue websites for the most up-to-date information
                        </li>
                        <li style="margin-bottom: 8px;">
                            <i class="fa fa-mobile" style="color: #ea4335;"></i>
                            Keep this itinerary handy on your phone or print it for reference
                        </li>
                    </ul>
                </div>
            </div>
        ', formatted_response)
        
        # update UI
        output$itinerary_table <- renderUI({
            HTML(html_response)
        })
        
        # update status and show notification
        status("plan_ready")
        showNotification(
            "Trip plan created! Check the Itinerary tab.",
            type = "message",
            duration = 10
        )
        
        # automatically switch to itinerary tab
        updateTabsetPanel(session, "nav", selected = "Itinerary")
    })
})

  # add a helper function to format time
format_time <- function(time_str) {
    if (is.na(time_str) || time_str == "") return("")
    tryCatch({
        time_obj <- as.POSIXct(time_str, format = "%H:%M:%S")
        format(time_obj, "%I:%M %p")
    }, error = function(e) {
        return(time_str)
    })
}

  # add a helper function to format date
format_date <- function(date_str) {
    if (is.na(date_str) || date_str == "") return("")
    tryCatch({
        date_obj <- as.Date(date_str)
        format(date_obj, "%A, %B %d, %Y")
    }, error = function(e) {
        return(date_str)
    })
}
  # Function to parse itinerary response
  parse_itinerary <- function(response) {
    # Example parsing logic (adjust based on actual response format)
    days <- strsplit(response, "\n\n")[[1]]
    itinerary_list <- lapply(days, function(day) {
      parts <- strsplit(day, "\n")[[1]]
      data.frame(Date = parts[1], Activities = paste(parts[-1], collapse = "; "))
    })
    do.call(rbind, itinerary_list)
  }
  
  output$saved_events_table <- renderDT({
    datatable(saved_events(), escape = FALSE, options = list(
      pageLength = 5,
      dom = "tip",
      columnDefs = list(list(visible = FALSE, targets = c(5, 6)))))
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
