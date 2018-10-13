library(shiny)
library(tidyverse)
library(leaflet)
library(rgdal)
library(sp)
library(htmltools)

# Function to read sl2, sl3, slg binary files. Many thanks to @hbrmstr for
# it and package `arabia`
# https://gitlab.com/hrbrmstr/arabia.git
read_sl2 <- function(path, verbose=TRUE) {
  
  f <- file(path.expand(path), "rb")
  dat <- readBin(f, "raw", n = file.size(path.expand(path)), endian="little")
  close(f)
  
  # read in the header
  header <- readBin(dat, what = "raw", n = 10)
  
  format <- readBin(header[1:2], "int", size=2, endian="little", signed=FALSE)
  
  if (!(format %in% 1:3)) stop("Invalid 'format' in header; Likely not an slg/sl2/sl3 file")
  
  ok_formats <- c("slg", "sl2", "sl3")
  if (verbose) message("Format: ", ok_formats[format])
  
  version <- readBin(header[3:4], "int", size=2, endian="little", signed=FALSE)
  blockSize <- readBin(header[5:6], "int", size=2, endian="little", signed=FALSE)
  
  if (blockSize == 1970) {
    if (verbose) message("Block size: downscan")
  } else if (blockSize == 3200) {
    if (verbose) message("Block size: sidescan")
  } else {
    stop("Block size is not 'downscan' or 'sidescan'; Likely not an slg/sl2/sl3 file")
  }
  
  alwaysZero <- readBin(header[7:8], "int", size=2, endian="little", signed=FALSE)
  
  # yep, we're going to build a list the hard/slow way
  sl2_lst <- vector("list")
  idx <- 1
  pos <- 8 # keeping track of our place in the stream
  
  while (pos < length(dat)) {
    
    # if verbose mode echo a "." every 100 records
    if (verbose && ((idx %% 100) == 0)) cat(".")
    
    blockSize <- readBin(dat[(pos+29):(pos+30)], "int", size=2, endian="little", signed=FALSE)
    prevBlockSize <- readBin(dat[(pos+31):(pos+32)], "int", size=2, endian="little", signed=FALSE)
    packetSize <- readBin(dat[(pos+35):(pos+36)], "int", size=2, endian="little", signed=FALSE)
    frameIndex <- readBin(dat[(pos+37):(pos+40)], "int", size=4, endian="little")
    
    dplyr::data_frame(
      channel = readBin(dat[(pos+33):(pos+34)], "int", size=2,endian="little", signed=FALSE),
      upperLimit = readBin(dat[(pos+41):(pos+44)], "double", size=4, endian="little"),
      lowerLimit = readBin(dat[(pos+45):(pos+48)], "double", size=4, endian="little"),
      frequency = readBin(dat[(pos+51)], "int", size=1, endian="little", signed=FALSE),
      waterDepth = readBin(dat[(pos+65):(pos+68)], "double", size=4, endian="little"),
      keelDepth = readBin(dat[(pos+69):(pos+72)], "double", size=4, endian="little"),
      speedGps = readBin(dat[(pos+101):(pos+104)], "double", size=4, endian="little"),
      temperature = readBin(dat[(pos+105):(pos+108)], "double", size=4, endian="little"),
      lng_enc = readBin(dat[(pos+109):(pos+112)], "integer", size=4, endian="little"),
      lat_enc = readBin(dat[(pos+113):(pos+116)], "integer", size=4, endian="little"),
      speedWater = readBin(dat[(pos+117):(pos+120)], "double", size=4, endian="little"),
      track = readBin(dat[(pos+121):(pos+124)], "double", size=4, endian="little"),
      altitude = readBin(dat[(pos+125):(pos+128)], "double", size=4, endian="little"),
      heading = readBin(dat[(pos+129):(pos+132)], "double", size=4, endian="little"),
      timeOffset = readBin(dat[(pos+141):(pos+144)], "integer", size=4, endian="little"),
      flags = list(
        dat[(pos+133):(pos+134)] %>%
          rawToBits() %>%
          as.logical() %>%
          set_names(
            c(
              "headingValid", "altitudeValid", sprintf("unk%d", 1:7),
              "gpsSpeedValid", "waterTempValid", "unk8", "positionValid",
              "unk9", "waterSpeedValid", "trackValid"
            )
          ) %>%
          .[c(1:2, 10:11, 13, 15:16)] %>%
          as.list() %>%
          purrr::flatten_df()
      )
    ) -> sl2_lst[[idx]]
    
    idx <- idx + 1
    
    pos <- pos + (packetSize+145-1)
    
  }
  
  if (verbose) cat("\n")
  
  dplyr::bind_rows(sl2_lst) %>%
    dplyr::mutate(
      channel = dplyr::case_when(
        channel == 0 ~ "Primary",
        channel == 1 ~ "Secondary",
        channel == 2 ~ "DSI (Downscan)",
        channel == 3 ~ "Left (Sidescan)",
        channel == 4 ~ "Right (Sidescan)",
        channel == 5 ~ "Composite",
        TRUE ~ "Other/invalid"
      )
    ) %>%
    dplyr::mutate(
      frequency = dplyr::case_when(
        frequency == 0 ~ "200 KHz",
        frequency == 1 ~ "50 KHz",
        frequency == 2 ~ "83 KHz",
        frequency == 4 ~ "800 KHz",
        frequency == 5 ~ "38 KHz",
        frequency == 6 ~ "28 KHz",
        frequency == 7 ~ "130-210 KHz",
        frequency == 8 ~ "90-150 KHz",
        frequency == 9 ~ "40-60 KHz",
        frequency == 10~ "25-45 KHz",
        TRUE ~ "Other/invalid"
      )
    ) %>%
    tidyr::unnest(flags)
  
}

ui <- fluidPage(
  # App title----
  titlePanel("App to Read 'Lowrance' Binary Track Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Upload data:
      fileInput("file", "Choose track file:",
                accept = c(".slg", ".sl2", ".sl3")),
      
      # Include clarifying text 
      helpText(".slg, .sl2 and .sl3",
               "Maximum size 100 Mb"),
      
      
      # Input the depth of the sensor
      numericInput("sensorinput", "Input the sensor's depth, m", 0,
                   step = 0.1, min = 0, max = 2),
      
      # Include clarifying text 
      helpText("The distance from the sensor to the water surface.",
               "Заглубление датчика, т.е. расстояние от датчика до",
               "поверхности воды"),
      
      # Button
      downloadButton("downloadcsv", "Download .csv"),
      downloadButton("downloadshp", "Download shapefile"),
      
      # author info
      shiny::hr(),
      em(
        span("Created by "),
        a("Anatolii Tsyplenkov", href = "mailto:atsyplenkov@gmail.com"),
        span(", Sept 2018"),
        br(), br()
      )
      
    ),
    
    # Main panel for displaying outputs ----
    # Main:
    mainPanel(
      
      leafletOutput("mymap"),
      
      includeMarkdown("include.md")
      
    )
  )
)

# Define server logic  ----
server <- function(input, output) {
  
  # Set the maximum file size
  options(shiny.maxRequestSize = 100*1024^2)
  
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    # Calculate the duration of progress bar loading
    N <- round(file.size(path.expand(input$file$datapath)) / 1024^2)
    
    # Add progress bar
    withProgress(message = 'Reading data', value = 0, {
      
      for(i in 1:N){
        
        # Long Running Task
        Sys.sleep(0.5)
        
        # Update progress
        incProgress(1/N)
      }
      
      # Read sonar file
      Dataset <- read_sl2(input$file$datapath)
      
      # Recalculate main parameters a bit
      Dataset %>% 
        mutate(Depth = (waterDepth / 3.2808399) + input$sensorinput, # feet to meter conversation
               Long = lng_enc / 6356752.3142 * 180 / pi, # Mercator Meters to Degrees
               Lat = (2 * atan(exp(lat_enc / 6356752.3142)) - (pi / 2)) * (180 / pi)) %>% # Mercator Meters to Degrees
        select(Depth, Long, Lat) %>% 
        group_by(Long, Lat) %>% 
        summarise(Depth = median(Depth)) -> Dataset
      
      # Write coordinates in the object
      coordinates(Dataset) <- ~Long + Lat
      
      # Set Projection
      proj4string(Dataset)  <-  CRS("+init=epsg:4326")
      
      return(Dataset)
      
    })
  })  
  
  # Show map:
  output$mymap <- renderLeaflet({
    
    if (is.null(input$file)) return(NULL)
    
    points <- Dataset()
    
    popup <- paste0(signif(points$Depth, 3), " m")
    
    color_pal <- colorNumeric("YlGnBu", points$Depth, n = 7)
    
    leaflet(points) %>%
      addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
               options = providerTileOptions(maxZoom = 20),
               group = "Google") %>% 
      addTiles(options = providerTileOptions(maxZoom = 20),
               group = "OSM") %>%
      addCircles(weight = 2,
                 radius = .4,
                 color = color_pal(points$Depth),
                 stroke = TRUE,
                 fillOpacity = .99,
                 popup = popup) %>% 
      addLegend("bottomright", pal = color_pal, values = points$Depth,
                title = "Depth",
                labFormat = labelFormat(suffix = " m"),
                opacity = 1) %>% 
      addLayersControl(
        baseGroups = c("Google", "OSM"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  ### Download .csv
  output$downloadcsv <- downloadHandler(
    filename = function() {
      paste(input$file, ".csv", sep = ";")
    },
    content = function(file) {
      write.csv(Dataset(), file, row.names = FALSE)
    }
  )
  
  ### Download shapefile
  # based on Stackoverflow
  # https://stackoverflow.com/questions/41707760/download-a-shape-file-from-shiny
  output$downloadshp <- downloadHandler(
    filename = "shapefile.zip",
    content = function(file) {
      data = Dataset()
      # create a temp folder for shp files
      temp_shp <- tempdir()
      # write shp files
      writeOGR(data, temp_shp, "lowrance", "ESRI Shapefile", 
               overwrite_layer = TRUE)
      # zip all the shp files
      zip_file <- file.path(temp_shp, "lowrance_shp.zip")
      shp_files <- list.files(temp_shp,
                              "lowrance",
                              full.names = TRUE) 
      zip_command <- paste("zip -j", 
                           zip_file, 
                           paste(shp_files, collapse = " "))
      system(zip_command)
      # copy the zip file to the file argument
      file.copy(zip_file, file)
      # remove all the files created
      file.remove(zip_file, shp_files)
    }
  )
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
