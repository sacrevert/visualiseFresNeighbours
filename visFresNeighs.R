####################################################
## Quick visualisation of Frescalo neighbourhoods ##
####################################################
## O.L. Pescott, olipes@ceh.ac.uk
## 17/01/2022
## Update 29/06/2022 for France
#rm(list=ls())
library(shiny)
library(magrittr)
library(leaflet)
library(sp)
library(sf)
library(rgdal)
library(colorRamps)
library(data.table)
library(DT)
load(file = "data/idfBB.rdata") #bounding box for IDF (France vis)
#### Required for processing prior to visualisation
#source('gridref_let2num.R')
####
## Load data
#britNew <- read.csv(file = "data/britishFresWeights_Jan2022_v0.csv", header = T)[,c(2:4)]
#save(britNew, file = "data/britishFresWeights_Jan2022_v0.rdata")
#britSparNew <- read.csv(file = "data/britishFresWeights_Feb2022_SPARTA.csv", header = T)[,c(2:4)]
#save(britSparNew, file = "data/britishFresWeights_Feb2022_SPARTA.rdata")
#engNew <- read.csv(file = "data/englishFresWeights_Feb2022_v0.csv", header = T)[,c(2:4)]
#save(engNew, file = "data/englishFresWeights_Feb2022_v0.rdata")
#walNew <- read.csv(file = "data/welshFresWeights_Feb2022_v0.csv", header = T)[,c(2:4)]
#save(walNew, file = "data/welshFresWeights_Feb2022_v0.rdata")
#scoNew <- read.csv(file = "data/scottishFresWeights_Feb2022_v0.csv", header = T)[,c(2:4)]
#save(scoNew, file = "data/scottishFresWeights_Feb2022_v0.rdata")
#eireNew <- read.csv(file = "data/irishFresWeights_Jan2022_v0.csv", header = T)[,c(2:4)]
#save(eireNew, file = "data/irishFresWeights_Jan2022_v0.rdata")
#roieireNew <- read.csv(file = "data/roirelandFresWeights_Feb2022_v0.csv", header = T)[,c(2:4)]
#save(roieireNew, file = "data/roirelandFresWeights_Jan2022_v0.rdata")
#neireNew <- read.csv(file = "data/northirelandFresWeights_Feb2022_v0.csv", header = T)[,c(2:4)]
#save(neireNew, file = "data/northirelandFresWeights_Jan2022_v0.rdata")
#### Create XY lookup for sites
#brGrLookup <- cbind(letGr = unique(britNew$target.let), gr_let2numSHINY(unique(britNew$target.let), centre = T), country = "Britain")
#save(brGrLookup, file = "data/britishSitesXY.rdata")
#erGrLookup <- cbind(letGr = unique(eireNew$target.let), gr_let2numSHINY(unique(eireNew$target.let), centre = T), country = "Ireland")
#save(erGrLookup, file = "data/irishSitesXY.rdata")
#engGrLookup <- cbind(letGr = unique(engNew$target.let), gr_let2numSHINY(unique(engNew$target.let), centre = T), country = "England")
#save(engGrLookup, file = "data/englishSitesXY.rdata")
#walGrLookup <- cbind(letGr = unique(walNew$target.let), gr_let2numSHINY(unique(walNew$target.let), centre = T), country = "Wales")
#save(walGrLookup, file = "data/welshSitesXY.rdata")
#scoGrLookup <- cbind(letGr = unique(scoNew$target.let), gr_let2numSHINY(unique(scoNew$target.let), centre = T), country = "Scotland")
#save(scoGrLookup, file = "data/scottishSitesXY.rdata")
#roiGrLookup <- cbind(letGr = unique(roieireNew$target.let), gr_let2numSHINY(unique(roieireNew$target.let), centre = T), country = "Republic of Ireland")
#save(roiGrLookup, file = "data/roirelandSitesXY.rdata")
#niGrLookup <- cbind(letGr = unique(neireNew$target.let), gr_let2numSHINY(unique(neireNew$target.let), centre = T), country = "Northern Ireland")
#save(niGrLookup, file = "data/northirelandSitesXY.rdata")
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

##
ui <- fluidPage(
tabsetPanel(
  ## Intro
  tabPanel(
    h3("Introduction"),
    br(),
    "This app has been created to allow for the quick visualisation of neighbourhoods used in the Frescalo method",
    tags$a(href="https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/j.2041-210X.2011.00146.x", "(Hill, 2012)."),
    "Files for British and Irish countries, and one or two other areas, are given as downloads via the links below.",
    hr(),
    h4("Britain and Ireland"),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Britain.zip", "Britain"),
    br(),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/England.zip", "England"),
    br(),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Wales.zip", "Wales"),
    br(),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Scotland.zip", "Scotland"),
    br(),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Ireland.zip", "Ireland"),
    br(),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Northern\ Ireland.zip", "Northern Ireland"),
    br(),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Republic\ of\ Ireland.zip", "Republic of Ireland"),
    br(),
    hr(),
    h4("British and Irish counties"),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Oxon.zip", "Oxfordshire"),
    br(),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Leics.zip", "Leicestershire"),
    br(),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/Wilts.zip", "Wiltshire"),
    br(),
    hr(),
    h4("International"),
    tags$a(href="https://github.com/sacrevert/frescaloNeighbourhoods/blob/main/IDF.zip", "Ile-de-France"),
    br(),
    hr(),
    h4("Contact"),
    HTML("These neighbourhoods were created by Dr Oli Pescott at the Biological Records Centre within UKCEH Wallingford.
      <a href='mailto:olipes@ceh.ac.uk'>Contact Dr Pescott</a> for more information on their creation.")
  ),
  ## Main panel for experts
  tabPanel(
    h3("Frescalo neighbourhood visualisations"),
    sidebarLayout(
      sidebarPanel(
        #style = "overflow-y: auto; overflow-x: auto;",
        h4("Select neighbourhood weights file"),
        h5("Columns in this order: target site, neighbour site, weight"),
        fileInput('target_upload1', 'Choose file to upload (in rdata format)',
                  accept = c(
                    '.rdata'
                  )),
        h4("Select all sites coordinates (x/y) file"),
        h5("Columns in this order: site name, easting/longitude, northing/latitude"),
        fileInput('target_upload2', 'Choose file to upload (in rdata format)',
                  accept = c(
                    '.rdata'
                  )),
        h4("Coordinate system"),
        selectizeInput(inputId = "epsg", label = ("Choose the EPSG code used for your site coordinates projection, or add the EPSG code"),
                    choices = c("27700 (Britain)" = "27700", "29902 (Ireland)" = "29902", "4326 (Global)" = "4326", "2154 (France)" = "2154"), 
                    selected = NULL, multiple = F, options = list(create = TRUE)),
        h4("Scale"),
        selectizeInput(inputId = "scale", label = ("Choose visualisation scale, or add your own value in km as a number"),
                    choices = c("Hectad (10 km)" = "10", "Tetrad (2 km)" = "2"), selected = NULL, multiple = F, options = list(create = TRUE)),
        h4("Target site"),
        selectizeInput(inputId = "site", label = ("Select target site neighbourhood to visualise"),
                    choices = NULL, selected = NULL, multiple = F),
        ),
      
      mainPanel(
        #br(),
        #hr(),
        h4("Selected Frescalo neighbourhood"), # add output text here
        HTML("<li>For the selected site unit, the site neighbourhood of this target (including the target itself) will be mapped.</li>
             <li>The radius of each circle is determined by the weight assigned in the file provided by the user, and is scaled for visualisation by the \"Scale\" number selected opposite.</li>
             <li>Note that here all squares in a neighbourhood are mapped, whereas in Hill (2012) the visualisation suppressed
             squares with weights <0.05.</li>"),
        br(),
        leafletOutput('map', height = '1000px', width = '800px'),#), 
        br(),
        hr(),
        dataTableOutput("neighbourhood"),
        br(),
        hr()
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {

  siteList <- reactive({
    
    inFile <- input$target_upload1 # All target sites, neighbour sites, neighbour weights
    if (is.null(inFile))
      return(NULL)
    sites <- loadRData(inFile$datapath)
    df <- unique(sites[,1]) # unique list of sites
    return(df)
    
    })
  
  targetList <- reactive({
    
    inFile <- input$target_upload1 # Selected target site, neighbour site, neighbour weight
    if (is.null(inFile))
      return(NULL)
    target <- input$site
    if (is.null(target))
      return(NULL)
    sites <- loadRData(inFile$datapath)
    df <- sites[sites[,c(1)]==target,]
    return(df)
    
  })
  
  siteCoords <- reactive({
    
    inFile <- input$target_upload2 # site name, easting, northing, country (optional)
    if (is.null(inFile))
      return(NULL)
    coords <- loadRData(inFile$datapath)
    dat <- targetList()
    df <- coords[coords[,1] %in% dat[,2],]
    return(df)
    
  })
  
  
  # Update list of sites available for selection based on uploaded lists
  observeEvent(input$target_upload1, {
    
    updateSelectInput(session = session,
                        inputId = 'site',
                        choices = siteList())
    
  })
  
  points <- reactive({
    
    scl <- as.numeric(input$scale)
    if (is.null(scl))
      return(NULL)
    epsg <- input$epsg
    if (is.null(epsg))
      return(NULL) 
    site <- input$site
    if (is.null(site))
      return(NULL)
    dat <- targetList() # filtered list of sites with neighbours and weights
    if (is.null(dat))
      return(NULL)  
    coords <- siteCoords() # x/y coords for all these sites
    if (is.null(coords))
      return(NULL)
    datXY <- merge(dat, coords, by.x = names(dat)[2], by.y = names(coords)[1], all.x = T, all.y = F) # x/y should be columns 4 and 5 now
    #coordinates(datXY) ~EASTING+NORTHING
    #crs(datXY) <- paste0("+init=EPSG:",input$epsg)
    ## st_buffer fails if weights too small
    datXY$weights <- ifelse(datXY$weights < 1.0e-10, 1.0e-10, datXY$weights)
    if (is.null(epsg) | is.null(scl)) {
        return(NULL) } else {
      datSf <- st_as_sf(datXY, coords = c(names(datXY)[4], names(datXY)[5]), crs = as.numeric(epsg))
      if(scl == 10) {
        distSel <- 4000 } else if (scl == 2) {
        distSel <- 1000 } else {
        distSel <- (scl*1000)/2  
        }
      datSfbuf <- st_buffer(datSf, dist = distSel*datSf$weights)
      datSf2 <- st_transform(datSfbuf, 4326)
        }
  })
  
  output$map <- renderLeaflet({
    
    site <- input$site
    if (is.null(site))
      return(NULL)
    datSf2 <- points()
    if (is.null(datSf2))
      return(NULL)
    epsg <- input$epsg
    if (is.null(epsg))
      return(NULL)  
    datSp <- as_Spatial(datSf2)
    Lcrs <- leafletCRS(crsClass = "L.Proj.CRS", code = "WGS84",
                       proj4def = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                       resolutions = 1,#1.5^(25:15),
                       bounds=c(-20,5,48,65))
    if (epsg == "2154") { # France
    leaflet(datSp) %>%
      setView(2.4, 48.9, zoom = 7) %>%
      setMaxBounds(lng1 = idfBB['xmin'], lat1 = idfBB['ymin'], lng2 = idfBB['xmax'], lat2 = idfBB['ymax']) %>%
      # Base groups
      addTiles(group = "OSM") %>%
      addProviderTiles(provider=providers$Esri.WorldImagery, group = "Satellite") %>%
      # Overlay neighbourhood
      addPolygons(opacity = 0.7, smoothFactor = 0.5, group = "Neighbourhood") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM", "Satellite"),
        overlayGroups = c("Neighbourhood"),
        options = layersControlOptions(collapsed = FALSE))
    } else { # UK/Ireland
      leaflet(datSp) %>%
        setView(-2.9, 59.9, zoom = 6) %>%
        setMaxBounds(-20,48,5,65) %>%
        # Base groups
        addTiles(group = "OSM") %>%
        addProviderTiles(provider=providers$Esri.WorldImagery, group = "Satellite") %>%
        # Overlay neighbourhood
        addPolygons(opacity = 0.7, smoothFactor = 0.5, group = "Neighbourhood") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          overlayGroups = c("Neighbourhood"),
          options = layersControlOptions(collapsed = FALSE))
      
    }
    
  })
  
  output$neighbourhood <- DT::renderDataTable({
    
    df <- targetList()
    if (is.null(df))
      return(NULL)
    datatable(df)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
