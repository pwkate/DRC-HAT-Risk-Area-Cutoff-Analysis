
library(shiny)
library(leaflet)
library(reactable)
library(raster)
library(sf)
library(terra)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("High Risk Areas of HAT in DRC"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(h4("High-risk Areas (per 10,000)"),
            numericInput(inputId = "cutoff",
                        label="Cut Off Value:",
                        value = 100,
                        min = 0,
                        max = 2500),
            actionButton(inputId = "go",
                         label = "GO")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("risk"),
          downloadButton(outputId = "shp"),
          tableOutput(outputId = "summary")
        )
    )
)

server <- function(input, output) {
  
  cased<-raster("clip_ratio.tif")
  drc<-st_read("cod_34s_t.shp")
  pop<-raster("landscan_34s.tif")
  
  #load("cased.RData")
  #load("pop.RData")
  #load("drc.RData")
  
  co_history <- reactiveVal(numeric(0))
  summary_dt <- reactiveVal(NULL)
  polyhr<-reactiveVal(NULL)
  polyhr34s<-reactiveVal(NULL)
  
  pal <- colorQuantile("Reds", values(cased),n=7,
                       na.color = "transparent")
  output$risk <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(cased, opacity = 0.6, colors = pal, project = TRUE)
  })
  
  observeEvent(input$go, {
    
    co_history(c(co_history(), input$cutoff))
    
    hr<- st_as_sf(rasterToPolygons(cased, fun=function(x){x >= input$cutoff}))
    hr<-st_union(st_geometry(hr))
    hr<- st_as_sf(hr)

    area_hr<-round(st_area(hr)/1000000,3)
    
    zv <- vect(hr)
    zr<-rast(pop)
    pop_hr <- zonal(zr, zv, fun='sum',na.rm=T)
    
    zt<-vect(drc)
    t_pop<-zonal(zr, zt, fun='sum',na.rm=T)
    
    t_area<-round(st_area(drc)/1000000,3)
    
    new_dt<-data.frame(Cutoff=input$cutoff, Area = area_hr,
                       Area_pct=round(area_hr/t_area*100,5),
                       Population = pop_hr$landscan_34s,
                       Pop_pct=round(pop_hr$landscan_34s/t_pop$landscan_34s*100,5))
    summary_dt(rbind(summary_dt(), new_dt))
    
    hrt<-st_transform(hr,crs=4326)
    
    polyhr(hrt)
    polyhr34s(hr)
    
    leafletProxy("risk") %>%
      clearShapes() %>%
      addPolygons(data = polyhr(), fillOpacity = 0, weight = 0.8, color = "black", opacity = 1)
    
  })
  
  output$summary <- renderTable({
    summary_dt()
  })
  
  output$shp <- downloadHandler(
    filename = function() {
      paste0("CutOff_", as.character(input$cutoff), ".shp")
    },
    content = function(file) {
      # Save the Excel file
      st_write(polyhr34s(), file)
    }
  )
  
  #output$risk <- renderLeaflet({
  #  leaflet() %>%
  #    addTiles() %>%
  #    addRasterImage(cased, opacity = 0.8,colors = pal,project = T) %>% 
  #    addPolygons(data=polyhr(),fillOpacity = 0,weight = 0.5,color="black",opacity=1)
  #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
