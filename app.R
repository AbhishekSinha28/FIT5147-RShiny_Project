library(leaflet)
library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(readxl)
library(treemap)

vic.lga.shp <- readShapeSpatial("data/vmlite_lga_cm/vmlite_lga_cm.shp")
data_2 <- read_excel(here::here("data","Data_Tables_LGA_Criminal_Incidents_Year_Ending_March_2020.xlsx"), sheet = "Table 02")

data_2$`Local Government Area` <- toupper(data_2$`Local Government Area`)

data_2 <- data_2 %>% select(c("Year","Local Government Area","Incidents Recorded","LGA Rate per 100,000 population"))

data_2 <- data_2 %>% mutate(`Local Government Area` = recode(`Local Government Area`, "COLAC-OTWAY" = "COLAC OTWAY"))

data_2_total <- data_2 %>% group_by(Year,`Local Government Area`) %>% summarise(`LGA Rate per 100,000 population` = sum(`LGA Rate per 100,000 population`))

data_2_total <- data_2_total %>% mutate(log_LGA_Rate_per_100000_pop = log(`LGA Rate per 100,000 population`))

colnames(vic.lga.shp@data)[6] = "Local Government Area"

merge.lga.profiles3<-sp::merge(vic.lga.shp, data_2_total, 
                               by="Local Government Area", duplicateGeoms = TRUE)

data_3 <- read_excel(here::here("data","Data_Tables_LGA_Criminal_Incidents_Year_Ending_March_2020.xlsx"), sheet = "Table 03")

data_3 <- data_3 %>% select(c("Year","Local Government Area","Offence Division","Incidents Recorded"))

data_odiv <- data_3 %>% group_by(`Local Government Area`, `Offence Division`, Year) %>% summarise(`Incidents Recorded` = sum(`Incidents Recorded`))

data_odiv$Year <- as.character(data_odiv$Year)

data_odiv$`Local Government Area` <- toupper(data_odiv$`Local Government Area`)

data_4 <- read_excel(here::here("data","Data_Tables_LGA_Criminal_Incidents_Year_Ending_March_2020.xlsx"), sheet = "Table 04")

data_tree <- data_4 %>% select(c("Year","Local Government Area","Location Subdivision","Incidents Recorded"))

data_tree <- data_tree %>% group_by(Year, `Local Government Area`, `Location Subdivision`) %>% summarise(`Incidents Recorded` = sum(`Incidents Recorded`))

data_tree$`Local Government Area` <- toupper(data_tree$`Local Government Area`)

data_5 <- read_excel(here::here("data","Data_Tables_LGA_Criminal_Incidents_Year_Ending_March_2020.xlsx"), sheet = "Table 05")

data_sun <- data_5 %>% select(c("Year","Local Government Area","Investigation Status","Incidents Recorded"))

data_sun$Year <- as.character(data_sun$Year)

data_sun$`Local Government Area` <- toupper(data_sun$`Local Government Area`)

ui <- dashboardPage(
  
  dashboardHeader(title = span("Crime Statistics in Victoria", style = "font-weight: bold;font-size: 15px")),
  ## Sidebar content
  dashboardSidebar( 
    
    sidebarMenu(id = "tabs",
                
                
                menuItem("Crime Rate", tabName = "Crime", icon = icon("balance-scale"),
                         menuItem(tabName = "Crime", selected = NULL,
                                  selectInput("var1", "Type Of Crime", choices = data_odiv$`Offence Division`, selected = "Select", multiple = FALSE)), helpText("Change the Offence Type"),
                         selectInput("var2", "Select LGA", choices = data_odiv$`Local Government Area`, selected = "Select", multiple = FALSE), helpText("Change the LGA"),
                         selectInput("var3", "Select Year", choices = data_tree$Year, selected = "Select", multiple = FALSE), helpText("Change the Year"),
                         selectInput("var4", "Select Status of Charges", choices = data_sun$`Investigation Status`, selected = "Select", multiple = FALSE), helpText("Change the Investation Status")))),
  
  
  ## Body content
  dashboardBody(
    tabItems( 
      tabItem(tabName = "Crime",
              h2(""),
              fluidRow(
                box(title = "Recorded Offences for LGAs per 100,000 pop Map", status = "primary",solidHeader = TRUE,leafletOutput("VicMap", height = 400), width = 100),
                box(title = "Overall Trend in recorded incedents in a LGA", status = "primary", solidHeader = TRUE, plotlyOutput("LGA_Trend", height = 400), width = 100),
                box(title = "Overall Trend in recorded incedents for an offence type", status = "primary", soliderHeader = TRUE, plotlyOutput("Offence_Division", height = 400),width =100),
                plotOutput("threemap_population_country",height="600px"),
                box(title = "Investigation Status", status = "primary", solidHeader = TRUE, plotlyOutput("Investigation_Status", height = 400), width = 100)
                
                
  
                
                
              )
    )
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #filtered_offence <- reactive(crimeinVic1[crimeinVic1$type == input$slider,])
  #filtered_lga <- reactive(LGA1[LGA1$lga_name == input$lga,])
  
  output$VicMap <- renderLeaflet({
    
    title1 <- tags$div(
      HTML('<h3>Crime in Victoria</h3>')
    )
    pals <- colorNumeric(
      "YlOrRd",
      domain = merge.lga.profiles3$log_LGA_Rate_per_100000_pop
    )
    
    
    #set up labels for the hover
    labels1 <- sprintf(
      "<strong>%s</strong><br/><h6>%g</h6>",
      merge.lga.profiles3$`Local Government Area`,
      merge.lga.profiles3$log_LGA_Rate_per_100000_pop
    ) %>% lapply(htmltools::HTML)
    
    #display map
    map1 <- leaflet(merge.lga.profiles3) %>%
      addTiles()
    map1 %>% 
      addPolygons(
        fillColor = ~pals(log_LGA_Rate_per_100000_pop),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels1,
        
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto"
        ))%>% 
      addLegend(pal = pals, 
                values = ~log_LGA_Rate_per_100000_pop, 
                opacity = 0.7, title = "Log of Cases by LGA per 100,000",
                position = "bottomright") %>% 
      
      addControl(title1, position = "topright")
    
  })
  
  
  output$Offence_Division <- renderPlotly({
    data_odiv %>%
      filter(`Local Government Area` == input$var2, `Offence Division` == input$var1) %>%
      ggplot(aes(Year, `Incidents Recorded`)) +
      geom_bar(stat = "identity",fill = "orange", position = "dodge") +
      theme_bw() +
      labs(title = "Recorded Incidents Overtime for selected Offence type", x = "Year", y = "Incidents Recorded")
  })
  
  
  output$LGA_Trend <- renderPlotly({
    data_odiv %>%
      filter(`Local Government Area` == input$var2) %>%
      ggplot(aes(Year, `Incidents Recorded`)) +
      geom_bar(stat = "identity",fill = "navyblue", position = "dodge") +
      theme_bw() +
      labs(title = "Recorded Incidents Overtime", x = "Year", y = "Incidents Recorded")
  })

  
  output$threemap_population_country <- renderPlot({
    temp=data_tree %>% filter(Year == input$var3 & `Local Government Area` == input$var2)
    .tm <<- treemap(temp, 
                    index="Location Subdivision", 
                    vSize="Incidents Recorded", 
                    vColor="Incidents Recorded",
                    type="value",
                    title = "Tree Map for recorded incidents by type of location in a LGA",
                    palette="Blues",
                    border.col ="white",
                    position.legend="right",
                    fontsize.labels = 16,
                    title.legend="Count")
  })
  
  output$Investigation_Status <- renderPlotly({
    data_sun %>%
      filter(`Local Government Area` == input$var2, `Investigation Status` == input$var4) %>%
      ggplot(aes(x = Year, y = `Incidents Recorded`)) +
      geom_col(fill = "red", position = "dodge") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Year", y = "Crime Incidents")
  })
  
}
shinyApp(ui = ui, server = server)