#
#  East Group 2 Dashboard
#

library(shiny)
library(tidyverse)
library(reshape2)
library(dplyr)
library(sf)
library(ggmap)
library(leaflet)
library(ggplot2)
library(lubridate)
library(ggmap)
library(basictabler)
library(htmlwidgets)
library(scales)

##  The Pre-Code

# Parks & Schools Tab code

# loading the files
councils <- st_read("City_Council_Districts/City_Council_Districts.shp", stringsAsFactors = FALSE)
schoollocations <- school <- st_read("School_Boundaries/School_Boundaries.shp", stringsAsFactors = FALSE) 
parks <- read_csv("Parks_Locations_and_Features.csv")

councils.data <- councils %>% st_set_geometry(NULL)
school.data <- schoollocations %>% st_set_geometry(NULL)
parks.spatial <- parks %>% st_as_sf(coords=c("Lon", "Lat")) %>% st_set_crs(value=4326)

# create the color palettes to color the points
distpal <- colorFactor(palette = 'Set1', domain =councils.data$Num)


parkpal <- colorFactor(palette = 'Set1', domain =parks.spatial$Park_Type)
schoolpal <- colorFactor(palette = c("maroon", "black"), domain =schoollocations$SchoolType)

# join the polygons and points
schoolparks <- st_join(x=schoollocations, y=councils%>%select(Num))
schoolparks %>% group_by(Num)

# create the popups
parks.spatial$Parkpopup <-  paste("<b>",parks.spatial$Park_Name,"</b><br>",
                                  "Type: ",parks.spatial$Park_Type,"<br>",
                                  "Address: ",parks.spatial$Address,sep =" ")

schoollocations$Schoolpopup <-  paste("<b>",schoollocations$School,"</b><br>",
                                      "Type: ", schoollocations$SchoolType,"<br>")

# Abandoned Property tab code

#Loading the shp files for districts and abandoned properties
ap_shp <- st_read("Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE)
districts <- st_read("City_Council_Districts/City_Council_Districts.shp", stringsAsFactors = FALSE)  

#Updating from geometry to point to improve performance and create the ability to use circular markers
df <- st_centroid(ap_shp)

#Cleaning the data by reducing NAs, updating column names, creating grouping data element and updating NA to Unknown
df <- df %>%
    filter(!is.na(Date_of_Ou))

df <- df %>% 
    rename(Status = Outcome_St,District = Council_Di, CompletionDt = Date_of_Ou)

df$CompletionDt <- as.Date(df$CompletionDt)

df$Type <- ifelse(startsWith(df$Structures,'House'),"Single Family Home",
                  ifelse(startsWith(df$Structures,'Multi'),"Multi Family Home",
                         ifelse(startsWith(df$Structures,'Commercial'),"Commercial Unit",
                                ifelse(startsWith(df$Structures,'Duplex'),"Multi Family Home","Other"))))

df$Type <- df$Type %>%
    replace_na("Unknown")

Districts <- unique(districts$Num)
Outcomes <- unique(df$Status)
Type <- unique(df$Type)

#Creating palettes for output
distpal_ap <- colorFactor(palette = c('Set1'), domain = unique(districts$Num))
typepal <- colorFactor(palette = c('Set1'), domain = unique(df$Type))

# 311 tab code

calls <- read.csv('311_Phone_Call_Log_Mod.csv') %>%
  mutate(textMonth = months(as.Date(Call_Date)))




# Define UI 
ui <- navbarPage(title = "South Bend Dashboard",
                 tabPanel(title = "Parks & Schools",
                          sidebarLayout(
                              sidebarPanel(width = 2,
                                  selectInput(
                                      inputId="District",
                                      label= "District ID",
                                      choices=c("All", "None", councils.data[,"Num"]),
                                  ), 
                                  
                                  selectInput(
                                      inputId="ParkType",
                                      label= "Park Type",
                                      choices=c("All", "None", parks[,"Park_Type"]),
                                  ),
                                  
                                  selectInput(
                                      inputId="SchoolType",
                                      label= "School Type",
                                      choices=c("All", "None", school.data[,"SchoolType"]),
                                  )
                              ),
                              mainPanel(
                                leafletOutput(outputId = "schoolparkmap",height = 800 , width = 1400)
                                    )#end mainPanel
                          ) # end sidebarLayout
                 ),#end tab panel first
  
                 tabPanel(title = "Abandoned Property",
                          sidebarLayout(
                              sidebarPanel(width = 2,
                                         # column(width = 2,
                                         selectInput(inputId = "variable1",
                                                     label = "Status",
                                                     choices = c(df$Status),
                                                     multiple = TRUE,
                                                     selected = Outcomes),
                                         selectInput(inputId = "variable2",
                                                     label = "District",
                                                     choices = c(districts$Num),
                                                     multiple = TRUE,
                                                     selected = Districts),
                                         selectInput(inputId = "variable3",
                                                     label = "Type",
                                                     choices = c(df$Type),
                                                     multiple = TRUE,
                                                     selected = Type),
                                         dateRangeInput('daterange',
                                                        label = 'Date range input: mm/yyyy',
                                                        start = min(as.Date(df$CompletionDt)),
                                                        end = max(as.Date(df$CompletionDt)),
                                                        format = "mm/yyyy"),
                                         basictablerOutput('total'),
                                         basictablerOutput('by_district'),
                                         basictablerOutput('by_outcome'),
                                         plotOutput('hist', height = 220)
                            ), # end sidebarPanel
                            #Main body map determination
                            mainPanel(
                              leafletOutput("mappy",height = 800 , width = 1400)
                            ) # end mainPanel
                          ) # end sidebarLayout                          
                 ),#end tab panel second
                 
                 tabPanel(title = "311 Calls",
                          sidebarLayout(
                            sidebarPanel(width = 2,
                              selectInput(inputId = "monthChooser",
                                          label = "Month: ",
                                          choices = c("All", unique(calls$textMonth))),
                              checkboxGroupInput(inputId = "departmentChooser",
                                                 label = "Select Departments",
                                                 choices = c("All", unique(calls$Department)),
                                                 selected = "All"
                                                 
                              )
                            ),# end sidebar panel
                            
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel(title = "Total Calls",
                                                   plotOutput(outputId = "totCallsPlot")
                                          ),
                                          tabPanel(title = "Call Duration Spans",
                                                   plotOutput(outputId = "boxPlot")
                                          ),
                                          tabPanel(title = "Call Duration Averages",
                                                   plotOutput(outputId = "barPlot")
                                          )) # end tabsetPanel
                            )# end mainPanel
                          )  #end sidebar layout                         
                          )# end tab panel third
    ) # end navbarPage


# Define server logic 

server <- function(input, output) {

 # Sundeep's code
  
    dist.subset <- reactive({
        if (input$District== "All"){
            dist_rtrn <- councils
        }else{
            dist_rtrn <- councils %>% dplyr::filter(Num %in% input$District)
        }
        return(dist_rtrn)
    }) # end reactive 1
    
    park.subset <- reactive({
        if (input$ParkType=="All"){
            park_rtrn <- parks.spatial
        }else{
            park_rtrn <- parks.spatial %>% dplyr::filter(Park_Type %in% input$ParkType)
        }
        return(park_rtrn)
    }) # end reactive 2
    
    school.subset <- reactive({
        if (input$SchoolType =="All"){
            school_rtrn <- schoollocations
        }else{
            school_rtrn <-schoollocations%>% dplyr::filter(SchoolType %in% input$SchoolType)
        }
        return(school_rtrn)
    }) # end reactive 3
    
    output$schoolparkmap <- renderLeaflet({
        parks.spatial$Parkpopup <-  paste("<b>",parks.spatial$Park_Name,"</b><br>",
                                             "Type: ",parks.spatial$Park_Type,"<br>",
                                             "Address: ",parks.spatial$Address,sep =" ")
        
        schoollocations$Schoolpopup <-  paste("<b>",schoollocations$School,"</b><br>",
                                          "Type: ", schoollocations$SchoolType,"<br>")

        leaflet()%>%
            addTiles()%>%
            addPolygons(data = dist.subset(), color=~distpal(dist.subset()$Num))%>% 
            addPolygons(data=school.subset(), popup=~Schoolpopup, color=~schoolpal(SchoolType), stroke=0, fillOpacity =2) %>%
            addCircleMarkers(data=park.subset(), popup=~Parkpopup, color=~parkpal(Park_Type), stroke=0, fillOpacity = 2, radius = 4) %>%
            addLegend("bottomleft",pal = schoolpal, values = school.data$SchoolType, title =  "School Types", opacity = 2) %>%
            addLegend("bottomright",pal = parkpal, values = parks$Park_Type, title =  "Park Types", opacity = 2)
            
      }) # end schoolparkmap

    # Nick's code 
    
    #Total Count 
    output$total <- renderBasictabler({
      sum_count <- as.data.frame(df) %>%
        filter((CompletionDt >= input$daterange[1]) & (CompletionDt <= input$daterange[2]) &
                 (District %in% input$variable2) & (Status %in% input$variable1) &
                 Type %in% input$variable3) %>%
                count() %>% arrange_()
      tbl <- BasicTable$new()
      tbl$addData(sum_count, firstColumnAsRowHeaders=TRUE,
                  explicitColumnHeaders=c("Total Count"))
      basictabler(tbl)
    }) # end total count
    
    #Count by District table
    output$by_district <- renderBasictabler({
      district_count <- as.data.frame(df) %>%
        filter((CompletionDt >= input$daterange[1]) & (CompletionDt <= input$daterange[2]) &
                 (District %in% input$variable2) & (Status %in% input$variable1) & Type %in% input$variable3) %>%
                  dplyr::select(District) %>%  group_by(District) %>% count()%>% arrange_()
      tbl <- BasicTable$new()
      tbl$addData(district_count, firstColumnAsRowHeaders=TRUE,
                  explicitColumnHeaders=c("District","Total Count"))
      basictabler(tbl)
    }) # end count by district

    #Count by Outcome table
    output$by_outcome <- renderBasictabler({
      outcome_count <- as.data.frame(df) %>%
        filter((CompletionDt >= input$daterange[1]) & (CompletionDt <= input$daterange[2]) &
                 (District %in% input$variable2) & (Status %in% input$variable1) &
                 Type %in% input$variable3) %>%  dplyr::select(Status) %>%
                  group_by(Status) %>%  count()%>%  arrange_()
      tbl <- BasicTable$new()
      tbl$addData(outcome_count, firstColumnAsRowHeaders=TRUE,
                  explicitColumnHeaders=c("Status","Total Count"))
      basictabler(tbl)
    })  # end count by outcome 
    
    #Count by year bar chart
    output$hist <- renderPlot({
      df %>%
        filter((CompletionDt >= input$daterange[1]) & (CompletionDt <= input$daterange[2]) & (District %in% input$variable2) &
                 (Status %in% input$variable1) & Type %in% input$variable3) %>%
        ggplot(., aes(x = year(CompletionDt))) +
        geom_bar() +
        xlab("Year") 
    }) # end count by year
    
    #Define Map
    output$mappy <- renderLeaflet({
      #District subset
      dist.subset <-   districts %>% filter(Num %in% input$variable2)
      #Abandoned properties subset
      df.subset <- df %>% filter((CompletionDt >= input$daterange[1]) & (CompletionDt <= input$daterange[2]) &
                                   (District %in% input$variable2) & (Status %in% input$variable1) & Type %in% input$variable3)
      #Map Output
      leaflet() %>%
        addTiles() %>%
        addLegend("bottomright",pal = distpal_ap, values = districts$Num,title =  "District", opacity = .75) %>%
        addLegend("bottomleft",pal = typepal,values = df$Type, title = "Building Type", opacity = 1) %>%
        addCircleMarkers(data = df.subset,color = ~typepal(df.subset$Type),radius = 1, opacity = 1) %>%
        addPolygons(data = dist.subset,color = ~distpal_ap(dist.subset$Num),weight = 6,
     #    addPolygons(data = dist.subset,fillColor = "transparent",color = ~distpal_ap(dist.subset$Num),weight = 6,
      #  addPolygons(data = dist.subset(),color = ~distpal_ap(dist.subset()$Num),weight = 6,
        )
    })  # end map
 
      
    # Gai's code 
 
     calls.filt <- reactive ({
      
      # filter by month
      if(input$monthChooser =="All"){
        calls_filt.month <- calls
      }  else {
        calls_filt.month <- calls %>% 
          filter(textMonth %in% input$monthChooser)
      }
      
      # filter by Department 
      if(input$departmentChooser =="All"){
        calls_filt.data <- calls_filt.month
      }  else {
        print(input$departmentChooser)
        calls_filt.data <- calls_filt.month %>% 
          filter(Department %in% input$departmentChooser)
      }
      return(calls_filt.data)
    }) # end reactive filter
    
    
    output$totCallsPlot <- renderPlot({
      ggplot(calls.filt() %>% group_by(Department) %>% na.omit %>%
               summarise(duration_Seconds = sum(duration_Seconds), n=n()) %>%
               mutate(avg_sec_percall = round(duration_Seconds/n, 1)) %>%
               rename(total_num_calls = 'n'),
             aes(x = total_num_calls, y = Department, fill = avg_sec_percall)) + 
        geom_bar(stat = "identity", position = "dodge") +
        #   theme(legend.position = "bottom") +
        xlab(NULL) +
        ylab(NULL) +
        labs(title = "City of South Bend -- '311' Calls", subtitle = "Call Volume by Department ") +
        theme(plot.title = element_text(color = "darkblue"),
              plot.subtitle = element_text(color = "darkblue")) 
      
    }) # end total calls plot
    
    
    output$barPlot <- renderPlot({
      ggplot(calls.filt() %>% group_by(Department) %>% na.omit %>%
               summarise(duration_Seconds = sum(duration_Seconds), n=n()) %>%
               mutate(avg_sec_percall = round(duration_Seconds/n, 1)) %>%
               # mutate(Department = fct_reorder(Department, avg_sec_percall, .desc = FALSE)) %>%
               rename(total_num_calls = 'n'),
             aes(x = avg_sec_percall, y = Department, fill = total_num_calls)) + 
        geom_bar(stat = "identity", position = "dodge") +
        #   theme(legend.position = "bottom") +
        xlab(NULL) +
        ylab(NULL) +
        labs(title = "City of South Bend -- '311' Calls", subtitle = "Average Duration by Department ") +
        theme(plot.title = element_text(color = "darkblue"),
              plot.subtitle = element_text(color = "darkblue")) 
      
    }) # end barplot
    
    
    output$boxPlot <- renderPlot({
      ggplot(calls.filt() %>% group_by(Department) %>%
               mutate(Average_Duration = mean(duration_Seconds, na.rm=TRUE)),
             #      aes(x = reorder(Department, Average_Duration),
             aes(x = Department,
                 y = duration_Seconds)) +
        geom_boxplot(aes(fill=Average_Duration)) +
        scale_fill_gradient2() +
        scale_y_continuous(trans='log10') +
        coord_flip() +
        theme(legend.position = "bottom") +
        xlab(NULL) +
        ylab(NULL) +
        labs(title = "City of South Bend -- '311' Calls", subtitle = "Duration by Department ") +
        theme(plot.title = element_text(color = "darkblue"),
              plot.subtitle = element_text(color = "darkblue")) 
      
    }) # end boxplot    
    
    
}# end server


# Run the application 
shinyApp(ui = ui, server = server)




