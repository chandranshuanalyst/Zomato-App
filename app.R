#Libraries loaded
library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(leaflet)
library(terra)
#Data reading
data=read.csv("Data/zomato.csv")
data2=read_excel("Data/Country-Code.xlsx")
#Data modification
data2$Country.Code<-data2$`Country Code`
data_merged=merge(data,data2,by="Country.Code")

#Points identified
#1.We see there are about 500 Indian restaurants with latitude and longitude 0.
#2.There are 18 rows with average cost of 2 equal to 0.
#3. As only about 6% of data is distorted, we decide to drop them.
#Data Manipulation
data3=data_merged[(data_merged$Longitude!=0),]
data3=data3[(data3$Average.Cost.for.two!=0),]

rest_count=data3%>%
    group_by(Country)%>%
    summarise(Number_of_restaurents=n(),.groups = 'drop') %>% 
    as.data.frame()

avg_cost=data3%>%
    group_by(Country)%>%
    summarise(mean_cost=mean(Average.Cost.for.two),.groups = 'drop') %>% 
    as.data.frame()

online_delivery=data3%>%
    group_by(Country)%>%
    summarise(online_service_yes=sum(Has.Online.delivery == "Yes"),.groups = 'drop') %>% 
    as.data.frame()

table_booking=data3%>%
    group_by(Country)%>%
    summarise(online_service_yes=sum(Has.Table.booking == "Yes"),.groups = 'drop') %>% 
    as.data.frame()

title<-tags$a(href='https://www.zomato.com/india',
              tags$img(src="logo.jpg"))
#Defining Ui
ui<- dashboardPage(skin = "red",
    dashboardHeader(title=title),
    dashboardSidebar(
        sidebarMenu(id="sidebarmenu",
                    menuItem("Dataset",tabName = "D"
                             ),
                    menuItem("Restaurent Analysis" ,tabName ="RA",icon=icon("building"),
                             menuSubItem("Country Analysis",tabName="CA"),
                             menuSubItem("City Analysis",tabName="CiA")),
                    menuItem("User Guide",tabName = "UG"),
                    conditionalPanel("input.sidebarmenu === 'CA'",
                                     selectInput("country_input","Select country",unique(data3$Country))
                    ),
                    conditionalPanel("input.sidebarmenu === 'CiA'",
                                     selectInput("country_input2","Select country",unique(data3$Country)),
                                     selectInput("city_input","Select City",unique(data3$Country))
                    )
        )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "D",
                    h2("Dataset"),
                    #Input: Choose dataset ----
                        selectInput("dataset", "Choose a dataset:",
                                    choices = c("Zomato", "Country Code", "Combined data")),
                    
                    # Button
                    downloadButton("downloadData", "Download")
                    ),
            tabItem(tabName ="CA",
                    h2("Restaurent Analysis Country Wise"),
                    fluidRow(
                        infoBoxOutput("rest"),
                        infoBoxOutput("cost"),
                        infoBoxOutput("online"),
                        infoBoxOutput("table")
                    ),
                    fluidRow(
                        dataTableOutput("top_rest")
                    ),
                    fluidRow(
                        plotOutput("plot1")
                    )
            ),
            tabItem(tabName = "CiA",
                    h2("City Wise Analysis"),
                    fluidRow(
                        infoBoxOutput("rest2"),
                        infoBoxOutput("cost2"),
                        infoBoxOutput("online2"),
                        infoBoxOutput("table2")
                    ),
                    fluidRow(
                        plotOutput("plot2")
                    ),
                    fluidRow(
                        plotOutput("plot3")
                    ),
                    fluidRow(
                        plotOutput("plot4")
                    )
            ),
            tabItem(tabName = "UG",
                    h2("User Guide For Zomato Restaurent"),
                    selectInput("Country_list3","Select Country",unique(data3$Country)),
                    selectInput("City_list3","Select City",unique(data3$City)),
                    pickerInput("Cuisine_list3","Select Cuisine/s", choices=NULL, options = list(`actions-box` = TRUE),multiple = F),
                   leafletOutput("map"),
                   dataTableOutput("table3")
            ))
    )
)
#Defining server
server<-function(input,output,session){
    
    observe({
        updateSelectInput(session, "city_input", choices = unique(data3[data3$Country==input$country_input2,"City"]))
    })
    
    observe({
        updateSelectInput(session, "City_list3", choices = unique(data3[data3$Country==input$Country_list3,"City"]))
    })
    
    observe({
        updatePickerInput(session, "Cuisine_list3", choices = unique(data3[data3$City==input$City_list3,"Cuisines"]))
    })
    
    # Define reactive values for selected country
    selected_country <- reactive({
        # Access the selected country from input$country_input
        input$country_input
    })
    
    datasetInput <- reactive({
        switch(input$dataset,
               "Zomato" = data,
               "Country Code" = data2,
               "Combined data" = data_merged)
    })
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
    
    
    output$rest <- renderInfoBox({
        r<-rest_count[rest_count$Country==input$country_input,2]
        infoBox(
            "Number of Restaurents",r, icon = icon("list"),
            color = "purple"
        )
    })
    output$rest2 <- renderInfoBox({
        rest_count2=data3%>%
            group_by(City)%>%
            summarise(Number_of_restaurents=n(),.groups = 'drop') %>% 
            as.data.frame()
        r2<-rest_count2[rest_count2$City==input$city_input,2]
        infoBox(
            "Number of Restaurents",r2, icon = icon("list"),
            color = "purple"
        )
    })
    
    output$cost <- renderInfoBox({
        a<-avg_cost[avg_cost$Country==selected_country(),2]
        infoBox(
            "Cost of 2", a, icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$cost2 <- renderInfoBox({
        avg_cost2=data3%>%
            group_by(City)%>%
            summarise(mean_cost=mean(Average.Cost.for.two),.groups = 'drop') %>% 
            as.data.frame()
        a2<-avg_cost2[avg_cost2$City==input$city_input,2]
        infoBox(
            "Cost of 2", a2, icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$online <- renderInfoBox({
        infoBox(
            "Online Service",online_delivery[online_delivery$Country==selected_country(),2] , icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$online2<- renderInfoBox({
        online_delivery2=data3%>%
            group_by(City)%>%
            summarise(online_service_yes=sum(Has.Online.delivery == "Yes"),.groups = 'drop') %>% 
            as.data.frame()
        infoBox(
            "Online Service",online_delivery2[online_delivery2$City==input$city_input,2] , icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
    
    output$table <- renderInfoBox({
        infoBox(
            "Table service available",table_booking[table_booking$Country==selected_country(),2], icon = icon("list"),
            color = "purple"
        )
    })
    
    output$table2<- renderInfoBox({
        table_booking2=data3%>%
            group_by(City)%>%
            summarise(online_service_yes=sum(Has.Table.booking == "Yes"),.groups = 'drop') %>% 
            as.data.frame()
        infoBox(
            "Table service available",table_booking2[table_booking2$City==input$city_input,2], icon = icon("list"),
            color = "purple"
        )
    })
    
    output$top_rest<-renderDataTable(
        datatable(data3[data3$Country==selected_country(),][order(data3[data3$Country==selected_country(),]$Aggregate.rating, decreasing = TRUE), ], options = list(
            columnDefs = list(list(className = 'dt-center', targets = 4)),
            pageLength = 5,
            lengthMenu = c(3,5,7,10),
            scrollX = TRUE
        ))
    )
    output$plot2<-renderPlot({
        p2=data3[data3$City==input$city_input,]%>%
            group_by(Locality)%>%
            summarise(Count_of_restaurents=n(),.groups = 'drop') %>%
            arrange(desc(Count_of_restaurents))%>%
            as.data.frame()
        
        ggplot(p2, aes(x=Locality, y=Count_of_restaurents,fill=Count_of_restaurents)) +
            geom_bar(stat="identity")+
            ggtitle("Count of restaurents in locality in City")+
            theme(plot.title=element_text(margin=margin(t=40,b=-30)))
    }, res = 96)
    
    output$plot1<-renderPlot({
        p=data3[data3$Country==selected_country(),]%>%
            group_by(Cuisines)%>%
            summarise(popularity_of_cuisine=n(),.groups = 'drop') %>%
            arrange(desc(popularity_of_cuisine))%>%
            as.data.frame()%>%
            head(5)
        
        ggplot(p, aes(x=Cuisines, y=popularity_of_cuisine,fill=popularity_of_cuisine)) +
            geom_bar(stat="identity")+
            ggtitle("Top Cuisines in a resturent in a country")+
            theme(plot.title=element_text(margin=margin(t=40,b=-30)))
    }, res = 96)
    
    output$plot3<-renderPlot({
        p3=data3[data3$City==input$city_input,]%>%
            group_by(Rating.text)%>%
            summarise(Count_of_rating=n(),.groups = 'drop') %>%
            arrange(desc(Count_of_rating))%>%
            as.data.frame()
        
        ggplot(p3, aes(x=Rating.text, y=Count_of_rating,fill=Rating.text)) +
            geom_bar(stat="identity")+
            ggtitle("Count of rating types in City")+
            theme(plot.title=element_text(margin=margin(t=40,b=-30)))
    }, res = 96)
    
    output$plot4<-renderPlot({
        p3=data3[data3$City==input$city_input,]%>%
            group_by(Price.range)%>%
            summarise(Count_of_price_range=n(),.groups = 'drop') %>%
            arrange(desc(Count_of_price_range))%>%
            as.data.frame()
        
        ggplot(p3, aes(x=Price.range, y=Count_of_price_range,fill=Price.range)) +
            geom_bar(stat="identity")+
            ggtitle("Count of price ranges in City")+
            theme(plot.title=element_text(margin=margin(t=40,b=-30)))
    }, res = 96)
    
    output$map <- renderLeaflet({
        cd=data3[data3$City==input$City_list3,]
       # Create a Leaflet map
       leaflet() %>%
          setView(lng = mean(data3$Longitude), lat = mean(data3$Latitude), zoom = 8) %>%
         addTiles()%>%# Add a tile layer (you can customize the map source)
        addMarkers(
           lng = cd[cd$Cuisine==input$Cuisine_list3,"Longitude"],
         lat = cd[cd$Cuisine==input$Cuisine_list3,"Latitude"],
         popup = cd[cd$Cuisine==input$Cuisine_list3,"Restaurent.Name"]
    )
    })

    output$table3<-renderDataTable(
        DT::datatable(
            data3[data3$City==input$City_list3 & data3$Cuisine==input$Cuisine_list3 ,], options = list(
            scrollX = TRUE)
        )
    )
}
#Calling shiny app
shinyApp(ui, server)
