library(shiny)

library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(shinyWidgets)
library(readr)
library(htmltools)
library(data.table)
#data
total_matches_played <- df %>% filter(name=='total_matches_played') 
total_matches_played <- sum(total_matches_played$value)
nombres <- df %>% group_by(player_name) %>% count()

ui <- dashboardPage(
    dashboardHeader(title = "CSGO"),
    dashboardSidebar(
        sidebarMenuOutput("menu"),
        sidebarMenuOutput("menu2"),

        pickerInput(
            inputId = "nombresset",
            label = "Selecciona un jugador",
            choices = unique(nombres$player_name),
            selected = nombres[1,1],
            multiple = FALSE,
            options = list(
                `selected-text-format`= "count",
                `count-selected-text` = "{0} models choosed (on a total of {1})"
            )
        )

    ),
    
    dashboardBody(
        tabItems(
            #################### TAB DASHBOARD
        tabItem(tabName = "dashboard",
        fluidRow(
            
            infoBoxOutput("partidos", width = 3),
            infoBoxOutput("minutos", width = 3),
            infoBoxOutput("ganados", width = 3),
            infoBoxOutput("perdidos", width = 3)
 
        ),
        
        fluidRow(
            
            infoBoxOutput("kills", width = 3),
            infoBoxOutput("muertes", width = 3),
            infoBoxOutput("mvp", width = 3),
            infoBoxOutput("facas", width = 3)
            
        ),
        
        fluidRow(
        box(
            title = "Histogram", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            highchartOutput("plot1")
        ),
        
        box(
            title = "Inputs", status = "warning", solidHeader = TRUE,
            "Box content here", br(), "More box content",
            sliderInput("slider", "Slider input:", 1, 100, 50),
            textInput("text", "Text input:")
        )
        
        )
        ),
        
        #################### TAB WIDGETS
        
    tabItem(tabName = "widgets",
            fluidRow(
                box(
                        title = "Ganados/Jugados", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        highchartOutput("plot2")
                    )
            )
        )
        )
    )
)
################################################################################################################################
server <- function(input, output) {

   # df_reactive <- reactive({
    #    df %>%
    #        filter(player_name == input$nombresset) %>%
    #        arrange(nombresset) 
    #})
    
########## MENU
    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
                    )
    })
    output$menu2 <- renderMenu({
        sidebarMenu(
            menuItem("Widgets", icon = icon("th"), tabName = "widgets")
        )
    })
    
########## BODY
    ###### TAB 1
        
      output$partidos <- renderInfoBox({
          a <- df %>%
              filter(player_name == input$nombresset)
          a <- a %>% filter(name=='total_matches_played')  
          a<- sum(a$value)
        infoBox(
            "Matchs jugados", a, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    # Same as above, but with fill=TRUE
    output$minutos <- renderInfoBox({
        infoBox(
            "Progress", total_matches_played, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    # Same as above, but with fill=TRUE
    output$ganados <- renderInfoBox({
        infoBox(
            "Ganados", total_matches_played, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    # Same as above, but with fill=TRUE
    output$perdidos <- renderInfoBox({
        infoBox(
            "Perdidos", total_matches_played, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })

    
    
    
    # Same as above, but with fill=TRUE
    output$kills <- renderInfoBox({
        infoBox(
            "Kills", total_matches_played, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    # Same as above, but with fill=TRUE
    output$muertes <- renderInfoBox({
        infoBox(
            "Muertes", total_matches_played, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    # Same as above, but with fill=TRUE
    output$mvp <- renderInfoBox({
        infoBox(
            "MVP", total_matches_played, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    # Same as above, but with fill=TRUE
    output$facas <- renderInfoBox({
        infoBox(
            "Combates con Facas", total_matches_played, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    output$plot1 <- renderHighchart({
        hchart(stat, "column", hcaes(x = player_name, y = total_time_played))
    })
    
    output$plot2 <- renderHighchart({
        hchart(stat, "column", hcaes(x = player_name, y = total_time_played))
    })
}

shinyApp(ui, server)




