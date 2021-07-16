#library(shinydashboard)

#data
total_matches_played <- df %>% filter(name=='total_matches_played') 
total_matches_played <- sum(total_matches_played$value)

ui <- dashboardPage(
    dashboardHeader(title = "Info boxes"),
    dashboardSidebar(),
    dashboardBody(
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
            plotOutput("plot3", height = 250)
        ),
        
        box(
            title = "Inputs", status = "warning", solidHeader = TRUE,
            "Box content here", br(), "More box content",
            sliderInput("slider", "Slider input:", 1, 100, 50),
            textInput("text", "Text input:")
        )
        
        )
    )
)

server <- function(input, output) {
    

    # Same as above, but with fill=TRUE
    output$partidos <- renderInfoBox({
        infoBox(
            "Matchs jugados", total_matches_played, icon = icon("list"),
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
}

shinyApp(ui, server)




