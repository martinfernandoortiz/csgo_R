llibrary(shiny)
library(highcharter)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(RColorBrewer)
library(shinyWidgets)
library(readr)
library(htmltools)
library(CSGo)
library(dplyr)


csgo_stats1 <- get_stats_friends(api_key = '###', user_id = '76561198018191299') #Dani
csgo_stats <- get_stats_friends(api_key = '###', user_id = '76561199036069511') #yo

df <- as.data.frame(csgo_stats$friends_stats)
df1 <- as.data.frame(csgo_stats1$friends_stats) %>% filter(player_name=='0rT1x')
df <- rbind(df,df1)
rm(df1)

df[is.na(df)] <- 0
df <- df %>% mutate(value=as.integer(value))

maps <- df %>%  filter(type=='maps') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
weapon <- df %>%  filter(type=='weapon info') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
stat <- df %>%  filter(type=='stat' & category=='stats') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
performance <- df %>%  filter(category=='performance') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)
last <- df %>%  filter(category=='last match') %>% select(c(1,2,7)) %>% pivot_wider(names_from = name, values_from = value)


stat <- stat[,-c(6,10:16)]
performance <- performance[,-c(28:37,41:46)]


listita <- unlist(performance$total_kills_headshot)
length(listita)
vector <- c()
odd <- function(x) x%%2 != 0 
for (i in 1:length(listita)){
  if (isTRUE(odd(i))){ 
  vector <- append(vector,as.integer(listita[i]))  

  }
}

performance <- performance %>% mutate(jedyot=vector)

performance <- performance %>% mutate(jedshots= as.integer(jedyot)/as.integer(total_kills))

stat <- stat %>% left_join(performance, by="player_name")

# EFECTIVIDAD
armas <- df %>% filter(type=='weapon info') %>%  group_by(name_match) %>% count()
tipos <- df %>% filter(type=='weapon info') %>%  group_by(category) %>% count()
tipos


eficiencia <- df %>%
  filter(
    name_match %in% armas$name_match
  ) %>%
  mutate(
    stat_type = case_when(
      str_detect(name, "shots") ~ "shots",
      str_detect(name, "hits") ~ "hits",
      str_detect(name, "kills") ~ "kills"
    )
  )%>% 
  pivot_wider(
    names_from = stat_type, 
    id_cols = c("name_match","player_name"), 
    values_from = value
  ) %>%
  mutate(
    kills_efficiency = kills/shots*100,
    hits_efficiency = hits/shots*100,
    hits_to_kill = kills/hits*100
  )

eficiencia <- eficiencia %>% select(1,2,3:8) %>%  pivot_longer(c(kills_efficiency,hits_efficiency, hits_to_kill), names_to = "estadisticas", values_to = "valores")



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
            infoBoxOutput("defius", width = 3)
 
        ),
        
        fluidRow(
            
            infoBoxOutput("kills", width = 3),
            infoBoxOutput("muertes", width = 3),
            infoBoxOutput("mvp", width = 3),
            infoBoxOutput("facas", width = 3)
            
        ),
        
        fluidRow(
        box(
            title = "Efectividad según arma", status = "primary", solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            height = 8,
            highchartOutput("plot1", height = 600)
        )
        
        )
        ),
        
        #################### TAB WIDGETS
        
    tabItem(tabName = "widgets",
            fluidRow(
                box(
                        title = "Tiempo Jugado", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE, width = 4,
                        highchartOutput("plot2")
                    ),
                box(
                  title = "Ganados/Jugados", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 4,
                  highchartOutput("plot3")
                ),
                box(
                  title = "Kills/Shots", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 4,
                  highchartOutput("plot4")
                )
                
            ),
            fluidRow(
              box(
                title = "Headshot/Kills", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 4,
                highchartOutput("plot5")
              ),
              box(
                title = "kills/Rounds", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 4,
                highchartOutput("plot6")
              ),
              box(
                title = "MVP/Matches", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 4,
                highchartOutput("plot7")
              )
              ),
              
              fluidRow(
                box(
                  title = "Deads por match", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  highchartOutput("plot8")
                ),

                box(
                  title = "Puntos contribuidos por match", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 6,
                  highchartOutput("plot9")
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
            menuItem("Estadísticas Individuales", tabName = "dashboard", icon = icon("dashboard"))
                    )
    })
    output$menu2 <- renderMenu({
        sidebarMenu(
            menuItem("Comparativo", icon = icon("th"), tabName = "widgets")
        )
    })
    
########## BODY
    ###### TAB 1
        #PARTIDOS
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
    #HEADSHOTS
    output$minutos <- renderInfoBox({
      b <- df %>%
        filter(player_name == input$nombresset)
      b <- b %>% filter(name=='total_kills_headshot')  
      b<- sum(b$value)
        infoBox(
            "Headshots", b, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    #GANADOS
    output$ganados <- renderInfoBox({
      a <- df %>%
        filter(player_name == input$nombresset)
      a <- a %>% filter(name=='total_wins')  
      a<- sum(a$value)
        infoBox(
            "Ganados", a, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    #DEFUSE
    output$defius <- renderInfoBox({
      a <- df %>%
        filter(player_name == input$nombresset)
      a <- a %>% filter(name=='total_defused_bombs')  
      a<- sum(a$value)
        infoBox(
            "Defuseadas", a, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })

    
    
    #KILLS
    output$kills <- renderInfoBox({
      a <- df %>%
        filter(player_name == input$nombresset)
      a <- a %>% filter(name=='total_kills')  
      a<- sum(a$value)
        infoBox(
            "Kills", a, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })

    #MUERTES    
    output$muertes <- renderInfoBox({
      a <- df %>%
        filter(player_name == input$nombresset)
      a <- a %>% filter(name=='total_deaths')  
      a<- sum(a$value)
        infoBox(
            "Muertes", a, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
    #MVP
    output$mvp <- renderInfoBox({
      a <- df %>%
        filter(player_name == input$nombresset)
      a <- a %>% filter(name=='total_mvps')  
      a<- sum(a$value)
        infoBox(
            "MVP", a, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
    
      #FACAS 
        output$facas <- renderInfoBox({
          a <- df %>%
            filter(player_name == input$nombresset)
          a <- a %>% filter(name=='total_kills_knife')  
          a<- sum(a$value)
        infoBox(
            "Kills con Faca", a, icon = icon("list"),
            color = "purple", fill = TRUE
        )
    })
        #EFECTIVIDAD POR ARMA

    output$plot1 <- renderHighchart({
      eficiencia %>% filter(player_name==input$nombresset) %>% hchart( "line", hcaes(name_match, valores, group = estadisticas)) %>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= NA)
      
    })
    
    # Tiempo Jugado
    output$plot2 <- renderHighchart({
      
        stat %>% arrange(-total_time_played) %>%  hchart("column", hcaes(x = player_name, y = total_time_played/3600)) %>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= "NA")
    })
    
    
    #Ganados 
    output$plot3 <- renderHighchart({
      
      stat %>% mutate(efectividadPartidos= as.integer(total_matches_won)/as.integer(total_matches_played))%>% 
        arrange(-efectividadPartidos)%>%
        hchart("column", hcaes(x = player_name, y = round(efectividadPartidos,2)))%>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= "Won/Total matches") 

    })
    
    #Kills/shots
    output$plot4 <- renderHighchart({
      
      performance %>% mutate(efectividadShots= as.integer(total_kills)/as.integer(total_shots_fired)) %>% 
        arrange(-efectividadShots) %>% 
            hchart("column", hcaes(x = player_name, y = round(efectividadShots,2)))%>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= list(text= "Total Kills / Total Shots Fired"))
    })
    

    
    #headshot
    output$plot5 <- renderHighchart({
      performance %>% arrange(-jedshots) %>%  hchart( "column", hcaes(x = player_name, y = round(jedshots,2)))%>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= list(text= "Headshots / Kills"))
      
    }) 
    
    #Kills/Rounds
    output$plot6 <- renderHighchart({
      
      stat %>% 
        mutate(efectividadShots= as.integer(total_kills)/as.integer(total_rounds_played)) %>% 
        arrange(-efectividadShots) %>% 
        hchart("column", hcaes(x = player_name, y = round(efectividadShots,2)))%>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= list(text= "Efectividad"))
    })
  
    #Kmvp/Match
    output$plot7 <- renderHighchart({
     stat %>% 
        mutate(mvp= as.integer(total_mvps)/as.integer(total_matches_played)) %>% arrange(-mvp) %>% 
       hchart( "column", hcaes(x = player_name, y = round(mvp,2)))%>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= list(text= "MVP"))
      
    })

    #DEADS/MATCHS
    output$plot8 <- renderHighchart({
      stat %>%  mutate(dead=as.integer(total_deaths)/as.integer(total_matches_played)) %>% arrange(dead) %>% 
              hchart("column", hcaes(x = player_name, y = round(dead,2)))%>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= NA)
          })
    

    #SCORE
    output$plot9 <- renderHighchart({
        stat %>% mutate(score= as.integer(total_contribution_score)/as.integer(total_matches_played)) %>% arrange(-score) %>% 
        hchart("column", hcaes(x = player_name, y = round(score,2)))%>% 
        hc_xAxis(title = NULL) %>% hc_yAxis(title= list(text= "Contribution Score / Matches played"))
      
    })
}

shinyApp(ui, server)




