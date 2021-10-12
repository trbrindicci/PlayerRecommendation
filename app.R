#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(plotly)
library(bslib)

FinalResultPS2 <- read_csv("FinalResultPS.csv")
FinalResultPS1 <- read.csv("FinalResultPS1.csv")
similarity <- FinalResultPS1[c(1,7)]
head(similarity)
table <- FinalResultPS2[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,18,29,30)]

# Define UI for application that draws a histogram
ui <- navbarPage(theme = NULL,
                 title = "Who Can Replace Mbappe?",
                 sidebarPanel(
                     selectInput(inputId = "indv",
                                 label = "Player",
                                 choices = table$Player, 
                                 selected = 'Jesse Lingard'),
                     
                     selectInput(inputId = "indv2",
                                 label = "Player",
                                 choices = table$Player, 
                                 selected = 'Pablo Sarabia'),
                     
                     checkboxGroupInput(inputId = "comp",
                                        label = "Competition",
                                        choices= unique(table$Comp)),
                     dataTableOutput("similarity"),
                     dataTableOutput("similarity2")
                     
                     
                 ),
                 mainPanel(
                     tabsetPanel( 
                         tabPanel("Radar", splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("radar"), plotlyOutput("radar2"))),
                         tabPanel("Dataframe", dataTableOutput("player_summary"))
                     )
                 ))

   
server <- function(input, output, session) {
        
        output$player_summary <- renderDataTable({
            table %>% 
                filter(Comp %in% input$comp) %>%
                select(Player, Pos, Age, 
                       Goals, Ast, xG, xA, 'SoT/90' = SoT.90, GCA, GCADrib) %>% 
                rename_all(~str_to_title(.x))
        })
        
        output$radar <- renderPlotly({
            player <- filter(table, Player %in% input$indv)
            r <- map_dbl(player[, 12:18], ~.x)
            nms <- names(r)
            
            #code to plot the radar
            fig <- plot_ly(
                type = 'scatterpolar',
                r = r,
                theta = nms,
                fill = 'toself',
                mode = 'markers',
                name = input$indv
            )
            
            
            fig <- fig %>%
                layout(
                    polar = list(
                        radialaxis = list(
                            visible = T,
                            range = c(0,max(r))
                        )
                    ),
                    showlegend = T
                )
        })
        
        output$radar2 <- renderPlotly({
            player2 <- filter(table, Player %in% input$indv2)
            r2 <- map_dbl(player2[, 12:18], ~.x)
            nms2 <- names(r2)
            
            #code to plot the radar
            fig <- plot_ly(
                type = 'scatterpolar',
                r = r2,
                theta = nms2,
                fill = 'toself',
                mode = 'markers',
                name = input$indv2
            )
            
            
            fig <- fig %>%
                layout(
                    polar = list(
                        radialaxis = list(
                            visible = T,
                            range = c(0,max(r2))
                        )
                    ),
                    showlegend = T
                )
        })
        
        
        output$similarity <- renderDataTable({
            similarity %>% 
                filter(Player %in% input$indv) %>%
                select(Player, SimilarityScore) %>% 
                rename_all(~str_to_title(.x))
        })
        output$similarity2 <- renderDataTable({
            similarity %>% 
                filter(Player %in% input$indv2) %>%
                select(Player, SimilarityScore) %>% 
                rename_all(~str_to_title(.x))
        })
        
}
    

# Run the application 
shinyApp(ui = ui, server = server)
