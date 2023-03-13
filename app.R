#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)

booklib <- readRDS("booklib.Rds")
lootlib <- readRDS("lootlib.Rds") %>%
  pivot_longer(Desk:Body, names_to = "container", values_to = "n_items")
reagents <- readRDS("reagents.Rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("D&D Generators"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("generator",
                        "What would you like to generate?",
                        c("Books", "Loot", "Reagents")),
            conditionalPanel(
              condition = "input.generator == 'Books'",
              inputPanel(
                numericInput("n_books", label = "Number of books:",
                             min = 1, max = 20, value = 3, step = 1),
                
                selectInput("topic_choice", label = "Topic(s)",
                            choices = c("Acrobatics", "Arcana", "Athletics", 
                                        "Bestiary", "Clothing", "Crafting", "General", 
                                        "History", "Leather", "Map", "Metal", "Nature", 
                                        "Religion", "Survival", "Wood"),
                            multiple = TRUE),
                
                selectInput("lang_choice", label = "Language(s)",
                            choices = c("Celestial", "Common", 
                                        "Draconic", "Dwarven", "Elven", 
                                        "Gnomish", "Halfling", "Infernal", "Undercommon"),
                            multiple = TRUE),
              )
            ),
            conditionalPanel(
              condition = "input.generator == 'Loot'",
              inputPanel(
                numericInput("loot_lvl", label = "What is your party's average character level?",
                             min = 1, max = 20, value = 1, step = 1),
                
                selectInput("loot_container", label = "Where is this loot found?",
                            choices = c("Desk", "Trunk", "Wardrobe", "Table", "Body"),
                            selected = "Trunk")
              )
            ),
            conditionalPanel(
              condition = "input.generator == 'Reagents'",
              inputPanel(
                numericInput("Nature", label = "What is the character's Nature skill modifier?",
                             min = -5, max = 5, value = 0, step = 1),
                
                numericInput("Survival", label = "What is the character's Survival skill modifier?",
                             min = -5, max = 5, value = 0, step = 1),
                
                numericInput("Hours", label = "How many hours are being spent searching?",
                             min = 1, max = 12, value = 1, step = 1),
                
                selectInput("location", label = "Location(s)",
                            choices = sort(c("Forest", "Mountain", "Grassland", 
                                           "River", "Coast", "Underdark", "Swamp", 
                                           "Desert", "Wastes")), 
                            selected = "Forest",
                            multiple = TRUE),
                
                selectInput("season", label = "Season(s)",
                            choices = c("Spri", "Summ", "Fall", "Wint"), 
                            selected = "Spri",
                            multiple = TRUE),
                
                selectInput("zone", label = "Zone(s)",
                            choices =  c("Temperate", "Polar", "Tropical", "Subtropical"), 
                            selected = "Temperate",
                            multiple = TRUE)
              )
            ),
           
        ),

        # 
        mainPanel(
          conditionalPanel(condition = "input.generator == 'Books'",
                           tableOutput("books_gen")
            
          ),
          conditionalPanel(condition = "input.generator == 'Loot'",
                           tableOutput("loot_gp"),
                           tableOutput("loot_items")
          ),
          conditionalPanel(condition = "input.generator == 'Reagents'",
                           tableOutput("reagents_gen")
          ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$books_gen <- renderTable({
        temp <- booklib %>%
          { if(!is.null(input$topic_choice)) filter(., Primary_Topic %in% input$topic_choice | Secondary_Topic %in% input$topic_choice) else . } %>%
          { if(!is.null(input$lang_choice)) filter(., Language %in% input$lang_choice) else .} %>%
          mutate(samp_prob = Num_copies/sum(Num_copies))
        
        
        if(nrow(temp) < input$n_books){
          data.frame(error = "There aren't enough books in the library to fulfill your selections!")
        } else {
          sample_idx <- sample.int(nrow(temp), input$n_books, prob = temp$samp_prob)
          
          temp[sample_idx, c("Title", "Author", "F_NF", "Primary_Topic", 
                             "Secondary_Topic", "Age", "Num_copies", "Region", 
                             "Language", "Length")]
        }

    })
    
    output$loot_gp <- renderTable(data_frame("Gold Pieces Found:" = 
                                               as.integer(round(rnorm(1, mean=(input$loot_lvl*3), 
                                                                      sd=input$loot_lvl), 0)
                                                          )
                                             )
                                  )
    
    output$loot_items <- renderTable({
      num_items <- sample.int(4, 1)
    
      #creating location-specific data, weighting, & selecting
      temp <- lootlib %>%
        filter(container == input$loot_container) %>%
        mutate(samp_prob = n_items/sum(n_items))
      
      sample_idx <- sample.int(nrow(temp), num_items, prob = temp$samp_prob)
      
      temp[sample_idx, c("Item", "Value")]
    })
    
    output$reagents_gen <- renderTable({
        baselineskill <- 6
        skill_mod <- (input$Nature+input$Survival)-baselineskill
        
        #math to determine collection
        skill_mod <- 1+(.05*skill_mod)
        
        #filtering by location, season, and zone
        temp <- reagents %>%
          filter(str_detect(Location, paste0(input$location, collapse = "|"))) %>%
          filter(str_detect(Season, paste0(input$season, collapse = "|"))) %>%
          filter(str_detect(Zone, paste0(input$zone, collapse = "|")))
        
        if(nrow(temp) > 0){
          temp$Find.Pct <- temp$Find.Pct/100
          temp$Find.Pct <- temp$Find.Pct*skill_mod
          
          reagents_found <- data_frame()
          
          for(r in 1:nrow(temp)){
            if(runif(1, 0, 1) < temp[r, "Find.Pct"]){
              reagents_found <- rbind(reagents_found, temp[r,])
            }
          }
          
          
          if(nrow(reagents_found) > 0){
            sample_idx <- sample.int(nrow(reagents_found), input$Hours, 
                                     prob = reagents_found$Find.Pct)
            
            out <- reagents_found[sample_idx, ]
            out <- out %>%
              select(!c(Find.Pct, Check))
            
          } else {
            out <- data_frame(error = "No reagents were found!")
          }
        } else {
          out <- data_frame(error = "No reagents were found!")
        }
        
        out
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
