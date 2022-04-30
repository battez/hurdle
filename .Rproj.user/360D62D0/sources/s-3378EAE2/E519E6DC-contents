# extremely basic shiny app by Luke Barker 2022.
library(shiny)
library(shinyWidgets)
library(rsconnect)


# Define UI for application 
ui <- fluidPage(
    tags$head(tags$style('
     body {
        font-family: "Helvetica Neue"; 
        font-style: bold;}'
    )),
  
    # Application intro
    intro_panel <- tabPanel(
      "About",
      
      titlePanel("Wordle words so far..."),
      

      p("This is just a best attempt at listing out wordle answers from days before today, to be spoiler-free."),
      p(a(href = "https://www.nytimes.com/games/wordle/index.html", "Play Wordle at NY Times today"))
    ),
    
    
    ## TODO: swap out the below boilerplate app with output of Words in 
    ## a useable format
    
    # TODO: sidebar
    # dropdown choosing how far back to show
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("range", 
                      label = "How far back to go?",
                      min = 0, max = 100, value = c(1))
          # noUiSliderInput(
          #   inputId = "noui1",
          #   min = 0, max = 50,
          #   value = 0
          # ),
          
        ),

        # Show word (i.e. wordle previous answer)
        mainPanel(
          textOutput("value_range")
        )
    )
)

# Define server logic required to get our data
server <- function(input, output, session) {

  library(googlesheets4)
  library(tidyverse)
  library(lubridate)
  
  today <- today(tzone="UTC")
  offset = 6 # wordle list seems to be out by 6 as of Apr 28th 2022!
  
  # use a word list saved on google spreadsheet:
  gs4_deauth()
  
  # get public 'previous wordles' spreadsheet
  # a repurposed list taken from a medium blog , credit - Owen Yin
  # https://medium.com/@owenyin/here-lies-wordle-2021-2027-full-answer-list-52017ee99e86
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1vWiEdagCYtBq-sOrQ6UfWglQwRFkdAIxiV_Hhl_TDE8/edit?usp=sharing")
  
  previous_w <- df %>%
    select(date, word) %>% 
    mutate(date=as_date(mdy(date), format = "%Y-%m-%d") - days(offset))  %>%
    filter(date < today) %>%
    arrange(desc(date))
  
  print(head(previous_w))
  
  
  #session$list <- slice(previous_w, input$range)
  output$value_range <- renderText({ 
    paste(input$range, " day(s) ago")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
