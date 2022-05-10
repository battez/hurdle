# extremely basic shiny app by Luke Barker 2022.
# a similar PHP version of this exists elsewhere: 
# https://www.stockq.org/life/wordle-answers.php !
library(shiny)
library(shinyWidgets)
library(rsconnect)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(DT)


# Define server logic required to get our data
server <- function(input, output, session) {
  
  today <- today(tzone="UTC")
  offset <- 8 # wordle list seems to be out by ?? as of Apr 28th 2022!
  
  # use a word list saved on google spreadsheet:
  gs4_deauth()
  
  # get public 'previous wordles' spreadsheet
  # a repurposed list taken from a medium blog, 
  # credit - https://medium.com/@owenyin/
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1vWiEdagCYtBq-sOrQ6UfWglQwRFkdAIxiV_Hhl_TDE8/edit?usp=sharing")
  

  
  # TODO: possibly remove * entries and reflow the date range of the indexes 
  # 
  # sort into date order then make these readable for the Shiny display
  previous_w <- df %>%
    select(date, word) %>% 
    # find yesterdays date and remove everything after it
    mutate(date=as_date(mdy(date), format="%Y-%m-%d") - days(offset) )  %>%
    filter(date < today) %>%
    # filter out the NY times altered words
    filter(nchar(word) > 3) %>%
    arrange(desc(date)) %>%
    relocate(word)
    
  # reflow the dates index as there are gaps now:
  # make a new descending date index
  rows <- nrow(previous_w)
  date_index <- vector()
  for (i in 1:(rows)) {
    date_index <- append(date_index, today - days(i))
  }
  
  # add this column with the official wordle number: 
  wordle_nos <- rows - as.numeric(rownames(previous_w))
  
  ## reindex our data table for nice displaying
  previous_w <- previous_w %>%
    mutate(date=format(date_index, format="%a %d %B")) %>%
    mutate(wordle=as.character(wordle_nos))
    
  
  # set length of slider to no. of words there have been up until today
  max_slider <- rows 
  
  output$slider <- renderUI(
    noUiSliderInput(
      inputId = "range",
      color="#008000",
      label = "Select number of previous days",
      min=0, max=max_slider,
      step=1,
      format=wNumbFormat(decimals=0),
      width = "50%", height = "300px",
      value = c(7),
      orientation = "vertical"
    )
  )
    
  
  output$value_range <- renderText({ 
    paste("showing past ", input$range, " day(s) solutions")
  })
  
  ## hack the data table to have no headings
  output$show <- DT::renderDataTable(
    DT::datatable(
      slice(previous_w, 1:input$range), 
      list(dom = "ft", paging=FALSE, 
           searchHighlight=TRUE, 
           class="compact stripe",
           headerCallback = JS(
             "function(thead, data, start, end, display){",
             "  $(thead).remove();",
             "}")
           ), rownames=FALSE
    )
    
  )
  
  # some summary stats ?
  # library(stringr)
  # library(tidytext)
  # summary <- previous_w %>% 
  #   mutate(first=x) %>%
  
  
}


# Define UI for application 
ui <- fluidPage(
  
  
  tags$head(tags$style(
    'body {
        font-family: "Helvetica Neue", sans-serif !important; 
        font-weight: bold;
      }
      .noUi-connects{background-color:#ffc425;}'
    
  )),
  
  # Application intro
  intro_panel <- tabPanel(
    "About",
    
    titlePanel("Wordle words so far..."),
    
    p("Browse the previous wordles' solutions. Updates daily."),
    p(a(href = "https://www.nytimes.com/games/wordle/index.html", 
        "Play Wordle at NY Times today"))
  ),
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    position="left",
    sidebarPanel(
      uiOutput("slider"),
      
    ),
    
    # Show previous words (i.e. wordle previous answer)
    mainPanel(
      
      textOutput("value_range"),
      dataTableOutput("show")
    )
  )
)
# Run the application 
shinyApp(ui = ui, server = server)
