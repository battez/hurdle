# extremely basic shiny app by Luke Barker 2022.
library(shiny)
library(googlesheets4)
library(tidyverse)
library(lubridate)


today <- today(tzone="UTC")
print(today)
offset = 6 # list seems to be out by 6 as of Apr 28th 2022!

# word list saved on google spreadsheet:
gs4_deauth()


# get  public 'previous wordles' spreadsheet
# repurposed list taken from a medium blog , credit - Owen Yin
# https://medium.com/@owenyin/here-lies-wordle-2021-2027-full-answer-list-52017ee99e86
df <- read_sheet("https://docs.google.com/spreadsheets/d/1vWiEdagCYtBq-sOrQ6UfWglQwRFkdAIxiV_Hhl_TDE8/edit?usp=sharing")
  
previous_w <- df %>%
  select(date, word) %>% 
  mutate(date=as_date(mdy(date), format = "%Y-%m-%d") - days(offset))  %>%
  filter(date < today) %>%
  arrange(desc(date))
  

print(head(previous_w))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application intro
    intro_panel <- tabPanel(
      "About",
      
      titlePanel("Wordle words so far..."),
      

      p("This is just a best attempt at listing out wordle answers from days before today, to be spoiler-free."),
      p(a(href = "https://www.nytimes.com/games/wordle/index.html", "Play Wordle at NY Times today"))
    ),
    
    
    ## TODO: swap out the below boilerplate app with output of Words in 
    ## a useable format
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
