# extremely basic shiny app by Luke Barker 2022.
# a similar PHP version of this exists elsewhere: 
# https://www.stockq.org/life/wordle-answers.php !
library(shiny)

library(rsconnect)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(DT)
library(stringr)
library(tidytext)


library(shinyWidgets)
library(shinybusy)
library(httr)
library(jsonlite)


# Define server logic required to get our data
server <- function(input, output, session) {
  
  today <- today(tzone="UTC")
  offset <- 8 # word list seems to be out by ?? as of Apr 28th 2022!
  
  # use a word list saved on google spreadsheet:
  gs4_deauth()
  
  # get public 'previous words' spreadsheet
  # a repurposed list taken from a medium blog, 
  # credit - https://medium.com/@owenyin/
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1vWiEdagCYtBq-sOrQ6UfWglQwRFkdAIxiV_Hhl_TDE8/edit?usp=sharing")
  
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
  
  # add this column too, with the official word number: 
  answer_nos <- rows - as.numeric(rownames(previous_w))
  
  # reindex our data table for nice displaying
  previous_w <- previous_w %>%
    mutate(date=format(date_index, format="%a %d %B")) %>%
    mutate(wordle=as.character(answer_nos))
    
  # set length of slider to no. of words there have been up until today
  max_slider <- rows 
  
  output$slider <- renderUI(
    noUiSliderInput(
      inputId = "range",
      color="#f6de62",
      label="Select number of previous days",
      min=0, 
      max=max_slider,
      step=1,
      format=wNumbFormat(decimals=0),
      width = "50%", 
      height = "300px",
      value = c(7),
      orientation = "vertical"
    )
  )
    
  
  # lookup words to get JSON results from free dictionary api
  rest_url <- "https://api.dictionaryapi.dev/api/v2/entries/en/"
  search <- NULL
  observeEvent(input$tbl_rows_selected, {
    
    search <- previous_w[input$tbl_rows_selected, ]$word
    url <- paste0(rest_url, search)
    
    # GET request of API
    req <- httr::GET(url = url)
    req_parsed <- httr::content(req, type="application/json", 
                                as="text", encoding="UTF-8")
    jdata <- fromJSON(req_parsed)
    tbl_flat <- as_data_frame(flatten(jdata))
    result <- unlist(tbl_flat$phonetics)
    
    ## printout the meanings 
    # wrangle the nested JSON into something usable (flattened dataframe)
    dfm <- tbl_flat[["meanings"]][[1]][["definitions"]][[1]]
    
    # found on stackoverflow:
    vectorBulletList <- function(vector) {
      if(length(vector > 1)) {
        paste0("<ul><li>", 
               paste0( paste0(vector), collapse = "</li><li>"),
               "</li></ul>")   
      }
    }
    output$meanings <- renderUI(HTML(vectorBulletList(dfm$definition)))
    
    
    # we  update the HTML <audio> URL / src
    new_src = NULL 
    ## tidyr parse the JSON
    df <- data.frame(keys=names(result), vals=result, row.names=NULL) %>%
      distinct() %>% # used in case there are duplicates in rows
      pivot_wider(names_from=keys, values_from=vals, values_fn=list) 
     
   
    if("audio" %in% names(df)) {
      new_src = unlist( df$audio[1] )
    } else if ("audio1" %in% names(df)) {
      new_src = unlist(df$audio1[1])
    } else {
      new_src = ""
    }
    
    output$out <- renderText(new_src) 
  

    # options described at https://notiflix.github.io/notify
    # notify_success(result['word'], 
    #             config_notify( width="460px", 
    #                           fontSize="16px",
    #                           background="#325f7e",
    #                           notiflixIconColor="#ffcccc",
    #                           showOnlyTheLastOne=TRUE,
    #                           fontFamily="Helvetica", 
    #                           opacity=0.9,
    #                           closeButton=FALSE),
    #               position="center-top"
    #             )
   
  })
  
  # display the no. of days the slider has selected to show
  output$value_range <- renderText({ 
    paste("showing past ", input$range, " day(s) solutions ")
  })
    
  
  ## hack the displayed data table to have no headings
  headerCallback <- JS(
    "function(thead, data, start, end, display){",
    "  $(thead).remove();",
    "}")
  
  output$tbl <- renderDT(
    ## a table widget is returned >
    ## used to manipulate ahead of rendering 
    DT::datatable(
      slice(previous_w, 1:input$range), 
      list(dom = "ft", 
           paging=FALSE, 
           searchHighlight=TRUE, 
           class="compact stripe",
           headerCallback = headerCallback
           ), 
      rownames=FALSE, 
      selection="single"
    )
    
  )
  
  # output some summary stats 
  x <- previous_w$word
  letters <- vector("character")
  for (i in 1:5) {
    exploded <- unlist(lapply(x, substring, i, i))
    letters <- append(letters, exploded )
  }
  
  freq <- as.data.frame(letters)
  freq$letters <- as.factor(freq$letters)
  theme_set(theme_minimal(base_size = 12))
  output$frequencies <- renderPlot(
    
    
    ggplot(freq, aes(x=letters) ) +
      geom_histogram(stat="count", alpha=0.8, fill="#ffa66b" )+
      labs(title=paste("", nrow(freq)," Letters So Far")) +
      labs(x="", y="") +
      
      theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_blank(), 
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            axis.text=element_text(face="bold", size=13),
            plot.title = element_text(face="bold", size=14))
  )
  
  # TODO: plot the word-types from the lexicon
  # https://stackoverflow.com/questions/55463594/how-to-extract-adjectives-and-adverbs-from-vector-of-text-in-r
  
  # https://stackoverflow.com/questions/1497539/fitting-a-density-curve-to-a-histogram-in-r
}



# Define UI for application 
ui <- fluidPage(
  
  # loading animation
  add_busy_bar(color = "#ff6b08", height="10px", centered=TRUE),
  
  tags$head(tags$style(
    'body {
        font-family: "Helvetica Neue", sans-serif !important; 
        font-weight: bold;
      }
      .noUi-connects{
        background-color:#9cccd6;
      }
      tr td {cursor:pointer }
      #out {visibility:hidden;}
    .shiny-output-error, .shiny-output-error:before { 
        visibility: hidden; 
    }'
    
  )),
  
  
  
  # Application intro
  intro_panel <- tabPanel(
    "About",
    
    titlePanel("Pastle"),
    
    p("Browse the past solutions. Updates daily. ", 
      a(href="https://www.nytimes.com/games/wordle/index.html", 
        "Play today.", title="play todays puzzle") )
  ),
  
  
  
  # Sidebar with a slider input for number of words to show 
  sidebarLayout(
    
    position="left",
    sidebarPanel(
      uiOutput("slider")
     
    ),
    
    # Show previous words list (i.e. previous answers)
    mainPanel(
      
      textOutput("out"),
      tags$audio(src="", type="audio/mp3", autoplay=F, controls=F, id="a"), 
      tags$script(" 
                  $(document).ready(function(){
                    $('#a').attr('data-src', 'https://api.dictionaryapi.dev/api/v2/entries/en/PASTEL'); 
                    $('#out').on('DOMSubtreeModified', 
                      
                      function(){
                        
      
                        if(!$(this).text()) {
                        
                          $('#a').attr('src', $('#a').attr('data-src') );
                        } else {
                          $('#a').attr('src', $(this).text() );
                        }
                      } )})"),
      
      uiOutput("meanings"),
      
      plotOutput("frequencies"),
      
      textOutput("value_range"),
      
      DTOutput("tbl") 
      
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
