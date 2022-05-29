library(tidyverse)
library(stringr)
library(tidytext)
library(httr)
library(jsonlite)
# json https://blog.exploratory.io/working-with-json-data-in-very-simple-way-ad7ebcc0bb89

# lookup words to get JSON results from free dictionary api
rest_url <- "https://api.dictionaryapi.dev/api/v2/entries/en/"

search <- "PASTEL"
url <- paste0(rest_url, search)

req <- httr::GET(url = url)
req_parsed <- httr::content(req, type="application/json", as="text")
jdata <- fromJSON(req_parsed)


tbl_flat <- as_data_frame(flatten(jdata))
phonetics <- unlist(tbl_flat$phonetics)

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
vectorBulletList(dfm$definition)
# result <- map(req_parsed, fromJSON)
# result <- unlist(result)


