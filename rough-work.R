library(tidyverse)
library(stringr)
library(tidytext)
library(httr)
library(jsonlite)
# json https://blog.exploratory.io/working-with-json-data-in-very-simple-way-ad7ebcc0bb89

# lookup words to get JSON results from free dictionary api
rest_url <- "https://api.dictionaryapi.dev/api/v2/entries/en/"

search <- "HINGE"
url <- paste0(rest_url, search)

req <- httr::GET(url = url)
req_parsed <- httr::content(req, type="application/json", as="text")
stop()
jdata <- fromJSON(req_parsed)
stop()

tbl_flat <- as_data_frame(flatten(jdata))

phonetics <- unlist(tbl_flat$phonetics)
#meanings <- unlist(tbl_flat$meanings)

dfm <- tbl_flat[["meanings"]][[1]][["definitions"]][[1]]

# wrangle the nested JSON into something usable (flattened dataframe)
# result <- map(req_parsed, fromJSON)
# result <- unlist(result)


