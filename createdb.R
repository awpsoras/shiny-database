# creating database file to play with
library(tidyverse)
library(dbplyr)
library(here)

# Run this to reset the example db!

refreshDB <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), here("testdb.sqlite"))
  
  # I think I want to add some dates here somehow
  data <- starwars %>% select(!c(films, vehicles, starships)) %>% 
    mutate(
      year = sample(1900:2000, nrow(.), replace = TRUE),
      month = sample(1:12, nrow(.), replace = TRUE),
      day = sample(1:28, nrow(.), replace = TRUE),
      birthday = paste(year, month, day, sep = "-"),
      birthdate = as_date(birthday)
    ) %>% 
    select(!c(year, month, day, birthday))
  
  RSQLite::dbListTables(con)
  copy_to(con, data, temporary = FALSE, overwrite = TRUE)
  
  # view what we have
  # Idk what happened to birthday lol
  # Maybe it's better to store as text? Idk
  # It definitely comes back though!
  tbl(con, "data") %>% 
    select(birthdate) %>% collect() %>% 
    mutate(birthdate = as_date(birthdate))
  
  DBI::dbDisconnect(con)
}

refreshDB()

seeTables <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), here("testdb.sqlite"))
  print(RSQLite::dbListTables(con))
  DBI::dbDisconnect(con)
}

seeTables()
