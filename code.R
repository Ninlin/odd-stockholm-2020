#library(tidyverse)
library(httr) 
library(jsonlite)
library(tidyr)
library(dplyr)
library(readr)
library(magrittr)

# MATSVINN --------

# save the path 
path <- 'https://catalog.sodertalje.se/rowstore/dataset/bd34bf56-8ebe-427a-8e8d-0a126f481f33'

## getting only part of the data
## looked into the documentation to find that it gets the data in 100 batches and the offset can be specified. 
## how many offsets do we need?
## convert the query into a  for-loop 

# write the API request
#httr
request <- GET(path)

#check the status
request$status_code

# load content
content <- content(request, as = 'text', encoding = 'UTF-8')
#pring cintent into concole
# str(content)


df <- fromJSON(content, flatten = TRUE) %>%
        data.frame()
# the dataset contains only 100 rows 
# pipe #e pipe operator %>%, which helps to chain several functions without assigning the result to a new object.

request <- GET(path,
               query = list('_offset' = 100))

# Get it all in a for-loop 
# Check how many rows

for (i in 1:5) {
        print(i)
}


df_total <- data.frame()

for (i in 0:2) {
        
        request <- GET(path,
                       query = list('_offset' = 100*i))
        # load content
        content <- content(request, as = 'text', encoding = 'UTF-8')
        
        df <- fromJSON(content, flatten = TRUE) %>%
                data.frame()
        
        df_total <- bind_rows(df_total, df) 
        # bind_rows binds columns with the same names, no error if the number is different 
}



library(stringr) # operations with strings

# two helper functions for data cleaning 
comma_to_dot <- function(var) {
        result <- as.numeric(str_replace(var, ",", "."))
}

zero_to_null <- function(var) {
        result <- ifelse(var == 0, NA, var)
}


df_total_clean <- df_total %>%
        # remove unneccessary columns
        select(-c(next., resultCount, offset, limit)) %>%
        # change column names with magrittr::set_columnnames
        set_colnames(str_replace(colnames(df_total)[5:ncol(df_total)], 'results.', '')) %>%
        # clean the numeric variables: replace commas with dots and zeroes with nulls 
        mutate_at(c("tallrikssvinn.gäst.dag","kökssvinn.gäst.dag", "serveringssvinn.gäst.dag"), comma_to_dot) %>%
        mutate_at(c("tallrikssvinn.gäst.dag","kökssvinn.gäst.dag", "serveringssvinn.gäst.dag"), zero_to_null) %>%
        # recode months
        mutate(månad = str_trim(månad)) %>%
        mutate(month = case_when(
                månad == 'Januari' ~ 1,
                månad == 'Februari' ~ 2,
                månad == 'Mars' ~ 3,
                månad == 'April' ~ 4,
                månad == 'Maj' ~ 5,
                månad == 'September' ~ 9,
                månad == 'Oktober' ~ 10,
                månad == 'November' ~ 11,
                månad == 'December' ~ 12
        )) %>%
        # create a date column
        mutate(date = ISOdate(år, month, 1)) %>%
        # trim 
        mutate(enhet = str_trim(enhet)) %>%
         select(-månad, -år, -month) %>%
        mutate(code = str_sub(enhet, end = 4L))
        
write_csv(df_total_clean, 'foodwaste.csv', na = 'NULL')

# OSM ---------
library(osmdata)

query <- getbb(place_name = "Södertälje") %>%
        opq() %>% #builds an over_pass query
        add_osm_feature(key = "amenity", value = "school")

schools <- osmdata_sf(q = query)

points <- schools$osm_polygons %>%
        filter(is.na(name) == F) %>%
        data.frame()
#https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/

# SCRAPING -----
# Parsing of HTML/XML files  
library(rvest)  
library(xml2)

page <- 'https://www.hitta.se/grundskolor+s%C3%B6dert%C3%A4lje/f%C3%B6retag/2'
#To convert a website into an XML object, you use the read_html() function.
page_html <- read_html(page) 

# web tool
#To extract the relevant nodes from the XML object you use html_nodes()
school_address <- page_html %>% 
        html_nodes('body') %>% 
        xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "address", " " ))]') %>% 
        html_text()

school_name <- skolor %>% 
        rvest::html_nodes('body') %>% 
        xml2::xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "header", " " ))]//h2') %>% 
        rvest::html_text()

#bind 

# WEB ---

# flatten does not work
gru_json <-fromJSON('https://catalog.sodertalje.se/store/1/resource/39')
#gym_json <- fromJSON('https://catalog.sodertalje.se/store/1/resource/36')

gru_df <- data.frame(gru_json$features$properties)

longitude <- data.frame(gru_json$features$geometry$coordinates)[1,] %>%
        pivot_longer(cols = starts_with("c"),
                     names_to = 'coords', values_to = 'lon' ) %>%
        select(-1)
latitude <- data.frame(gru_json$features$geometry$coordinates)[2,] %>%
        pivot_longer(cols = starts_with("c"),
                     names_to = 'coords', values_to = 'lat' ) %>%
        select(-1)

school_list_df <- bind_cols(gru_df, longitude, latitude) %>%
        mutate(code = if_else(str_detect(Namn, 'Majtorp'), 'Majt', 
                         if_else(str_detect(Namn, 'Ene'), 'Ene',
                              str_sub(Namn, end = 4L))))

        
foodwaste_address <- df_total_clean %>%
        left_join(school_list_df) 

write_csv(foodwaste_address, 'foodwaste_address.csv', na = 'Null')



# CLEANING DATA, DEALING WITH MISSING
# show data gaps - number of rows
# look at different measures - why some are interpreted as text? 
# can we simply convert it to a number? 
# getting all the nulls - because of commas and dots 

# What are all the measurements actually saying?
# Months in Swedish

# Would make more sense too look at the school year
# analyze total trent for all schools







