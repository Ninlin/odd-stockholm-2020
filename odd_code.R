#### Install & load packages ----
require('dplyr')
require('stringr')
require('httr')
require('jsonlite')
require('osmdata')
require('rvest')
require('xml2')

library(dplyr) # data manipulation
library(stringr) # operations with strings

#### Getting data from an API -----
library(httr) 
library(jsonlite)

# 1. Build a request 
path <- 'https://catalog.sodertalje.se/rowstore/dataset/bd34bf56-8ebe-427a-8e8d-0a126f481f33'
request <- GET(path)
request # check the information about the request

# 2. Extract content from a request
content <- content(request, as = 'text', encoding = 'UTF-8')
content # look at the result

# 3. Convert JSON to data frame
df <- fromJSON(content) %>%
        data.frame()

# Getting the second hundred rows 
request <- GET(path,
               query = list('_offset' = 100))

# For-loop example
for (i in 1:5) {
        print(i)
}

# For-loop to get all 247 rows of data
df_total <- data.frame()

for (i in 0:2) {
        # 1. Build a request 
        path <- 'https://catalog.sodertalje.se/rowstore/dataset/bd34bf56-8ebe-427a-8e8d-0a126f481f33'
        request <- GET(path,
                       query = list('_offset' = i*100))
        
        # 2. Extract content from a request
        content <- content(request, as = 'text', encoding = 'UTF-8')
        
        # 3. Convert JSON to data frame
        df <- fromJSON(content) %>%
                data.frame()
        
        df_total <- bind_rows(df_total, df)
}


#### Data Cleaning --------

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


#### Getting data from OSM ---------
library(osmdata)

query <- getbb(place_name = "Södertälje") %>%
        opq() %>% #builds an over_pass query
        add_osm_feature(key = "amenity", value = "school")

schools <- osmdata_sf(q = query)

points <- schools$osm_polygons %>%
        filter(is.na(name) == F) %>%
        data.frame()

#### Web Scraping -----
library(rvest)  
library(xml2)

page <- 'https://www.hitta.se/grundskolor+s%C3%B6dert%C3%A4lje/f%C3%B6retag/2'

school_address <- read_html(page) %>% 
        xml_find_all('//*[contains(concat( " ", @class, " " ), 
                     concat( " ", "address", " " ))]') %>% 
        html_text() %>%
        data.frame()

school_name <- read_html(page) %>% 
        xml_find_all('//*[contains(concat( " ", @class, " " ), 
                           concat( " ", "header", " " ))]//h2') %>% 
        html_text() %>%
        data.frame()

bind_cols(school_name, school_address)

#### Reading list of schools from JSON ----

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
