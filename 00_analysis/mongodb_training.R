# BUSINESS SCIENCE ----
# DS4B 202-R ----
# MONGO DB TRAINING -----
# Version 1

# LIBRARIES ----
library(mongolite) # Resource: https://jeroen.github.io/mongolite/
library(jsonlite)

library(config)

library(tidyverse)
library(lubridate)


# 1.0 CONNECTION TO REMOTE MONGODB ----

# Setup config Package & database YAML
Sys.setenv(R_CONFIG_ACTIVE = "default")

config <- config::get(file = "config.yml")

mongo_connect <- function(collection, database,
                          host     = config$host,
                          username = config$username,
                          password = config$password) {
  
    mongo(
      collection = collection,
      url        = str_glue("mongodb+srv://{username}:{password}@{host}/{database}")
    )
}

# Connect to MongoDB Atlas Cloud Database
mongo_connect(collection = "mtcars", database = "rstats")

# 2.0 ADD DATA ----

# Connect to collection
mongo_connection <- mongo_connect(collection = "mtcars", database = "rstats")

# Adding data
mtcars %>% 
    as_tibble(rownames = "model") %>%
    mongo_connection$insert() 

# 3.0 QUERYING DATA ----

mongo_connection$find()

mongo_connection$find(limit = 6) %>% 
  toJSON() %>% 
  prettify()

mongo_connection$find(query = '{"model": "Hornet Sportabout"}') %>% as_tibble()

# 4.0 MODIFYING A COLLECTION ----

new_car_tbl <- tibble(
    model = "Ford F150",
    mpg   = 9.8,
    cyl   = 8,
    disp  = 275.8,
    hp    = 180,
    drat  = 3.07,
    wt    = 7.85,
    qsec  = 23.45,
    vs    = 0,
    am    = 1,
    gear  = 4,
    carb  = 3
)

new_car_tbl

# 4.1 Insert New Record
mongo_connection$insert(new_car_tbl)

mongo_connection$count()

mongo_connection$find(query = '{"model": "Ford F150"}')

tibble(
    model = "Ford F150",
) %>% 
    toJSON() %>% 
    str_remove_all(pattern = "^\\[|\\]$") %>% 
    prettify() %>% 
    mongo_connection$find(query = .) %>% 
    as_tibble()

# 4.2 Change a Record

mongo_connection$update(
    query = '{"model": "Ford F150"}',
    update = '{"$set": {"mpg": 10.5}}'
) # Confirm change by running tibble() query above

mongo_connection$update(
    query = '{"model": "Ford F250"}',
    update = '{"$set": {"mpg": 10.8}}',
    upsert = TRUE
) 

mongo_connection$find(query = '{"model": "Ford F250"}')

mongo_connection$count()

mongo_connection$find() %>% as_tibble() %>% tail()

# 4.3 Remove a record
mongo_connection$remove('{"model": "Ford F250"}')

mongo_connection$find() %>% as_tibble() %>% tail()

# 4.4 Remove entire table (be careful)

mongo_connection$drop() # Deletes entire collection


# 4.5 Disconnecting from Database ----

mongo_connection$disconnect()


# 5.0 NESTED STRUCTURES ----

mongo_connection <- mongo_connect(
    database   = "stock_analyzer",
    collection = "user_base"
)

mongo_connection$drop()
mongo_connection$count()

user_base_tbl <- tibble(
    user           = c("user1", "user2"),
    password       = c("pass1", "pass2"), 
    permissions    = c("admin", "standard"),
    name           = c("User One", "User Two"),
    favorites      = list(c("AAPL", "GOOG", "NFLX"), c("MA", "V", "FB")),
    last_symbol    = c("GOOG", "NFLX"),
    user_settings  = list(tibble(mavg_short = 20, mavg_long = 50, time_window = 180), 
                          tibble(mavg_short = 30, mavg_long = 90, time_window = 365)),
    account_created = c(ymd_hms("2019-05-12 12:31:09"), ymd_hms("2019-06-04 06:18:02"))
) 

# Converting to JSON

user_base_tbl %>% 
    toJSON(POSIXt = "mongo") %>% 
    prettify()

# Adding nested structure to mongodb
mongo_connection$insert(user_base_tbl)


# Retrieve - Preserves nested structure and format
mongo_connection$find() %>% as_tibble()


# 6.0 STOCK ANALYZER APP - CRUD WORKFLOW -----

# Create new collection
mongo_connection <- mongo_connect(
    database   = "stock_analyzer",
    collection = "user_base_test"
)

mongo_connection$drop()
mongo_connection$count()

# 6.1 Add User Data ----
mongo_connection$insert(user_base_tbl)


# 6.2 Get User Data ----
# read_user_base <- function() {
#     user_base_tbl <<- read_rds(path = "00_data_local/user_base_tbl.rds")
# }

mongo_read_user_base <- function(database = "stock_analyzer", collection = "user_base_test") {
  
    mongo_connection <- mongo_connect(
        database   = database,
        collection = collection,
        host       = config$host, 
        username   = config$username, 
        password   = config$password
    )
    
    user_base_tbl <<- mongo_connection$find() %>% as_tibble()
    
    mongo_connection$disconnect()
  
}

rm(user_base_tbl)

mongo_read_user_base(database = "stock_analyzer", collection = "user_base")

# 6.3 What shinyauthr does... ----

user_1_tbl <- user_base_tbl %>% 
    filter(
        user     == "user1",
        password == "pass1"
    )

user_1_tbl

user_1_tbl %>% 
    pull(favorites)

pluck(user_1_tbl, "favorites", 1) <- c("AAPL", "GOOG", "NFLX", "ADBE")

pluck(user_1_tbl, "favorites", 1)

# 6.5 Update Mongo ----

user_name <- "user1"

# mongo_connection$find(query = query_string)

mongo_update_and_write_user_base <- function(user_name, column_name, assign_input, 
                                             database   = "stock_analyzer", 
                                             collection = "user_base_test") {
    
    user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
    
    mongo_connection <- mongo_connect(
      database   = database,
      collection = collection,
      host       = config$host, 
      username   = config$username, 
      password   = config$password
    )
    
    # Query String
    query_string <- str_c('{{"user": "', user_name, '"}}')
    
    # Update String
    user_base_tbl %>% 
        filter(user == user_name) %>% 
        select(-user, -oassword, -permissions) %>%
        toJSON(POSIXt = "mongo") %>% 
        str_remove_all(pattern = "^\\[|\\]$")
    
    # Update
    mongo_connection$update(
        query  = query_string,
        update = str_c('{"$set": ', update_string, '}')
      )
    
    mongo_connection$disconnect()
}



# Before update
mongo_connection$find()


# After update
mongo_update_and_write_user_base(
  user_name     = "user1", 
  column_name   = "user_settings", 
  assign_input  = list(tibble(
    mavg_short  = 15,
    mavg_long   = 75,
    time_window = 720
  ))
)

# 7.0 Save Functions ----

dump(c("mongo_connect", "mongo_read_user_base", "mongo_update_and_write_user_base"), 
     file = "00_scripts/crud_operations_mongodb.R", append = FALSE)


