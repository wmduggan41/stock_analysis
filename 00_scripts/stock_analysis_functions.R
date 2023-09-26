get_gold_list <- function(api_key, from = NULL, to = NULL) {
  
  base_url <- "https://api.stlouisfed.org/fred/series/observations"
  
  # Create query list
  query_list <- list(series_id = "WPU10210501", 
                     api_key   = api_key, 
                     file_type = "json")
  
  # Conditionally add start and end dates to the query if they are not NULL
  if (!is.null(from)) query_list$start_date <- from
  if (!is.null(to)) query_list$end_date <- to
  
  response <- GET(url = base_url, query = query_list)
  
  # Check request
  if(http_status(response)$category != "Success") {
    stop("Failed to fetch data from FRED API")
  }
  
  # Parse the JSON response
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Extract observations
  observations <- data$observations
  
  # Replace '.' with NA
  observations$value[observations$value == "."] <- NA
  
  # Convert 'value' to numeric
  observations$value <- as.numeric(as.character(observations$value))
  
  # Implement LOCF to fill missing values
  observations$value <- na.locf(observations$value, na.rm = FALSE)
  
  # If the first value is still NA after LOCF, you might want to handle that too, 
  # either by removing or using some other method. For this example, let's remove.
  observations <- observations[!is.na(observations$value), ]
  
  return(observations)
}

get_stock_list <-
function(stock_index = "SP500") {
  
  stocks <- tq_index(stock_index) %>% 
    select(symbol, company) %>% 
    arrange(symbol) %>% 
    mutate(label = str_c(symbol, company, sep = ", ")) %>% 
    select(label)
  
  return(stocks)
}
get_symbol_from_user_input <-
function(user_input) {
  user_input %>% str_split(pattern = ", ") %>% pluck(1, 1)
  
}
get_stock_data <-
function(stock_symbol, 
                           from = today() - days(180), 
                           to   = today(), 
                           mavg_short = 20, mavg_long = 50) {

    stock_symbol %>% 
        tq_get(get = "stock.prices", from = from, to = to) %>%
        select(date, adjusted) %>%
        mutate(mavg_short = rollmean(adjusted, k = mavg_short, na.pad = TRUE, align = "right")) %>%
        mutate(mavg_long  = rollmean(adjusted, k = mavg_long, na.pad = TRUE, align = "right"))
    
}
plot_stock_data <-
function(data) {
    g <- data %>%
        gather(key = "legend", value = "value", adjusted:mavg_long, factor_key = TRUE) %>%
        
        ggplot(aes(date, value, color = legend, group = legend)) +
        geom_line(aes(linetype = legend)) +
        theme_tq() +
        scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
        scale_color_tq() +
        labs(y = "Adjusted Share Price", x = "")
    
    ggplotly(g)
}
plot_gold_data <- 
  function(data) {
    
    if (!is.Date(data$date)) {
      data$date <- as.Date(data$date)
    }
    
    gg <- ggplot(data, aes(x = date, y = value)) +
      geom_line(color = "gold") +
      theme_tq() + 
      scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
      scale_color_tq() +
      labs(y = "USD Price", x = "")
    
    return(gg)
}
generate_commentary <-
function(data, user_input) {
    warning_signal <- data %>%
        tail(1) %>%
        mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
        pull(mavg_warning_flag)
    
    n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
    n_long  <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
    
    if (warning_signal) {
        str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
    } else {
        str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
        
    }
}
