# DS4B 102-R: PREDICTIVE WEB APPLICATIONS FOR BUSINESS ----
# DEMAND FORECAST ANALYSIS ----


# 1.0 LIBRARIES -----


# Core
library(tidyverse)
library(tidyquant)

# Interactive Visualizations
library(plotly)

# Modeling Libraries
library(parsnip)
library(timetk)

# Database
library(odbc)
library(RSQLite)




# 2.0 PROCESSED DATA ----
con <- dbConnect(RSQLite::SQLite(), "00_data/bikes_database.db")

bikes_tbl <- tbl(con, "bikes")
bikeshops_tbl <- tbl(con, "bikeshops")
orderlines_tbl <- tbl(con, "orderlines")

processed_data_tbl <- orderlines_tbl %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id")) %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    mutate(extended_price = quantity * price) %>%
    # Once computations are performed using the database backend, we collect() the data to pull it into R's memory
    collect()

processed_data_tbl <- processed_data_tbl %>%    
    mutate(order.date = ymd(order.date)) %>%
    separate(location, into = c("city", "state"), sep = ", ") %>%
    
    separate(description, 
             into = c("category_1", "category_2", "frame_material"),
             sep = " - ") %>%
    
    select(order.date, order.id, order.line, state, quantity, price,
           extended_price, category_1:frame_material, bikeshop.name)

dbDisconnect(con)




# 3.0 TIME SERIES AGGREGATION ----


# 3.1 DATA MANIPULATION ----
time_unit <- "quarter"

time_plot_tbl <- processed_data_tbl %>%
    
    mutate(date = floor_date(order.date, unit = time_unit)) %>%
    
    group_by(date) %>%
    summarize(total_sales = sum(extended_price)) %>%
    ungroup() %>%
    
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))

time_plot_tbl


# 3.2 Aggregate Time Series Function ----


# Here we will create a function called aggregate_time_series() 
# Our inputs will be data and time_unit
# The default time unit will be month, but you can change that
# The user will specify what data they want to input. This allows for flexibility instead of having it hard coded. 
# Within our function we are simply copy and pasting the data manipulation in 3.1
aggregate_time_series <- function(data, time_unit = "month") {
    
    
    output_tbl <- data %>%
        
        mutate(date = floor_date(order.date, unit = time_unit)) %>%
        
        group_by(date) %>%
        summarize(total_sales = sum(extended_price)) %>%
        ungroup() %>%
        
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))
    
    return(output_tbl)
    
}


# Here we are simply testing our data
# We get a 60x3 tibble
# date, total_sales, and label_text as the columns
# Remember the default time unit is "month"
processed_data_tbl %>%
    aggregate_time_series()


# Here we are simply testing our data
# We get a 60x3 tibble
# date, total_sales, and label_text as the columns
# We have changed the time unit to "day"
processed_data_tbl %>%
    aggregate_time_series(time_unit = "day")


# 3.3 TIME SERIES PLOT ----

data <- processed_data_tbl %>%
    aggregate_time_series("month")

g <- data %>%
    
    ggplot(aes(date, total_sales)) +
    
    geom_line(color = "#2c3e50") +
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")


ggplotly(g, tooltip = "text")


# 3.4 Plot Time Series Function ----


# Here we will create a function called plot_time_series() 
# Our only input will be "data"
# The body of our function will be the code we laid out above in 3.3
plot_time_series <- function(data) {
    g <- data %>%
        
        ggplot(aes(date, total_sales)) +
        
        geom_line(color = "#2c3e50") +
        geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
        geom_smooth(method = "loess", span = 0.2) +
        
        theme_tq() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "", y = "")
    
    
    ggplotly(g, tooltip = "text")
}


# Here we are going to test both of our functions together
# We overall get a nice time series plot of monthly data
processed_data_tbl %>%
    aggregate_time_series() %>%
    plot_time_series()
    



# 4.0 FORECAST -----

# Time Series Forecasting has traditionally been performed using ARIMA, Holt Winters, State Space Models, etc

# We will be using ML. It's much faster. It can pickup trends and seasonality without much tuning.


# 4.1 SETUP TRAINING DATA AND FUTURE DATA ----


data <- processed_data_tbl %>%
    aggregate_time_series(time_unit = "month")


# timetk will help us quickly understand our time series and expand the time stamps into features we can use for ML.
# tk_index() extracts the time series index, which is the time stamp information, as a vector
# It's basically a shortcut for pull(date_column)
# Think of the timestamp information as the "index" of a time series
data %>% tk_index() %>%
    # tk_get_timeseries_signature() converts the timestamp information into a dataframe
    # It's unique to the pattern within the time series you are interested in
    # index.num is the number of seconds since 1970. R uses Jan 1970 as the beginning of time for datetime
    # diff is the difference in seconds between the current timestamp and the previous timestamp
    # It informs us how regular or irregular our time series is
    tk_get_timeseries_signature()


data %>% tk_index() %>%
    # tk_get_timeseries_summary() returns a dataframe with summary info about the time series index
    # Think of units as smallest part of time stamp. For 2011-01-01, the unit is "days"
    # Think of scale as a measure between two observations. 
    # If every observation is spaced one month apart, the scale is a month
    tk_get_timeseries_summary()


# tk_augment_timeseries_signature() is a helper function 
# It simplifies the steps to adding time series signature columns to a dataframe
data %>% tk_augment_timeseries_signature()


# Here we are creating our training set
train_tbl <- data %>%
    tk_augment_timeseries_signature()


# Here we are creating our test set
future_data_tbl <- data %>%
    tk_index() %>%
    # tk_make_future_timeseries() helps in making future time stamps
    # These time stamps extend at same periodicity as the current time stamp scale
    # n_future is the amount of time stamps you want
    # So, if your time unit is month, n_future = 12 will give you 12 months
    # inspect_weekdays analyzes for days each week that should be removed (like weekends)
    # inspect_months analyzes for days every month that should be removed (like holidays)
    tk_make_future_timeseries(n_future = 12, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
    tk_get_timeseries_signature()



# 4.2 MACHINE LEARNING ----


# Here we are creating our XGBoost model
seed <- 123
set.seed(seed)
# boost_tree() sets the parsnip model specification to a Boosted Tree
# Hyperparameters are tunable parameters that can improve the XGBoost model
# mtry is how many columns to use. Using all columns may lead to overfitting
# trees will help set how fast it takes for our model to run. Fewer tress = faster time
# min_n sets the amount of observations needed in each node
# tree_depth
# learn_rate
# loss_reduction sets criteria for splitting. We will only split if it improves model by 1% (0.01)
# set_engine() sets to the appropriate modeling package
# fit.model_spec() fits te model to the data
# The model_spec() version allows us to see the parameters used for the parsnip model fitting process
model_xgboost <- boost_tree(mode = "regression", 
                            mtry = 30, 
                            trees = 500, 
                            min_n = 3, 
                            tree_depth = 8, 
                            learn_rate = 0.01, 
                            loss_reduction = 0.01) %>%
                            set_engine(engine = "xgboost") %>%
                            # "total_sales is proportionate to the rest of the columns in the dataset
                            fit.model_spec(total_sales ~ ., data = train_tbl %>% select(-date, -label_text, -diff))
    

# 4.3 MAKE PREDICTION & FORMAT OUTPUT ----


# Here we are making predictions on new data using the XGBoost model we created
prediction_tbl <- predict(model_xgboost, new_data = future_data_tbl) %>%
    bind_cols(future_data_tbl) %>%
    select(.pred, index) %>%
    rename(total_sales = .pred,
           date = index) %>%
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
    add_column(key = "Prediction")


output_tbl <- data %>%
    add_column(key = "Actual") %>%
    bind_rows(prediction_tbl)


# 4.4 Generate Forecast Function ----


# Here we are creating a function called generate_forecast()
# We are basically copying a ton of stuff we did just above, and putting it into a function
# We are setting the default n_future to 12, but remember we can change that
# We are setting the seed to NULL, but if the user wants the model to be reproducible he can change that
generate_forecast <- function(data, n_future = 12, seed = NULL) {
    
    train_tbl <- data %>% 
        tk_augment_timeseries_signature()
    
    future_data_tbl <- data %>%
        tk_index() %>%
        tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
        tk_get_timeseries_signature() 
    
    time_scale <- data %>%
        tk_index() %>%
        tk_get_timeseries_summary() %>%
        pull(scale)
    
    
    # Now, we will create a logical statement to adjust for the yearly data
    # "If time_scale equals "year", then linear regression for forecasting
    # "Otherwise, use XGBoost for forecasting
    if (time_scale == "year") {
        
        
        # linear_reg sets up a linear regression model specification with parsnip
        model <- linear_reg(mode = "regression") %>%
            set_engine(engine = "lm") %>%
            fit.model_spec(total_sales ~ ., data = train_tbl %>% select(total_sales, index.num))
        
    } else {
        
        
        # Here we are creating our XGBoost model
        seed <- seed
        set.seed(seed)
        model <- boost_tree(
            mode = "regression",
            mtry = 20,
            trees = 500,
            min_n = 3,
            tree_depth = 8,
            learn_rate = 0.01,
            loss_reduction = 0.01) %>%
            set_engine(engine = "xgboost") %>%
            fit.model_spec(total_sales ~ ., data = train_tbl %>% select(-date, -label_text, -diff))
    }
    
    
    prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
        bind_cols(future_data_tbl) %>%
        select(.pred, index) %>%
        rename(total_sales = .pred, 
               date        = index) %>%
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
        add_column(key = "Prediction")
    
    output_tbl <- data %>%
        add_column(key = "Actual") %>%
        bind_rows(prediction_tbl) 
    
    return(output_tbl)
}


# Here we are applying our function to a dataset
processed_data_tbl %>%
    aggregate_time_series(time_unit = "month") %>%
    generate_forecast(n_future = 12, seed = 123) %>%
    tail(20)




# 5.0 PLOT FORECAST ----


# 5.1 PLOT ----


# Here we are plotting our forecast
# First we will make the forecast
data <- processed_data_tbl %>%
    aggregate_time_series(time_unit = "month") %>%
    generate_forecast(n_future = 12, seed = 123)


# And now we will do the official plotting
g <- data %>%
    ggplot(aes(date, total_sales, color = key)) +
    geom_line() +
    geom_point(aes(text = label_text), size = 0.1) +
    geom_smooth(method = "loess", span = 0.2) +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")


# Here we are making the plot interactive with ggplotly
ggplotly(g, tooltip = "text")


# 5.2 Plot Forecast Function ----


# Here we are creating a function called plot_forecast()
# We are basically copying all of the plotting method we did just above
plot_forecast <- function(data) {
    
    
    # Much like with our generate_forecast() function, we will set a separate scale for year
    time_scale <- data %>%
        tk_index() %>%
        tk_get_timeseries_summary() %>%
        pull(scale)
    
    
    # Here we are creating a value that simply counts the number of predictions made
    # The amount of predictions will be based on time unit and n_future from other functions
    n_predictions <- data %>%
        filter(key == "Prediction") %>%
        nrow()
    
    
    g <- data %>%
        ggplot(aes(date, total_sales, color = key)) +
        geom_line() +
        #geom_point(aes(text = label_text), size = 0.1) +
        #geom_smooth(method = "loess", span = 0.2) +
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        expand_limits(y = 0) +
        labs(x = "", y = "")
    
    
    # Here we are creating a logical statement much like with generate_forecast()
    # "If time scale equals "year", then add geom_smooth method of "lm"
    # "Otherwise, add geom_smooth method of "loess"
    if (time_scale == "year") {
        g <- g +
            geom_smooth(method = "lm")
    } else {
        g <- g +
            geom_smooth(method = "loess", span = 0.2)
    }
    
    
    # Only 1 Prediction
    if (n_predictions == 1) {
        g <- g + geom_point(aes(text = label_text), size = 1)
    } else {
        g <- g + geom_point(aes(text = label_text), size = 0.01)
    }
    
    
    # Here we are making the plot interactive with ggplotly
    ggplotly(g, tooltip = "text")
    
    
}


# Here we are testing out our plot_forecast function
processed_data_tbl %>%
    aggregate_time_series(time_unit = "month") %>%
    generate_forecast(n_future = 12, seed = 123) %>%
    plot_forecast()
    

# Here we are testing out our plot_forecast function, but on daily data
processed_data_tbl %>%
    aggregate_time_series(time_unit = "day") %>%
    generate_forecast(n_future = 365, seed = 123) %>%
    plot_forecast()


# Here we are testing out our plot_forecast function, but on weekly data
processed_data_tbl %>%
    aggregate_time_series(time_unit = "week") %>%
    generate_forecast(n_future = 52, seed = 123) %>%
    plot_forecast()


# Here we are testing out our plot_forecast function, but on quarterly data
processed_data_tbl %>%
    aggregate_time_series(time_unit = "quarter") %>%
    generate_forecast(n_future = 4, seed = 123) %>%
    plot_forecast()


# Here we are testing out our plot_forecast function, but on yearly data
# The forecast looks pretty crummy because there is not enough of a pattern for XGBoost to detect
# Very rare in time series for one model to fit all sizes
processed_data_tbl %>%
    aggregate_time_series(time_unit = "year") %>%
    generate_forecast(n_future = 2, seed = 123) %>%
    plot_forecast()



# Here we are applying our function to a dataset
processed_data_tbl %>%
    aggregate_time_series(time_unit = "year") %>%
    generate_forecast(n_future = 2, seed = 123) 
    


# Here we are testing out our plot_forecast function, but on yearly data
# This time we have adjusted the generate_forecast() function to have linear regression for yearly data
processed_data_tbl %>%
    aggregate_time_series(time_unit = "year") %>%
    generate_forecast(n_future = 2, seed = 123) %>%
    plot_forecast()



# Here we are testing out our plot_forecast function, but on yearly data of only one year
processed_data_tbl %>%
    aggregate_time_series(time_unit = "year") %>%
    generate_forecast(n_future = 1, seed = 123) %>%
    plot_forecast()




# 6.0 SAVE FUNCTIONS ----

dump(c("aggregate_time_series", "plot_time_series", "generate_forecast", "plot_forecast"), 
     file = "00_scripts/04_demand_forecast.R")
