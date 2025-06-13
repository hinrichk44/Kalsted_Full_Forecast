# Introduction ----

# This script will produce a demand forecast analysis for bike sales




# 1.0 Libraries -----


# Core
library(tidyverse)
library(tidyquant)

# Interactive Visualizations
library(plotly)

# Modeling Libraries
library(parsnip)
# The timetk (Time-Series Tool Kit) package extends the tidyverse so you can treat dates/times just like any other column.
# But still taking advantage of time-series–specific helpers. 
# Because everything returns a tibble, timetk plugs directly into dplyr, tidyr, modeltime, and the broader tidymodels ecosystem, 
# Making it easy to build end-to-end forecasting workflows (data prep → modeling → back-testing → refitting).
library(timetk)

# Database
library(odbc)
library(RSQLite)




# 2.0 Loading Data from Database ----

# Connection errors can happen when your RMarkdown document is run in a different environment than your Global Environment
# This can make it difficult to source files when working on your RMarkdown document
# To get around this, we can create a path that is just used for debugging while we are running scripts in our Global Environment
# Basically there's a difference between "../00_data/bikes_database.db" and "/00_data/bikes_database.db"
# The two dots make a difference in where the data comes from


# Here we are setting up a connection with our SQL database
# We are going to store our connection as an object named "con"
# dbConnect() comes from the DBI package and makes a database connection
# SQLite() is a driver that is used to make the connection to SQLite databases
#  "../00_data/bikes_database.db" is the actual path to the database
# In your environment tab you should see the connection under Data
# It will say "Formal Class SQLiteConnection"
# setwd("C:/Users/hkalsted/OneDrive - Technomics/Desktop/DS4B_102_R_Shiny_Apps_1") will allow this line of code to run smoothly
con <- dbConnect(RSQLite::SQLite(), "00_data/bikes_database.db")


# tbl() enables working with database tables via dplyr
# tbl() only pulls in the first 1000 rows to show a snapshot of the database
# When connecting with databases, tbl() does not pull the data into memory
# It's making a reference to the database table
# So, bikes_tbl is a List of 2
# Source:   table<bikes> [?? x 4]
# R knows there are four columns in this table, but it doesn't know how many rows are in the database since data has not been collected yet.
# This is why you see the double ?? in [?? x 4]
# Database: sqlite 3.41.2 [C:\Users\hkalsted\OneDrive - Technomics\Desktop\DS4B_102_R_Shiny_Apps_1\00_data\bikes_database.db]
#           bike.id   model                          description                  price
#           <dbl>    <chr>                          <chr>                        <dbl>
#     1       1      Supersix Evo Black Inc.        Road - Elite Road - Carbon   12790
bikes_tbl <- tbl(con, "bikes")


# bikeshops_tbl is a List of 2
# Source:   table<bikeshops> [?? x 3]
# Database: sqlite 3.41.2 [C:\Users\hkalsted\OneDrive - Technomics\Desktop\DS4B_102_R_Shiny_Apps_1\00_data\bikes_database.db]
#               bikeshop.id    bikeshop.name                   location       
#               <dbl>          <chr>                           <chr>          
#     1           1           Pittsburgh Mountain Machines     Pittsburgh, PA 
bikeshops_tbl <- tbl(con, "bikeshops")


# orderlines_tbl is a List of 2
# Source:   table<orderlines> [?? x 6]
# Database: sqlite 3.41.2 [C:\Users\hkalsted\OneDrive - Technomics\Desktop\DS4B_102_R_Shiny_Apps_1\00_data\bikes_database.db]
#           order.id order.line order.date customer.id product.id quantity
#           <dbl>      <dbl> <chr>            <dbl>      <dbl>    <dbl>
#     1        1          1 2011-01-07           2         48        1
orderlines_tbl <- tbl(con, "orderlines")


# When building Web Apps, speed is of the utmost importance
# To improve speed, we can actually use the database to do much of the data manipulation
# This speeds up Web Apps because SQL databases are designed for performance, whereas R is designed for flexibility
# So here we are joining our tables together and we are doing this BEFORE data is collected into memory. 
# Behind the scenes, all of these computations are performed using dbplyr (database-plyr).
# So, dplyr converts the code to SQL commands via dbplyr.
# This makes calculations much faster. 
combined_data_tbl <- orderlines_tbl %>%
    # Joins can take a long time with large data, so should not be performed within your Web App
    # In left_join() the left dataframe takes priority in the join
    # So in this case the left dataframe is orderlines_tbl
    # Here we are joining customer.id from orderlines_tbl with bikeshop.id from bikeshops_tbl
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id")) %>%
    # Here we are joining bikes.id from bikes_tbl with product.id from orderlines_tbl
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    # Here we are going to create a new column called "extended_price"
    # It will simply be the quantity times price
    mutate(extended_price = quantity * price) %>%
    # Once computations are performed using the database back-end, we collect() the data to pull it into R's memory
    # We officially have a 15,644 x 12 tibble after this entire process
    collect()


# So now that we've brought in and joined our data together, we are going to do some data wrangling 
# When data is imported from a database it might not be formatted in the way we actually want to use it
# Because the data has been collected, these computations are performed using the R/Shiny server.
# These computations will not be as fast as SQL, but are very flexible. 
processed_data_tbl <- combined_data_tbl %>%   
    # order.date is imported as a character column. We need to convert it to a date column
    # ymd() comes from the lubridate package and converts text in a year-month-day character data type into a date data type.
    mutate(order.date = ymd(order.date)) %>%
    # separate() separates a column into multiple columns using a separator
    # So the location column has city and state together like Ithaca, NY
    # We will separate these two values into different columns "city" and "state"
    # We separate them by comma because that is how they are formatted in the location column
    separate(location, into = c("city", "state"), sep = ", ") %>%
    # Here we are separating the description column into three different values
    # These values are separated by a hyphen in the description column, so that's how we separate them individually
    separate(description, into = c("category_1", "category_2", "frame_material"), sep = " - ") %>%
    select(order.date, order.id, order.line, state, quantity, price,
           extended_price, category_1:frame_material, bikeshop.name)


# Disconnecting from the database as is standard practice
dbDisconnect(con)




# 3.0 Time Series Aggregation ----


# * 3.1 Data Manipulation ----

# So here we are setting up a variable "time_unit" and we will assign it the value of "quarter"
# This unit will be how our time series plot is portrayed (quarterly, yearly, monthly, etc)
time_unit <- "quarter"

# And now here we are going to create our tibble that generates the time series data we will plot
time_plot_tbl <- processed_data_tbl %>%
    # So here we are going to create a date column based on the order.date column
    # floor_date() comes from lubridate and converts dates to a floored value for any date within a range using a time unit
    # So we designated the time unit to be "quarter" up above
    mutate(date = floor_date(order.date, unit = time_unit)) %>%
    group_by(date) %>%
    # So here we are finding the total sales per time unit
    # In this case total quarterly sales since we set up the unit to be quarterly
    summarize(total_sales = sum(extended_price)) %>%
    ungroup() %>%
    # This will end up being formatted like: "Date: 2011-04-01\nRevenue: $4,084,755"
    # The \n indicates a new line
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))

# Here we have a 20x3 tibble
#     date       total_sales    label_text                          
#     <date>           <dbl>    <glue>                              
# 1 2011-01-01     2305065      Date: 2011-01-01 Revenue: $2,305,065
time_plot_tbl


# * 3.2 Aggregate Time Series Function ----

# Here we will create a function called aggregate_time_series() 
# Our inputs will be series_data and time_unit
# The default time unit will be month, but you can change that
# The user will specify what data they want to input. This allows for flexibility instead of having it hard coded. 
# Within our function we are simply copy and pasting the data manipulation in 3.1
# In terms of a data process pipeline, it's good to ingest data then process it with functions to simplify our workflow
aggregate_time_series <- function(series_data, time_unit = "month") {
    
    
    time_series_tbl <- series_data %>%
        mutate(date = floor_date(order.date, unit = time_unit)) %>%
        group_by(date) %>%
        summarize(total_sales = sum(extended_price)) %>%
        ungroup() %>%
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))
    
    return(time_series_tbl)
    
}


# Here we are simply testing our function
# We get a 60x3 tibble
# date, total_sales, and label_text as the columns
# Remember the default time unit is "month"
processed_data_tbl %>%
    aggregate_time_series()


# Here we are simply testing our function
# We get a 962x3 tibble
# date, total_sales, and label_text as the columns
# We have changed the time unit to "day"
processed_data_tbl %>%
    aggregate_time_series(time_unit = "day")


# * 3.3 Time Series Plot ----

# Here we are creating a tibble using our custom aggregate_time_series() function
monthly_sales_data <- processed_data_tbl %>%
    # We will make our sales data monthly
    aggregate_time_series(time_unit = "month")

# And now we are converting that monthly sales data into a ggplot object
monthly_sales_plot <- monthly_sales_data %>%
    # Creating the blank canvas for our plot
    ggplot(aes(date, total_sales)) +
    # Because it's a time series plot we will use geom_line()
    geom_line(color = "#2c3e50") +
    # We will add points that our line will go through
    # These points will be annotated with our label_text column
    # So, labels will be generated when we hover over the points
    # When you plan to convert the graphic with ggplotly(), mapping a column to text is a legitimate trick—the value is picked up as hover text. 
    # It will come with a warning "Ignoring unknown aesthetics: text"
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    # geom_smooth() adds trend lines to the plot data
    # LOESS stands for "locally estimated scatterplot smoothing"
    # The span argument adjusts the flexibility of the trend line
    # We can make the trend line almost fit identically to the data, or more flexible
    geom_smooth(method = "loess", span = 0.2) +
    # Adding tidyquant theme to the plot
    theme_tq() +
    # Here we are making sure that the y-axis starts at 0
    expand_limits(y = 0) +
    # scale_y_continuous() allows us to modify the y-axis when it's a continuous variable
    # We will make sure the y-axis is formatted in a dollar value
    scale_y_continuous(labels = scales::dollar_format()) +
    # We are removing the x and y labels since they are self-explanatory from the chart
    labs(x = "", y = "")

# Here we are making our visualization interactive
# ggplotly() converts a ggplot into an interactive plotly object
# tooltip = "text" refers to this line of code: geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) 
ggplotly(monthly_sales_plot, tooltip = "text")


# * 3.4 Plot Time Series Function ----

# Here we will create a function called plot_time_series() 
# Our only input will be "sales_data"
# The body of our function will be the code we laid out above in 3.3
plot_time_series <- function(sales_data) {
    
    sales_plot <- sales_data %>%
        ggplot(aes(date, total_sales)) +
        geom_line(color = "#2c3e50") +
        geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
        geom_smooth(method = "loess", span = 0.2) +
        theme_tq() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "", y = "")
    
    
    ggplotly(sales_plot, tooltip = "text")
}


# Here we are going to test both of our functions together
# We overall get a nice time series plot of monthly data (since the default time unit is month)
processed_data_tbl %>%
    aggregate_time_series() %>%
    plot_time_series()




# 4.0 Time Series Forecasting -----

# Time Series Forecasting has traditionally been performed using ARIMA, Holt Winters, State Space Models, etc
# We will be using ML. It's much faster. It can pickup trends and seasonality without much tuning.
# Autoregressive techniques can be much slower for forecasting.
# ARIMA requires a lot of tuning and experience to get good results.
# Tuning ARIMA models is different depending on the time series, which makes it NOT flexible to different data sets. 


# * 4.1 Setting Up Training and Test Data ----

# So first we will create our full time series data
# In this case a monthly sales time series
monthly_sales_data <- processed_data_tbl %>%
    # We are using our custom aggregate_time_series() function
    aggregate_time_series(time_unit = "month")


# So now we are using some functions from the timetk package to review our time series data.
# The timetk package will help us quickly understand our time series and expand the time stamps into features we can use for ML.
monthly_sales_signature <- monthly_sales_data %>% 
    # tk_index() extracts the time series index, which is the time stamp information, as a vector
    # It's basically a shortcut for pull(date_column)
    # Think of the timestamp information as the "index" of a time series
    # [1] "2011-01-01" "2011-02-01" "2011-03-01"
    tk_index() %>%
    # tk_get_timeseries_signature() converts the timestamp information into a dataframe of key info about that time series index
    # It's a "signature" because it's unique to the pattern within the time series you are interested in
    # The index.num column is the number of seconds since 1970. R uses Jan 1970 as the beginning of time for datetime
    # The diff column is the difference in seconds between the current timestamp and the previous timestamp (good for understanding periodicity)
    # It informs us how regular or irregular our time series is.
    # In time-series analysis, periodicity is the fixed number of observations that make up one complete cycle of a recurring pattern.
    # Periodicity is the count of observations per cycle.
    # For example the periodicity of a day is 24 (24 hours in a day), and for a week is 7 (7 days a week)
    # Periodicity is NOT the same thing as Seasonality
    # Think of periodicity as the clock tick and seasonality as what actually changes when that clock ticks.
    # The half column tells us which half of the year the date is in
    # The month.xts column is a numerical representation of the month (January = 0)
    tk_get_timeseries_signature()


# A tibble: 60 x 29
#       index       index.num    diff  year year.iso  half quarter month month.xts month.lbl   day  hour minute second hour12 am.pm  wday wday.xts wday.lbl  mday  qday  yday mweek
#       <date>          <dbl>   <dbl> <int>    <int> <int>   <int> <int>     <int> <ord>     <int> <int>  <int>  <int>  <int> <int> <int>    <int> <ord>    <int> <int> <int> <int>
#     1 2011-01-01 1293840000      NA  2011     2010     1       1     1         0 January       1     0      0      0      0     1     7        6 Saturday     1     1     1     5
monthly_sales_signature


monthly_sales_summary <- monthly_sales_data %>% 
    tk_index() %>%
    # tk_get_timeseries_summary() returns a dataframe with summary info about the time series index
    # Think of units as the smallest part of the time stamp. For 2011-01-01, the unit is "days"
    # Think of scale as a measure between two observations. 
    # If every observation is spaced one month apart, the scale is a month
    tk_get_timeseries_summary()


# A tibble: 1 x 12
#           n.obs start      end        units scale tzone diff.minimum diff.q1 diff.median diff.mean diff.q3 diff.maximum
#           <int> <date>     <date>     <chr> <chr> <chr>        <dbl>   <dbl>       <dbl>     <dbl>   <dbl>        <dbl>
#     1    v60   2011-01-01 2015-12-01 days  month UTC        2419200 2592000     2678400  2628610. 2678400      2678400
monthly_sales_summary


monthly_sales_augmented <- monthly_sales_data %>% 
    # tk_augment_timeseries_signature() is a helper function 
    # It simplifies the steps to adding time series signature columns to a dataframe
    # So we take the original dataframe and then add columns, NOT rows, to it
    # We go from a 60x3 tibble to a 60x31 tibble
    tk_augment_timeseries_signature()


# A tibble: 60 x 31
#       date       total_sales label_text   index.num    diff  year year.iso  half quarter month month.xts month.lbl   day  hour minute second hour12 am.pm  wday wday.xts wday.lbl
#       <date>           <dbl> <glue>           <dbl>   <dbl> <int>    <int> <int>   <int> <int>     <int> <ord>     <int> <int>  <int>  <int>  <int> <int> <int>    <int> <ord>   
    # 1 2011-01-01      483015 Date: 2011-~    1.29e9      NA  2011     2010     1       1     1         0 January       1     0      0      0      0     1     7        6 Saturday
monthly_sales_augmented


# Here we are officially creating our training set
train_tbl <- monthly_sales_data %>%
    # So we are taking our monthly sales data and applying the tk_augment_timeseries_signature() function from the timetk package
    # Typical in ML we would do a train-test split of the data, but that is not the case here
    tk_augment_timeseries_signature()


# A tibble: 60 x 31
#       date       total_sales label_text   index.num    diff  year year.iso  half quarter month month.xts month.lbl   day  hour minute second hour12 am.pm  wday wday.xts wday.lbl
#       <date>           <dbl> <glue>           <dbl>   <dbl> <int>    <int> <int>   <int> <int>     <int> <ord>     <int> <int>  <int>  <int>  <int> <int> <int>    <int> <ord>   
# 1 2011-01-01      483015 Date: 2011-~    1.29e9      NA  2011     2010     1       1     1         0 January       1     0      0      0      0     1     7        6 Saturday
train_tbl


# Here we are officially creating our test set
future_data_tbl <- monthly_sales_data %>%
    # So first we will use tk_index() to extract the time series index
    tk_index() %>%
    # tk_make_future_timeseries() helps in making future time stamps
    # These time stamps extend at same periodicity as the current time stamp scale
    # n_future is the amount of time stamps you want
    # So, if your time unit is month, n_future = 12 will give you 12 months
    # Since we are indeed working with monthly data we will be using n_future = 12
    # inspect_weekdays analyzes for days each week that should be removed (like weekends)
    # inspect_months analyzes for days every month that should be removed (like holidays)
    tk_make_future_timeseries(n_future = 12, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
    tk_get_timeseries_signature()


# A tibble: 12 x 29
#       index       index.num    diff  year year.iso  half quarter month month.xts month.lbl   day  hour minute second hour12 am.pm  wday wday.xts wday.lbl   mday  qday  yday mweek  week week.iso week2
#       <date>          <dbl>   <dbl> <int>    <int> <int>   <int> <int>     <int> <ord>     <int> <int>  <int>  <int>  <int> <int> <int>    <int> <ord>     <int> <int> <int> <int> <int>    <int> <int>
    # 1 2016-01-01 1451606400      NA  2016     2015     1       1     1         0 January       1     0      0      0      0     1     6        5 Friday        1     1     1     5     1       53     1
future_data_tbl


# * 4.2 Machine Learning for Forecasting ----

# Setting the seed for data reproducibility 
set.seed(123)


# Here we are creating our XGBoost model using the parsnip package
# Hyperparameters are tunable parameters that can improve the XGBoost model
# This can be tough since there are many combinations of values to use for the model
# We are going to make several assumptions about the data to reduce combining just a ton of features
# boost_tree() sets the parsnip model specification to a Boosted Tree
# The mode is "regression" and not "classification"
ts_model_xgboost <- boost_tree(mode = "regression",
                            # mtry is how many columns to use. Using all columns may lead to overfitting
                            mtry = 30, 
                            # trees will help set how fast it takes for our model to run. Fewer tress = faster time
                            trees = 500, 
                            # min_n sets the amount of observations needed in each node. Each node must have 3 values at minimum
                            min_n = 3, 
                            # tree_depth
                            # Max tree depth is 8 to prevent overfitting
                            tree_depth = 8, 
                            # learn_rate
                            learn_rate = 0.01, 
                            # loss_reduction sets criteria for splitting. We will only split if it improves model by 1% (0.01)
                            loss_reduction = 0.01) %>%
    # set_engine() sets to the appropriate modeling package
    set_engine(engine = "xgboost") %>%
    # fit.model_spec() fits the model to the data
    # The model_spec() version allows us to see the parameters used for the parsnip model fitting process
    # "total_sales is proportionate to the rest of the columns in the dataset"
    # We will be excluding date, label_text, and diff from the model
    fit.model_spec(total_sales ~ ., data = train_tbl %>% select(-date, -label_text, -diff))


# * 4.3 Making Prediction Using XGBoost Model ----


# Here we are making predictions on new data using the XGBoost model we created
# So first we run the predict() function on our future data
# This gives us a 12x1 tibble with the column ".pred"
prediction_tbl <- predict(ts_model_xgboost, new_data = future_data_tbl) %>%
    # Then we will add the future data itself to this tibble
    # This gives us a 12x30 tibble
    bind_cols(future_data_tbl) %>%
    # Now we are only selecting the predicted values and the time series index
    select(.pred, index) %>%
    rename(total_sales = .pred,
           date = index) %>%
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
    # add_column() is similar to mutate() and is useful for adding a column that is a vector
    # Since these are predicted values we want to label them that way
    add_column(key = "Prediction")


# A tibble: 12 x 4
    #         total_sales date       label_text                       key       
    #         <dbl> <date>     <glue>                               <chr>     
    # 1     764236. 2016-01-01 Date: 2016-01-01 Revenue: $764,236   Prediction
prediction_tbl


# So here we are going to combine the predicted values with the actual values
output_tbl <- monthly_sales_data %>%
    # Since these are actual values we want to label them that way
    add_column(key = "Actual") %>%
    # We want to create a full dataframe that contains the actual data and the forecasted predictions
    bind_rows(prediction_tbl)


# * 4.4 Generate Forecast Function ----


# Here we are creating a function called generate_forecast()
# We are basically copying a ton of stuff we did just above, and putting it into a function
# The sales_data argument is our true time series data that we will use for making predictions
# We are setting the default n_future to 12, but remember we can change that
# We are setting the seed to NULL, but if the user wants the model to be reproducible he can change that
generate_forecast <- function(sales_data, n_future = 12, seed = NULL) {
    
    train_tbl <- sales_data %>% 
        tk_augment_timeseries_signature()
    
    future_data_tbl <- sales_data %>%
        tk_index() %>%
        tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
        tk_get_timeseries_signature() 
    
    time_scale <- sales_data %>%
        tk_index() %>%
        tk_get_timeseries_summary() %>%
        # The scale will get us what the time series is denoted in
        # Could be year, month, day, etc
        pull(scale)
    
    
    # Now, we will create a logical statement to adjust for the yearly data
    # "If time_scale equals "year", then linear regression for forecasting
    # "Otherwise, use XGBoost for forecasting
    # XGBoost does not do well when there's limited data like when we convert time series data to yearly
    # We will use simple linear regression instead
    if (time_scale == "year") {
        
        # linear_reg sets up a linear regression model specification with the parsnip package
        # So we used parsnip to build both our xgboost and linear regression model
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
    
    
    # Creating our table of predictions
    prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
        bind_cols(future_data_tbl) %>%
        select(.pred, index) %>%
        rename(total_sales = .pred, 
               date        = index) %>%
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
        add_column(key = "Prediction")
    
    
    output_tbl <- sales_data %>%
        add_column(key = "Actual") %>%
        bind_rows(prediction_tbl) 
    
    
    # We return a tibble containing our actual values and our predicted values
    return(output_tbl)
    
}


# Here we are applying our function to the processed_data_tbl that we created at the very beginning of this script
# A tibble: 20 x 4
#       date       total_sales label_text                           key       
#       <date>           <dbl> <glue>                               <chr>     
#     1 2015-05-01    2307255  Date: 2015-05-01  Revenue: $2,307,255 Actual 
processed_data_tbl %>%
    aggregate_time_series(time_unit = "month") %>%
    generate_forecast(n_future = 12, seed = 123) %>%
    tail(20)




# 5.0 Plotting the Forecast ----


# * 5.1 Creating the Plot ----


# First we will create the data to be used for our plot
# We will use our custom aggregate_time_series() and generate_forecast() functions
montly_sales_plot_data <- processed_data_tbl %>%
    aggregate_time_series(time_unit = "month") %>%
    generate_forecast(n_future = 12, seed = 123)


# And now we will do the official plotting
montly_sales_plot <- montly_sales_plot_data %>%
    # The color =  key argument will help us distinguish between actual and predicted values
    ggplot(aes(date, total_sales, color = key)) +
    geom_line() +
    # We are making the size of the points small on the line chart
    geom_point(aes(text = label_text), size = 0.1) +
    # This will give us a trend line
    # The span argument will help the trend line fit the data better
    # span controls the amount of smoothing for the default loess smoother. Smaller numbers produce wigglier lines, larger numbers produce smoother lines.
    geom_smooth(method = "loess", span = 0.2) +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")


# Here we are making the plot interactive with ggplotly
ggplotly(montly_sales_plot, tooltip = "text")


# * 5.2 Plot Forecast Function ----


# Here we are creating a function called plot_forecast()
# We are basically copying all of the plotting method we did just above
plot_forecast <- function(sales_data) {
    
    
    # Much like with our generate_forecast() function, we will set a separate scale for year
    time_scale <- sales_data %>%
        # Extracting the time series index
        tk_index() %>%
        # Getting the time series summary as a tibble
        tk_get_timeseries_summary() %>%
        # Retrieving just the scale column from the tibble
        pull(scale)
    
    
    # Here we are creating a value that simply counts the number of predictions made
    # The amount of predictions will be based on time unit and n_future from other functions
    n_predictions <- sales_data %>%
        filter(key == "Prediction") %>%
        nrow()
    
    
    sales_plot_data <- sales_data %>%
        ggplot(aes(date, total_sales, color = key)) +
        geom_line() +
        # geom_point(aes(text = label_text), size = 0.1) +
        # geom_smooth(method = "loess", span = 0.2) +
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        # expand_limits() expands the range of an axis
        # It adjusts the range of the axis without removing data points
        # So here we are setting the y-axis to 0
        expand_limits(y = 0) +
        labs(x = "", y = "")
    
    
    # Here we are creating a logical statement much like with generate_forecast()
    # "If time scale equals "year", then add geom_smooth method of "lm"
    # Remember that we set up the time scale earlier in this function
    # "Otherwise, add geom_smooth method of "loess"
    # We are doing this because we found out that using loess on yearly data leads to a terrible chart
    # This is due to the lack of data when denoting the data yearly
    if (time_scale == "year") {
        
        sales_plot_data <- sales_plot_data +
            geom_smooth(method = "lm")
        
    } else {
        
        sales_plot_data <- sales_plot_data +
            geom_smooth(method = "loess", span = 0.2)
        
    }
    
    
    # "If there is only one prediction being made..."
    if (n_predictions == 1) {
        
        sales_plot_data <- sales_plot_data + 
            # We are increasing the size of the geom_point
            geom_point(aes(text = label_text), size = 1)
        
    } else {
        
        sales_plot_data <- sales_plot_data +
            geom_point(aes(text = label_text), size = 0.01)
        
    }
    
    
    # Here we are making the plot interactive with ggplotly
    ggplotly(sales_plot_data, tooltip = "text")
    
    
}


# Here we are testing out our plot_forecast function
# We are doing monthly data with 12 predictions
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
# NOTE: The chart looks good now since we adjusted our function
# But the first time we ran our function (pre-adjustment) the yearly data looked terrible
# processed_data_tbl %>%
#     aggregate_time_series(time_unit = "year") %>%
#     generate_forecast(n_future = 2, seed = 123) %>%
#     plot_forecast()


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




# 6.0 Saving Custom Functions ----

dump(c("aggregate_time_series", "plot_time_series", "generate_forecast", "plot_forecast"), 
     file = "00_scripts/04_demand_forecast.R")
