aggregate_time_series <-
function(series_data, time_unit = "month") {
    
    
    time_series_tbl <- series_data %>%
        mutate(date = floor_date(order.date, unit = time_unit)) %>%
        group_by(date) %>%
        summarize(total_sales = sum(extended_price)) %>%
        ungroup() %>%
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))
    
    return(time_series_tbl)
    
}
plot_time_series <-
function(sales_data) {
    
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
generate_forecast <-
function(sales_data, n_future = 12, seed = NULL) {
    
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
plot_forecast <-
function(sales_data) {
    
    
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
