generate_new_bike <- function(bike_model, category_1, category_2, frame_material, .ml_model) {
    

    new_bike_tbl <- tibble(
        # The column name must be "model" because that is the column name in our functions
        model = bike_model,
        category_1 = category_1,
        category_2 = category_2,
        frame_material = frame_material
    ) %>%
        # We will apply the separate bike model function to create the features for us
        separate_bike_model()
    
    
    predict(.ml_model, new_data = new_bike_tbl) %>%
        # bind_cols() binds two data frames column-wise. You can also use bind_rows() for a similar operation
        # This will give us a 1x14 tibble with the major columns we need for prediction
        bind_cols(new_bike_tbl) %>%
        # We are simply renaming the .pred column into price
        rename(price = .pred)
    
    
}


format_table <- function(new_bike_tbl) {
    
    
    # Here we are formatting our data so we can upload it to an output table correctly
    new_bike_tbl %>%
        mutate(price = scales::dollar(price, accuracy = 1)) %>%
        # gather() converts a wide tibble to long tibble. spread() is similar
        # tidyr does also contain pivot_wide() and pivot_long()
        # So now we have a 13x3 tibble with model, New Model Attribute, and value as the column names
        # factor_key keeps the column order the same
        gather(key = "New Model Attribute", value = "value", -model, factor_key = TRUE) %>%
        # So now we have a 13x2 tibble with New Model Attribute and Jekyll Aluminum Black 1 as the column names
        spread(key = model, value = value)
    
}


bind_bike_prediction <- function(bikes_tbl, new_bike_tbl) {
    
    
    bikes_tbl %>%
        separate_bike_description() %>%
        mutate(estimate = "Actual") %>%
        # bind_rows() stacks the tibbles on top of each other
        bind_rows(
            new_bike_tbl %>% mutate(estimate = "Prediction")
        ) %>%
        select(estimate, model, category_1, category_2, frame_material, price)
    
    
}


plot_bike_prediction <- function(data, interactive = TRUE) {
    
    g <- data %>%
        # Right now category_2 is ordered alphabetically 
        # fct_reorder() reorders factors by another feature (usually numeric like price)
        # So now category_2 is ordered by PRICE and not alphabetically 
        mutate(category_2 = fct_reorder(category_2, price)) %>%
        # We're creating a new column called "label_text"
        # When we hover over a data point, it will show the label_text
        mutate(label_text = str_glue("Unit Price: {scales::dollar(price, accuracy = 1)}
                                 Model: {model} 
                                 Bike Type: {category_1}
                                 Bike Family: {category_2}
                                 Frame Material: {frame_material}"
        )) %>%
        ggplot(aes(category_2, price, color = estimate)) +
        # The violin plot will help show us the distribution of the data
        geom_violin() +
        # geom_jitter() is similar to geom_point, but adds some randomness to prevent points from overlapping
        # The width() argument will keep things tighter
        # alpha will allow us to see if data points are overlapping
        geom_jitter(aes(text = label_text), width = 0.1, alpha = 0.5) +
        # Adding the facet_wrap will split the data into two "columns"
        # In this case we now have Aluminum vs. Carbon
        facet_wrap(~ frame_material) +
        coord_flip() +
        # Scaling with log values will make things seem less skewed
        scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
        scale_color_tq() +
        theme_tq() +
        # This will create some space between the column names and their column frame (looks centered and nice instead of squished)
        theme(strip.text.x = element_text(margin = margin(5,5,5,5))) +
        labs(title = "",
             x = "",
             y = "Log Scale")
    
    
    # "if interactive, then return a ggplotly interactive chart, otherwise return normal ggplot chart"
    if (interactive) {
        
        return(ggplotly(g, tooltip = "text"))
        
        } else{
            
            return(g)
        }
    
    }
