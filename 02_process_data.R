separate_bike_model <-
function(data, keep_model_column = TRUE, append = TRUE) {
    
    
    # "if append = FALSE, then simply select the model column from our data"
    if (!append) {
        
        data <- data %>% select(model)
        
        }
    
    # "else, separate the model column into the code below"
    output_tbl <- data %>% 
        mutate(model = case_when(
            model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
            model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
            model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
            TRUE ~ model)) %>%
        separate(col     = model, 
                 into    = str_c("model_", 1:7), 
                 sep     = " ", 
                 remove  = FALSE, 
                 fill    = "right") %>%
        mutate(
            model_base = case_when(
                str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
                str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
                str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
                str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
                str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
                TRUE ~ model_1)) %>%
        mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
        select(-matches("model_[0-9]")) %>%
        mutate(
            black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
            hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
            team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
            red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
            ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
            dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
            disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
        )
    
    
    
    # "if keep_model_column = FALSE, then remove model column from output_tbl"
    if (!keep_model_column) output_tbl <- output_tbl %>% select(-model)
    
    
    return(output_tbl)
    
}
separate_bike_description <-
function(data, keep_description_column = TRUE, append = TRUE) {
    
    
    # "if append = FALSE, then simply select description column from our data as our output"
    if (!append) {
        
        data <- data %>% select(description)

    } 
      
      
    # "else, if append = TRUE then separate the description column into "category_1", "category_2", and "frame_material" and keep rest of data too"
    output_tbl <- data %>% separate(description, 
                                    sep    = " - ", 
                                    into   = c("category_1", "category_2", "frame_material"), 
                                    remove = FALSE)
        
   
    # "if keep_description_column = FALSE, then remove description column from output_tbl"
    if (!keep_description_column) output_tbl <- output_tbl %>% select(-description)
    
    
    # The outcome of our function is output_tbl
    # So, "data" comes in, and "output_tbl" comes out
    return(output_tbl)
    
}
