separate_bike_model <-
function(data, keep_model_column = TRUE, append = TRUE) {
    
    
    # "if not append, then simply select model column from our data"
    if (!append) {data <- data %>% select(model)}
    
    
    # 2.2 Process Model Column ----
    # Fix typo
    output_tbl <- data %>% 
        mutate(model = case_when(
            model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
            model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
            model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
            TRUE ~ model
    )) %>%
        
        # Separate "model" column using spaces
        # We will now get model_1, model_2, etc up until model_7
        separate(col     = model, 
                 into    = str_c("model_", 1:7), 
                 sep     = " ", 
                 remove  = FALSE, 
                 fill    = "right") %>%
        
        # Creating a "base" feature
        mutate(model_base = case_when(
            
            # Fix Supersix Evo
            str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Fat CAAD bikes
            str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Beast of the East
            str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
            
            # Fix Bad Habit
            str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Scalpel 29
            str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
            
            # catch all
            TRUE ~ model_1)
        ) %>%
        
        # Get "tier" feature
        mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
        
        # Remove unnecessary columns
        select(-matches("model_[0-9]")) %>%
        
        # Create Flags
        # So here we are creating a numeric value for each time one of these flags appear in the Bike Models
        # So all of these flags will become individual columns with numerical values in the rows
        mutate(
            black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
            hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
            team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
            red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
            ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
            dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
            disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
        )
    
    
    
    # "if not keep model column, then remove model column from output_tbl"
    if (!keep_model_column) output_tbl <- output_tbl %>% select(-model)
    
    
    return(output_tbl)
    
}
separate_bike_description <-
function(data, keep_description_column = TRUE, append = TRUE) {
    
    
    # "if not append, then simply select description column from our data"
    if (!append) {data <- data %>% select(description)}
   
    
     # 2.1 Separate Description Column ----
    output_tbl <- data %>% separate(description, 
             sep    = " - ", 
             # Category 1 = "bike type" and Category 2 = "bike family
             into   = c("category_1", "category_2", "frame_material"), 
             remove = FALSE)
    
    
    # "if not keep description column, then remove description column from output_tbl"
    if (!keep_description_column) output_tbl <- output_tbl %>% select(-description)
    
    return(output_tbl)
    
}
