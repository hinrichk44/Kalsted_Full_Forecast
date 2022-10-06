# DS4B 102-R: SHINY APPS - LEVEL 1 ----
# R PACKAGES

r_pkgs <- c(
    # Core
    # tidyverse contains: dplyr, ggplot2, tidyr, stringr, forcats, tibble, purrr, readr
    "tidyverse",
    # tidyquant for financial analysis in R. Has nice ggplot2 themes
    "tidyquant",
    
    # Database
    # obdc does the actual connection to databases
    "odbc",
    # RSQLites connects specifically to SQLite databases
    "RSQLite",
    
    # Visualization
    # plotly is an interactive visualization package
    "plotly",
    
    # Shiny-verse
    # flexdashboard is an RMarkdown format for creating web applications
    "flexdashboard",
    # shiny integrates user interface elements and reactivity
    "shiny",
    "shinyWidgets",
    # shinyjs integrates JavaScript functionality into Web Apps
    "shinyjs",
    
    # Modeling & Machine Learning
    # parsnip is a modeling API
    "parsnip",
    # rsample is a sampling API
    "rsample",
    # xgboost = "Extreme Gradient Boosted Trees"
    "xgboost"
    )


install.packages(r_pkgs)