if (!requireNamespace('renv')) install.packages('renv')

renv::init()
renv::install.packages(c("tidyverse", "lubridate","RColorBrewer", "lubridate", "igraph", "readxl",
                         "ggraph", "plyr", "shiny",
                         "tidygraph", "shinythemes", "shinyWidgets"))
renv::snapshot()
