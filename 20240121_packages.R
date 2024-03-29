#############################################################
#                                                           #
#         Package Installation and Loading                  #
#                                                           #
#                                                           #
#                                           maklee 20220505 #
#############################################################

# Specify your packages
# changed 'rgeos' to 'terra' (no longer available)
my_packages <- c("rlang", "cowplot", "colorspace", "colorblindr", "tidyverse","survey","tidymodels","knitr","haven","caret", "ggbeeswarm",
                 "pscl","datawizard","pROC","ggpubr","broom","broom.mixed","mice",
                 "xgboost","RColorBrewer","ggforce","table1","scales","ggrepel","sf",
                 "rnaturalearth","rnaturalearthdata","terra","MLmetrics", "cowplot",
                 "performanceEstimation","ranger","caTools","PRROC","recipes","ggh4x")
# Extract not installed packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])] 
# Install not installed packages
if(length(not_installed) != 0) if(sum(grepl("ggh4x", not_installed)) != 0) remotes::install_github("teunbrand/ggh4x")
if(length(not_installed) != 0) if(sum(grepl("colorblindr", not_installed)) != 0) remotes::install_github("clauswilke/colorblindr")
if(length(not_installed) != 0) if(sum(grepl("colorspace", not_installed)) != 0) install.packages("colorspace", repos = "http://R-Forge.R-project.org")


if(length(not_installed)) install.packages(not_installed)           

lapply(my_packages, require, character.only = TRUE)

rm(my_packages, not_installed)
