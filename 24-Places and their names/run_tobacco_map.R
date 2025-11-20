# Helper script to check packages and run tobacco_map.R

# Function to check and install packages
check_and_install <- function(packages) {
    for (pkg in packages) {
        if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
            cat(sprintf("Installing package: %s\n", pkg))
            install.packages(pkg, repos = "https://cloud.r-project.org/", dependencies = TRUE)
            library(pkg, character.only = TRUE)
        } else {
            cat(sprintf("Package %s is already installed\n", pkg))
        }
    }
}

# Required packages
required_packages <- c("sf", "ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata")

cat("Checking and installing required packages...\n")
check_and_install(required_packages)

cat("\nAll packages installed. Running tobacco_map.R...\n\n")

# Run the main script
source("tobacco_map.R")

cat("\nScript completed! Check for tobacco_heritage_netherlands.png in the current directory.\n")
