library(readr) 
library(dplyr) 
library(readxl) 
library(purrr) 
# Set working directory 
setwd("IN_DIR")

# Get the list of VCF files 
files <- list.files(path = "in", pattern = "*.vcf", full.names = TRUE) 
# Read the templates from the Excel files and preprocess the POS column 

templates <- list( 

  insertional = read_excel("list_i.xlsx") %>% mutate(POS = as.integer(POS)), 
  deletional = read_excel("list_d.xlsx") %>% mutate(POS = as.integer(POS)), 
  substitutional = read_excel("list_s.xlsx") %>% mutate(POS = as.integer(POS)) 
) 
 
# Function to process one file 
process_file <- function(file) { 

  # Read the VCF file, skipping the '##' lines 
  vcf <- read_tsv(file, comment = "##") 

  # Cross variants against the templates 
  map_df(names(templates), function(type) { 
    template <- templates[[type]] 

    # Join the VCF and the template 
    joined <- inner_join(vcf, template, by = c("POS", "REF", "ALT")) 

    # Count the matches and misses 
    yes <- nrow(joined) 
    missed <- nrow(template) - yes 
    no <- nrow(vcf) - yes

    # Return a data frame with the counts 
    data.frame(file = file, type = type, yes = yes, no = no, missed = missed) 
  }) 
} 

# Process all files
output <- map_df(files, process_file) 


# Write the output to a CSV file 
write_csv(output, "output.csv") 
