library(readr)
library(dplyr)
library(readxl)

# Set working directory
setwd("IN_DIR")

# Get the list of VCF files
files <- list.files(path = "in", pattern = "*.vcf", full.names = TRUE)

# Initialize an empty data frame for the output
output <- data.frame(file = character(), freq = double(), yes = integer(), no = integer(), missed = integer())

# Read the template from the Excel file and preprocess the POS column
template <- read_excel("list_vector.xlsx") %>%
  mutate(
    POS = as.integer(POS),
    ID = paste(POS, ALT, sep="_")
  )

# Loop over each file
for (file in files) {
  # Read the VCF file, skipping the '##' lines
  vcf <- read_tsv(file, comment = "##") %>%
    mutate(ID = paste(POS, ALT, sep="_"))
  
  # Get unique frequencies
  freqs <- unique(template$FREQ)
  
  # Loop over each frequency
  for (freq in freqs) {
    # Filter template for current frequency
    template_freq <- template %>% filter(FREQ == freq)
    
    # Initialize counters
    yes <- 0
    no <- 0
    
    # Cross variants against the template
    for (i in seq_along(vcf$ID)) {
      vcf_id <- vcf$ID[i]
      
      template_rows <- template_freq %>% filter(ID == vcf_id)
      
      if(nrow(template_rows) > 0){
        yes <- yes + 1
      } else {
        no <- no + 1
      }
    }
    
    # Count missed variants
    missed <- nrow(template_freq) - yes
    
    # Add the counts for this file to the output data frame
    output <- rbind(output, data.frame(file = file, freq = freq, yes = yes, no = no, missed = missed))
  }
}

# Write the output to a CSV file
write_csv(output, "out.csv")
