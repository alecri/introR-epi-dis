# create raw data from marathon data
library(tidyverse)
library(labelled)

load(url("https://alecri.github.io/downloads/data/marathon.Rdata"))

marathon_raw <- marathon %>% 
  mutate(
    sex = as.double(female),
    urinat3p = as.double(urinat3p),
    fluidint = as.double(fluidint),
    waterload = as.double(waterload),
    nsaid = as.double(nsaid)
  ) %>% 
  select(-nas135, -wtdiff, -wtdiffc, -female, -bmi) %>% 
  as.data.frame() %>% 
  remove_labels() %>% 
  remove_attributes("format.stata")

write.csv(marathon_raw, here::here("data", "raw", "marathon_raw.csv"), row.names = FALSE)
