# data management for marathon data

# libraries ----

library(tidyverse)
library(here)
library(labelled)
library(skimr)
library(flextable)
library(Hmisc)
library(writexl)


# read raw data ----
marathon_raw <- read.csv(here("data", "raw", "marathon_raw.csv"))


# labeling & coding ----

View(marathon_raw)        # open in a data viewer
head(marathon_raw, 10)    # display first 10 raw
str(marathon_raw)         # get the structure
names(marathon_raw)       # get the columns names
dim(marathon_raw)         # how many raws/variables

## set variable labels ----
marathon <- marathon_raw %>% 
  set_variable_labels(
    id = "ID number",
    na = "Serum sodium concentration (mmol/liter)", 
    sex = "Sex",
    age = "Age (years)" ,
    urinat3p = "Urine output",
    prewt = "Weight (kg) pre-race",
    postwt = "Weight (kg) post-race",
    height = "Height (cm)",
    runtime = "Race duration (minutes)",
    trainpace = "Training pace (seconds/mile)",
    prevmara = "Previous marathons (no.)", 
    fluidint = "Self-reported fluid intake",
    waterload = "Self-reported water loading",
    nsaid = "Self-reported use of NSAIDs"
  )

skimr::skim(marathon)            # overview of summary statistics for all variables
table(marathon$sex)
table(marathon$urinat3p)
table(marathon$fluidint)
table(marathon$waterload)
table(marathon$nsaid)

## set labels to character (factor) variable ----
marathon <- marathon %>% 
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    urinat3p = factor(urinat3p, levels = c(1, 2), labels = c("<3", "≥3")),
    fluidint = factor(fluidint, levels = c(1, 2, 3),
                      labels = c("Every mile", "Every other mile", "Every third mile or less often")),
    waterload = factor(waterload, levels = c(1, 2), labels = c("No", "Yes")),
    nsaid = factor(nsaid, levels = c(1, 2), labels = c("No", "Yes"))
  )
skim(marathon)
look_for(marathon)


# data management ----

## select variables ----
names(marathon)
marathon_subvar <- select(marathon, age, sex, runtime, ends_with("wt"), na)
head(marathon_subvar)

## filter observations ----
women_250 <- filter(marathon_subvar, sex == "Female", runtime > 250)
flextable(women_250)

## sorting ----
arrange(women_250, desc(na), runtime)

## create and modify variables ----
summary(marathon$wtdiff)
summary(marathon$bmi)

marathon <- marathon %>% 
  mutate(
    runtime = runtime/60,
    wtdiff = postwt - prewt,
    wtdiff_cat = cut(wtdiff, breaks = c(-5, -2, -1, 0, 1, 2, 3, 5), 
                     include.lowest = T, right = FALSE, ordered_result = TRUE),
    age_cat = cut2(age, g = 5, digits = 1),
    bmi = prewt/(height^2),
    bmi_cat = cut(bmi, breaks = c(15, 20, 25, 33), labels = c("< 20 (%)", "20-25 (%)", ">25 (%)")),
    nas135 = factor(na <= 135, labels = c("na > 135", "na ≤ 135")),
    nas130 = factor(na <= 130, labels = c("na > 130", "na ≤ 130"))
  ) %>% 
  set_variable_labels(
    runtime = "Race duration (hours)",
    wtdiff = "Weight change (kg) pre/post race",
    wtdiff_cat = "Categorization of weight change",
    age_cat = "Age categories (years)",
    bmi = "Body-mass index (kg/m^2)",
    bmi_cat = "Body-mass index categories (kg/m^2)",
    nas135 = "Serum sodium concentration ≤ 135 mmol/liter",
    nas130 = "Serum sodium concentration ≤ 130 mmol/liter"
  )



# save final data ----

data_dictionary <- generate_dictionary(marathon, details = "full")
save(marathon, data_dictionary, file = here("data", "derived", "marathon.RData"))
