# data management for marathon data

# libraries ----

library(tidyverse)
library(labelled)
library(skimr)
library(gt)
library(Hmisc)


# read raw data ----
marathon_raw <- read.csv("data/raw/marathon_raw.csv")

# labeling & coding ----

#View(marathon_raw)        # open in a data viewer
head(marathon_raw, 10)    # display first 10 raw
glimpse(marathon_raw)     # get a gimplse of the data
names(marathon_raw)       # get the columns names
dim(marathon_raw)         # how many raws/variables

## set variable labels ----
marathon <- set_variable_labels(
  marathon_raw,
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

# data management ----

## set labels to character (factor) variable ----
marathon <- mutate(
  marathon,
  sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
  urinat3p = factor(urinat3p, levels = c(1, 2), labels = c("<3", "≥3")),
  fluidint = factor(fluidint, levels = c(1, 2, 3),
                    labels = c("Every mile", "Every other mile", "Every third mile or less often")),
  waterload = factor(waterload, levels = c(1, 2), labels = c("No", "Yes")),
  nsaid = factor(nsaid, levels = c(1, 2), labels = c("No", "Yes"))
) %>% 
  copy_labels_from(marathon)
skim(marathon)
look_for(marathon)

## create and modify variables ----
marathon <- marathon %>% 
  mutate(
    runtime = runtime/60,
    wtdiff = postwt - prewt,
    bmi = prewt/(height^2),
    bmi_cat = cut(bmi, breaks = c(15, 20, 25, 33), labels = c("< 20", "20-25", ">25")),
    wtdiff_cat = cut(wtdiff, breaks = c(-5, -2, -1, 0, 1, 2, 3, 5), 
                     include.lowest = T, right = FALSE, ordered_result = TRUE),
    age_cat = cut2(age, g = 5, digits = 1),
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


# exploration ----

## select variables ----
names(marathon)
marathon_sub1 <- select(marathon, sex, runtime, ends_with("wt"), bmi, na)
head(marathon_sub1)

## filter observations ----
women_sub2 <- filter(marathon_sub1, sex == "Female", runtime >= 4, bmi > 25)
women_sub2

## sorting ----
arrange(women_sub2, desc(na), runtime)

## chaining ----
marathon %>% 
  mutate(
    # create and modify variables
  ) %>% 
  select(sex, runtime, ends_with("wt"), bmi, na) %>%
  filter(sex == "Female", runtime >= 4, bmi > 25) %>%
  arrange(desc(na), runtime)


## advanced data management ----

### pivot longer
weight_long <- marathon %>% 
  select(id, ends_with("wt"), sex) %>% 
  mutate(meanwt = .5*(prewt + postwt)) %>% 
  pivot_longer(cols = ends_with("wt"), names_to = "measure", values_to = "weight") %>% 
  mutate(
    measure = gsub("wt", "", measure),
    measure = fct_inorder(measure)
  )
tail(weight_long)

ggplot(weight_long, aes(measure, weight)) +
  geom_boxplot() +
  facet_wrap(~ sex)
group_by(weight_long, sex, measure) %>% 
  summarise(mean = mean(weight, na.rm = TRUE))


### pivot wider
weight_long %>% 
  pivot_wider(id_cols = c(id, sex), names_from = measure, values_from = weight) %>% 
  sample_n(size = 10)


# save final data ----

data_dictionary <- generate_dictionary(marathon, details = "full")
save(marathon, data_dictionary, file = "data/derived/marathon.RData")
