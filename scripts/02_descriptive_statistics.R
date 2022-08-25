# descriptive analyses for marathon data

# libraries & setting ----

library(tidyverse)
library(here)
library(scales)
library(broom)
library(Epi)
library(descr)
library(ggExtra)
theme_set(theme_minimal() + theme(legend.position = "bottom"))


# load derived data ----
load(here("data", "derived", "marathon.RData"))


# univariate ----

## continuous variables ----

summary(marathon$na)
ggplot(marathon, aes(na)) + 
  geom_histogram() +
  labs(x = "Serum sodium concentration (mmol/liter)")
ggplot(marathon, aes(na)) + 
  geom_boxplot() + 
  coord_flip() +
  labs(x = "Serum sodium concentration (mmol/liter)")
ggplot(marathon, aes(x = "", y = na)) + 
  geom_violin(fill = "lightblue") + 
  geom_jitter() +
  labs(x = "", y = "Serum sodium concentration (mmol/liter)")

c(Mean = mean(marathon$na), SD = sd(marathon$na),
  Median = median(marathon$na), IQR = IQR(marathon$na),
  Min = min(marathon$na), Max = max(marathon$na))
ggplot(marathon, aes(na)) + 
  geom_histogram(aes(y = ..density..), bins = 20) +
  geom_line(stat = "density", col = "blue", size = 2) +
  labs(x = "Serum sodium concentration (mmol/liter)", y = "density")


## cateegorical variables ----

tab_wtd <- table(marathon$wtdiff_cat)
tab_wtd
prop.table(tab_wtd)
dtab_wtd <- count(marathon, wtdiff_cat) %>% 
  filter(!is.na(wtdiff_cat)) %>% 
  mutate(
    prop = n/sum(n),
    percentage = scales::percent(prop)
  )
dtab_wtd

ggplot(filter(marathon, !is.na(wtdiff_cat)), aes(wtdiff_cat, fill = wtdiff_cat)) +
  geom_bar() +
  labs(x = "Serum sodium concentration <= 135 mmol/liter") +
  guides(fill = "none")
ggplot(dtab_wtd, aes(x = "", y = prop, fill = wtdiff_cat)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(label = percentage), position = position_stack(vjust = 0.5),
             show.legend = FALSE, label.size = NA) +
  coord_polar(theta = "y") +
  labs(fill = "Categories of weight change") +
  theme_void()


# bivariate ----

## continuous & categorical ----

by(marathon$na, marathon$sex, summary)
group_by(marathon, sex) %>% 
  summarise(n_nomiss = sum(!is.na(na)), mean = mean(na), median = median(na),
            sd = sd(na), iqr = IQR(na), min = min(na), max = max(na))
ggplot(marathon, aes(sex, na)) +
  geom_boxplot() + 
  labs(x = "", y = "Serum sodium concentration (mmol/liter)")
ggplot(marathon, aes(na, col = sex)) +
  geom_line(stat = "density") + 
  labs(x = "Serum sodium concentration (mmol/liter)", y = "Density")
# t test for differences in means
test_nasex <- t.test(na ~ sex, data = marathon, var.equal = TRUE)
test_nasex
broom::tidy(test_nasex)
# equivalent linear model
lm_nasex <- lm(na ~ sex, data = marathon)
summary(lm_nasex)
tidy(lm_nasex, conf.int = TRUE)


## 2x2 contingency tables ----

tab_nasex <- table(nas135 = marathon$nas135, sex = marathon$sex)
stat.table(list(nas135, sex), 
           list(count(), percent(sex)), 
           marathon, margin = T)

group_by(marathon, sex) %>%
  count(nas135) %>%
  mutate(perc = n/sum(n)) %>%
  ggplot(aes(x = nas135, y = perc, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "", y = "Frequency")

# test for independence 
CrossTable(tab_nasex, prop.r = FALSE, prop.c = FALSE, 
           prop.t = FALSE, prop.chisq = FALSE, expected = TRUE, chisq = TRUE)
# measure of associations
twoby2(exposure = relevel(marathon$sex, 2), outcome = relevel(marathon$nas135, 2))
# equivalent logistic model
fit_nasex <- glm(nas135 ~ sex, data = marathon, family = "binomial")
summary(fit_nasex)
tidy(fit_nasex, conf.int = TRUE, exponentiate = TRUE)


## 2 continuous variables ----

with(marathon, cor(na, wtdiff,  method = "pearson", use = "complete.obs"))
p_nawtdiff <- ggplot(marathon, aes(wtdiff, na)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Weight change (kg) pre/post race", y = "Serum sodium concentration (mmol/liter)")
ggMarginal(p_nawtdiff, type = "histogram")
