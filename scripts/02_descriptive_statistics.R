# descriptive analyses for marathon data

# libraries & setting ----

library(tidyverse)
library(here)
library(scales)
library(broom)
library(Epi)
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
  geom_jitter(size = 1) +
  labs(x = "", y = "Serum sodium concentration (mmol/liter)")

c("Mean" = mean(marathon$na), "SD" = sd(marathon$na),
  "Median" = median(marathon$na), "IQR" = IQR(marathon$na),
  "Min" = min(marathon$na), "Max" = max(marathon$na),
  "25th percentile" = quantile(marathon$na, .25),
  "75th percentile" = quantile(marathon$na, .75))
ggplot(marathon, aes(na)) + 
  geom_histogram(aes(y = ..density..), bins = 20) +
  geom_line(stat = "density", col = "blue", size = 2) +
  labs(x = "Serum sodium concentration (mmol/liter)", y = "density")


## cateegorical variables ----

tab_nas135 <- table(marathon$nas135)
tab_nas135
prop.table(tab_nas135)
dtab_nas135 <- count(marathon, nas135) %>% 
  mutate(
    prop = n/sum(n),
    percentage = scales::percent(prop)
  )
dtab_nas135

ggplot(marathon, aes(nas135, fill = nas135)) +
  geom_bar() +
  labs(x = "Serum sodium concentration <= 135 mmol/liter") +
  guides(fill = "none")
ggplot(dtab_nas135, aes(x = "", y = prop, fill = nas135)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(label = percentage), position = position_stack(vjust = 0.5),
             show.legend = FALSE, label.size = NA) +
  coord_polar(theta = "y") +
  labs(fill = "Hyponatremia") +
  theme_void()



# bivariate ----

## continuous & categorical ----

by(marathon$na, marathon$sex, summary)
group_by(marathon, sex) %>% 
  summarise(n_nomiss = sum(!is.na(na)), 
            mean = mean(na), 
            median = median(na),
            sd = sd(na), 
            iqr = IQR(na), 
            min = min(na), 
            max = max(na),
            p25 = quantile(na, .25),
            p75 = quantile(na, .75))
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

tab_nabmi <- table(nas135 = marathon$nas135, sex = marathon$bmi_cat)
stat.table(list(nas135, bmi_cat), 
           list(count(), percent(nas135)), 
           marathon, margin = T)

group_by(marathon, bmi_cat) %>%
  filter(!is.na(bmi_cat)) %>% 
  count(nas135) %>%
  mutate(perc = n/sum(n)) %>%
  ggplot(aes(x = nas135, y = perc, fill = bmi_cat)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "", y = "Frequency", fill = "Categories of BMI")

# test for independence 
summary(tab_nabmi)
# measure of associations
twoby2(exposure = relevel(marathon$bmi_cat, 2), outcome = relevel(marathon$nas135, 2))
# equivalent logistic model
fit_nabmi <- glm(nas135 ~ relevel(bmi_cat, 2), data = marathon, family = "binomial")
summary(fit_nabmi)
tidy(fit_nabmi, conf.int = TRUE, exponentiate = TRUE)


## 2 continuous variables ----

with(marathon, cor(na, wtdiff,  method = "pearson", use = "complete.obs"))
p_nawtdiff <- ggplot(marathon, aes(wtdiff, na)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Weight change (kg) pre/post race", y = "Serum sodium concentration (mmol/liter)")
ggMarginal(p_nawtdiff, type = "histogram")



# multivariate analysis ----

# logistic regression model
fit_multi <- glm(nas135 ~ bmi_cat + sex + wtdiff + wtdiff^2, 
                 data = marathon, family = "binomial")
summary(fit_multi)

tidy(fit_multi, conf.int = TRUE, exponentiate = TRUE)

marathon %>% 
  mutate(p_pred = predict(fit_multi, newdata = marathon, type = "response")) %>%
  filter(!is.na(bmi_cat)) %>% 
  ggplot(aes(wtdiff, p_pred, col = sex, linetype = bmi_cat)) +
  geom_line() +
  labs(x = "Weight change, kg", y = "Predicted probability", col = "Sex") +
  facet_grid(bmi_cat ~ .)


