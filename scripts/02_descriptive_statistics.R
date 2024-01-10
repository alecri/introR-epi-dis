# descriptive analyses for marathon data

# libraries & setting ----

library(tidyverse)
library(scales)
library(broom)
library(Epi)
library(ggExtra)
library(arsenal)
library(gridExtra)
theme_set(theme_minimal() + theme(legend.position = "bottom"))


# load derived data ----
load("data/derived/marathon.RData")


# univariate ----

## continuous variables ----

summary(marathon$na)
# histogram (counts)
ggplot(marathon, aes(na)) + 
  geom_histogram() +
  labs(x = "Serum sodium concentration (mmol/liter)")
# histogram (density)
ggplot(marathon, aes(na)) +
  geom_histogram(stat = "density", n = 2^5) +
  geom_line(stat = "density", col = "blue", size = 1.5) +
  labs(x = "Sodium concentration, mmol per liter")

# boxplot
ggplot(marathon, aes(na)) + 
  geom_boxplot() + 
  coord_flip() +
  labs(x = "Serum sodium concentration (mmol/liter)")
# percentiles (or quantiles)
quantile(marathon$na)
# descriptives
c("Mean" = mean(marathon$na), "SD" = sd(marathon$na),
  "Median" = median(marathon$na), "IQR" = IQR(marathon$na),
  "Min" = min(marathon$na), "Max" = max(marathon$na),
  "25th percentile" = quantile(marathon$na, .25),
  "75th percentile" = quantile(marathon$na, .75))
summarise(marathon, mean = mean(na), sd = sd(na), median = median(na), iqr = IQR(na))

# violin plot
ggplot(marathon, aes(x = "", y = na)) + 
  geom_violin(fill = "lightblue") + 
  geom_jitter(size = 1) +
  labs(x = "", y = "Serum sodium concentration (mmol/liter)")

## categorical variables ----

tab_nas135 <- table(marathon$nas135)
tab_nas135
prop.table(tab_nas135)
# alternative
dtab_nas135 <- count(marathon, nas135) %>% 
  mutate(
    prop = n/sum(n),
    percentage = scales::percent(prop)
  )
dtab_nas135

# bar plot (counts)
ggplot(marathon, aes(nas135, fill = nas135)) +
  geom_bar() +
  labs(x = "Serum sodium concentration <= 135 mmol/liter") +
  guides(fill = "none")
# bar plot (proportion)
ggplot(marathon, aes(nas135, fill = nas135)) +
  geom_bar(aes(y = ..count../sum(..count..))) +
  guides(fill = "none") +
  labs(x = "", y = "Prevalence of hyponatremia") 
# pie chart
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
# alternative
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
# boxplot by levels
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
ci.lin(lm_nasex)
# non parametric test
wilcox.test(na ~ sex, data = marathon)


## 2x2 contingency tables ----

tab_nabmi <- table(bmi_cat = marathon$bmi_cat, nas135 = marathon$nas135)
tab_nabmi
prop.table(tab_nabmi, margin = 1)
# customizable tables
stat.table(list(nas135, bmi_cat), 
           list(count(), percent(nas135)), 
           marathon, margin = T)
# bar plot
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
twoby2(tab_nabmi[1:2, 2:1])
twoby2(tab_nabmi[3:2, 2:1])

# equivalent logistic model
fit_nabmi <- glm(nas135 ~ relevel(bmi_cat, 2), data = marathon, family = "binomial")
summary(fit_nabmi)
ci.exp(fit_nabmi)


## 2 continuous variables ----

# correlation coefficient
with(marathon, cor(na, wtdiff,  method = "pearson", use = "complete.obs"))
# scatter plot + regression line
p_nawtdiff <- ggplot(marathon, aes(wtdiff, na)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Weight change (kg) pre/post race", y = "Serum sodium concentration (mmol/liter)")
ggMarginal(p_nawtdiff, type = "histogram")


## Table 1 ----

tab1 <- tableby(nas135 ~ age + sex + wtdiff + bmi_cat + runtime, data = marathon)
summary(tab1)


# statistical models ----

## linear regression ----

# univariate linear regression
fit1 <- lm(na ~ sex, data = marathon)
summary(fit1)
tidy(fit1, conf.int = T)

# multivariable linear regression
fit2 <- lm(na ~ wtdiff + sex, data = marathon)
ci.lin(fit2)
marathon <- mutate(marathon,
                   pred_fit2 = predict(fit2, newdata = marathon))

# multivariable linear regression with interaction
fit3 <- lm(na ~ wtdiff*sex, data = marathon)
ci.lin(fit3, ctr.mat = rbind(c(0, 1, 0, 0),
                             c(0, 1, 0, 1)))
marathon <- mutate(marathon,
                   pred_fit3 = predict(fit3, newdata = marathon))

# graphical comparison
gridExtra::grid.arrange(
  ggplot(marathon, aes(wtdiff, pred_fit2, col = sex)) +
    geom_line() +
    labs(x = "Weight change, kg", y = "Predicted mean na"),
  ggplot(marathon, aes(wtdiff, pred_fit3, col = sex)) +
    geom_line() +
    labs(x = "Weight change, kg", y = "Predicted mean na"),
  ncol = 2
)


## logistic regression ----

# univariate logistic regression
fit4 <- glm(nas135 ~ wtdiff, data = marathon, family = binomial)
ci.exp(fit4)

marathon <- marathon %>% 
  mutate(pred_p = predict(fit4, newdata = marathon, type = "response"),
         pred_odds = predict(fit4, newdata = marathon, type = "link")) 

grid.arrange(
  ggplot(marathon, aes(wtdiff, pred_p)) +
    geom_line() +
    labs(x = "Weight change, kg", y = "Predicted probability"),
  ggplot(marathon, aes(wtdiff, pred_odds)) +
    geom_line() +
    labs(x = "Weight change, kg", y = "Predicted odds"),
  ncol = 2
)

# multivariable logistic regression
fit5 <- glm(nas135 ~ wtdiff + sex, data = marathon, family = binomial)
ci.exp(fit5)

mutate(marathon, pred = predict(fit5, newdata = marathon, type = "response")) %>% 
  ggplot(aes(wtdiff, pred, col = sex)) +
  geom_line() +
  labs(x = "Weight change, kg", y = "Predicted probability")
