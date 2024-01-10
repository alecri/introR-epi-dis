# analyses for marathon data

# libraries & setting ----

library(tidyverse)
library(gtsummary)
library(flextable)
library(officer)
library(scales)
library(gam)
theme_set(theme_minimal() + theme(legend.position = "bottom"))


# load derived data ----
load("data/derived/marathon.RData")


# analyses ----

## BMI predictor ----
fit_bmicat <- glm(nas135 ~ bmi_cat, data = marathon, family = "binomial")

## weight change predictor ----

fit_wc_lin <- glm(nas135 ~ wtdiff, data = marathon, family = "binomial")
fit_wc_sq <- glm(nas135 ~ wtdiff + I(wtdiff^2) , data = marathon, family = "binomial")
fit_wc_gam <- gam(nas135 ~ s(wtdiff) , data = marathon, family = "binomial")

# table 1 ----

tab1 <- select(marathon, sex, age, prewt, bmi, trainpace, prevmara, 
               waterload, nsaid, runtime) %>% 
  #mutate(trainpace = seconds_to_period(trainpace)) %>% 
  tbl_summary(
    by = sex, missing = "no",
    statistic = list(all_continuous() ~ "{mean} ± {sd}",
                     prevmara ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)")
  ) %>% 
  modify_footnote(all_stat_cols() ~ NA) %>% 
  add_p() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Runners**")
tab1

# save tab1 in output folder
tab1 %>% 
  as_flex_table() %>% 
  save_as_docx(path = "output/tables/tab1.docx")


# figure 1 ----

summary(marathon$na)
hypo_severity <- marathon %>% 
  filter(!is.na(wtdiff_cat)) %>%
  arrange(desc(wtdiff_cat)) %>% 
  mutate(wtdiff_cat = fct_inorder(wtdiff_cat)) %>% 
  group_by(wtdiff_cat) %>% 
  summarise(
    n = n(),
    risk_na135 = mean(na <= 135 & na > 130), 
    risk_na130 = mean(na <= 130)
  ) %>% 
  pivot_longer(cols = -c(wtdiff_cat, n), names_to = "severity", values_to = "risk") %>% 
  mutate(
    n = replace(n, severity == "risk_na130", NA),
    severity = factor(severity, levels = c("risk_na135", "risk_na130"),
                      labels = c("Hyponatremia", "Severe hyponatremia"))
  )

p_fig1 <- ggplot(hypo_severity, aes(wtdiff_cat, y = risk, fill = severity)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), position = position_stack(vjust = 1.05)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Weight Change Categories (kg)", y = "Risk of Hyponatremia (%)", fill = "Severity")
p_fig1

# save figure in output folder
ggsave(filename = "output/figures/fig1.pdf", p_fig1, height = 6, width = 8)



# figure 2 -----

pred_wc <- data.frame(wtdiff = seq(-4, 3, by = .01))
pred_wc <- pred_wc %>% 
  mutate(
    linear = predict(fit_wc_lin, newdata = pred_wc),
    quadratic = predict(fit_wc_sq, newdata = pred_wc),
    additive = predict(fit_wc_gam, newdata = pred_wc)
  ) %>% 
  pivot_longer(cols = -wtdiff, values_to = "logodds", names_to = "model") %>% 
  group_by(model) %>% 
  mutate(or = exp(logodds - logodds[wtdiff == 0]))

p_fig2a <- ggplot(pred_wc, aes(wtdiff, or)) + 
  geom_line(aes(linetype = model)) +
  geom_segment(x = 0, y = 0, xend = 0, yend = -5, size = .2) + 
  geom_rug(data = filter(marathon, na <= 135), aes(y = NA), sides = "b", length = unit(0.01, "npc")) +
  geom_rug(data = filter(marathon, na > 135), aes(y = NA), sides = "t", length = unit(0.01, "npc")) +  
  scale_y_continuous(trans = "log", breaks = c(.125, .25, 0.5, 1, 2, 4, 8)) +
  labs(x = "Weight Change (kg)", y = "Odds Ratio", linetype = "Model") +
  xlim(range(pred_wc$wtdiff))
p_fig2a

ggsave(filename = "output/figures/fig2a.pdf", p_fig2a, height = 6, width = 8)


# table 2 ----

tab2_part1 <- tbl_summary(marathon, by = nas135, missing = "no", include = bmi_cat) %>% 
  modify_footnote(all_stat_cols() ~ NA) %>% 
  add_p() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Runners**")
tab2_part2 <- tbl_regression(fit_bmicat, exponentiate = TRUE) %>%
  add_global_p()
tab2 <- tbl_merge(list(tab2_part1, tab2_part2),
                  tab_spanner = c("**Univariate Predictors**", "**Multivariate Predictors**"))

tab2 %>% 
  as_flex_table() %>% 
  save_as_docx(path = "output/tables/tab2.docx",
               pr_section = prop_section(
                 page_size = page_size(orient = "landscape", width = 12, height = 9)
               ))
