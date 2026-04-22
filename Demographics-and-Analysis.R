
# Running the script the contains the cleaned data and needed libraries 
source("Data-Cleaning.R")

# Running Needed Libraries 
library(patchwork)
library(broom)
library(glue)

# Creating a function that tests for normality
# The function output is the needed diagnostic plots for normality and applying shapiro wilk test 
Normality_Testing <- function(data, x, y) {
  p1 <- data %>% ggplot(aes(x = {{x}})) + geom_histogram() + facet_wrap(vars({{y}}))
  p2 <- data %>% ggplot(aes(x = {{x}})) + geom_density() + facet_wrap(vars({{y}}))
  p3 <- data %>% ggplot(aes(sample = {{x}})) + geom_qq() + geom_qq_line() + facet_wrap(vars({{y}}))
  print(p1/p2/p3)
  data %>% group_by({{y}}) %>% shapiro_test({{x}})
}

# Applying the function on needed variables 
All_Data %>% Normality_Testing(age, Group)
All_Data %>% Normality_Testing(BMI, Group)

# Creating a demographics summary table with calculated p-value between the two groups to check for Randomization
All_Data %>% select(gender:`Marital Status`, -Height, -weight, Group, `RMDQ Before`, `ST Before`, `OSW Before`,
                    `VAS-Rest Before`, `VAS-Activity Before`) %>%
  tbl_summary(by = Group,
              label = list(
                gender ~ "Gender",
                age ~ "Age",
                `ST Before` ~ "Schober Test",
                `RMDQ Before` ~ "Roland Morris Disability Questionnaire",
                `OSW Before` ~ "Oswestry Disability Index",
                `VAS-Rest Before`~ "VAS Rest",
                `VAS-Activity Before` ~ "VAS Activity"
              ),
              type = list(
                `ST Before` ~ "continuous",
                `VAS-Rest Before` ~ "continuous",
                `VAS-Activity Before` ~ "continuous"
              ),
              digits = list(
                `RMDQ Before` ~ 1,
                `ST Before` ~ 0,
                `VAS-Rest Before` ~1,
                `VAS-Activity Before` ~0
              )) %>% add_overall() %>% add_p() %>% bold_labels()


# Doing before and after wilcoxin signed-rank test for paired data with correction and presenting it in a table  
All_Data %>% select(ID, Group, `RMDQ Before`, `RMDQ After`, `ST Before`, `ST After`,
                    `OSW After`, `OSW Before`, `VAS-Rest After`, `VAS-Rest Before`,
                    `VAS-Activity After`, `VAS-Activity Before`) %>% 
  pivot_longer(cols = -c(ID, Group),
               names_to = c("Score", "Time"),
               names_sep = " ",
               values_to = "Value") %>% 
  mutate(Time = factor(Time, levels = c("Before", "After"))) %>% 
  pivot_wider(names_from = Score, values_from = Value) %>%  
  tbl_strata(strata = Group, .tbl_fun = ~ .x %>%  
               tbl_summary(by = Time,
                           include = c(RMDQ, ST, OSW, `VAS-Rest`, `VAS-Activity`),
                           type = list(ST ~ "continuous",
                                       `VAS-Rest` ~ "continuous",
                                       `VAS-Activity` ~ "continuous"
                           ),
                           digits = list(
                             ST ~ 1,
                             `VAS-Rest` ~ 1,
                             `VAS-Activity` ~1
                           )) %>% 
               add_p(test = everything() ~ "paired.wilcox.test", group = ID) %>% 
               add_q(method = "bonferroni")) %>% 
  modify_column_hide(starts_with("p.value")) %>%
  modify_header(starts_with("q.value") ~ "**p-value**") %>%
  modify_table_styling(
    columns = starts_with("q.value"),
    footnote = "Wilcoxon signed rank test with Bonferroni correction for multiple testing"
  ) %>% 
  as_flex_table()


# Creating a data frame that contains only needed measures for outcome with group and ID 
Effect_Size_Data <- All_Data %>% 
  select(ID, Group, `RMDQ After`, `RMDQ Before`, `ST After`, `ST Before`,
         `OSW After`, `OSW Before`, 
         `VAS-Activity After`, `VAS-Activity Before`, `VAS-Rest After`, `VAS-Rest Before`) %>% 
  mutate(`RMDQ Change` = `RMDQ After` - `RMDQ Before`,
         `ST Change` = `ST After` - `ST Before`,
         `OSW Change` = `OSW After` - `OSW Before`,
         `VAS Activity Change` = `VAS-Activity After` - `VAS-Activity Before`,
         `VAS Rest Change` = `VAS-Rest After` - `VAS-Rest Before`)


# Defining a function for pairwise Mann whitny tests or wilcoxin for applying post hoc analysis with correction
gts_pairwise_wilcox <- function(data, variable, by, ...) {
  pw <- pairwise.wilcox.test(data[[variable]], data[[by]], p.adj = "bonferroni")
  
# Convert the test results tidy data format and selecting needed columns and reshaping it to wide format 
  pw_tidy <- broom::tidy(pw) %>%
    mutate(label = glue("**{group2} vs. {group1}**")) %>%
    select(label, p.value) %>%
    spread(label, p.value)
  
  return(pw_tidy)
}

# Creating a table with result of krsukal-wallis which is the non parametric equivalent to anova between groups
# Adding our pairwise post hoc results by add-stat and putting the function we made
Effect_Size_Data %>% select(contains("Change"), Group) %>% 
  tbl_summary(by = Group,
              type = `ST Change` ~ "continuous") %>% add_p() %>% 
  add_q(method = "bonferroni") %>% modify_header(starts_with("q.value") ~ "**p-value**") %>% 
  modify_column_hide(starts_with("p.value")) %>% modify_table_styling(
    columns = starts_with("q.value"),
    footnote = "Kruskal-Wallis rank sum test with Bonferroni correction for multiple testing"
  ) %>%  add_stat(fns = everything() ~ gts_pairwise_wilcox) %>%
  modify_fmt_fun(starts_with("**") ~ style_pvalue) %>% bold_labels()


# Creating a median line plot showing results of before and after with error bars in different clinical tests
# Scales are made to free in faceting as each clinical test has its own numbers 
All_Data %>%
  select(ID, Group,
         `RMDQ Before`, `RMDQ After`,
         `ST Before`, `ST After`,
         `OSW Before`, `OSW After`,
         `VAS-Rest Before`, `VAS-Rest After`,
         `VAS-Activity Before`, `VAS-Activity After`) %>%
  pivot_longer(
    cols = -c(ID, Group),
    names_to = c("Outcome", "Time"),
    names_sep = " ",
    values_to = "Value"
  ) %>%
  mutate(
    Time = factor(Time, levels = c("Before", "After")),
    Outcome = factor(Outcome)
  )  %>%
  group_by(Group, Outcome, Time) %>%
  summarise(
    med = median(Value, na.rm = TRUE),
    q1  = quantile(Value, 0.25, na.rm = TRUE),
    q3  = quantile(Value, 0.75, na.rm = TRUE)) %>% ungroup() %>% 
  ggplot(aes(x = Time, y = med, color = Group, group = Group)) + scale_color_manual(
    values = c("#C1121F", "#003049", "#669BBC")) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.10, linewidth = 0.6) +
  facet_wrap(~ Outcome, scales = "free",ncol = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(face = "bold")
  )



# Creating a bar graph with the results of different clinincal measures in different groups before and after 
All_Data %>% select(`RMDQ After`, `RMDQ Before`, `ST After`, `ST Before`,
                    `VAS-Activity After`, `VAS-Activity Before`, `VAS-Rest After`,
                    `VAS-Rest Before`, `OSW After`, `OSW Before`, Group) %>% 
  pivot_longer(cols = c(`RMDQ After`, `RMDQ Before`, `ST After`, `ST Before`,
                        `VAS-Activity After`, `VAS-Activity Before`, `VAS-Rest After`,
                        `VAS-Rest Before`, `OSW After`, `OSW Before`),
               names_to = "Score",
               values_to = "Value") %>% group_by(Group, Score) %>% 
  summarise(Median = median(Value)) %>% 
  mutate(Score = factor(Score, levels = c("RMDQ Before", "RMDQ After", "ST Before", "ST After",
                                          "VAS-Activity Before", "VAS-Activity After", "VAS-Rest Before",
                                          "VAS-Rest After", "OSW Before", "OSW After"))) %>% 
  ggplot(aes(Score, Median, fill = Group)) +
  geom_col(position = position_dodge(width = 0.5)) + scale_fill_manual(
    values = c("#C1121F", "#003049", "#669BBC")) + 
  theme_minimal() + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", face = "bold"))

