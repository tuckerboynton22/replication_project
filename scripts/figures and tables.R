## The script below is used to generate the tables and figures you see in the
# final PDF created by the R Markdown file.
rm(list=ls())
if (!require(foreign)) install.packages("foreign")
if (!require(ggtext)) install.packages("ggtext")
if (!require(readxl)) install.packages("readxl")
if (!require(ggrepel)) install.packages("ggrepel")
if (!require(stargazer)) install.packages("stargazer")
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(gt)) install.packages("gt"); library(gt)

frontier201 <- foreign::read.dta("data/frontier201.dta")
lewis205 <- foreign::read.dta("data/lewis205b.dta")
teamaggs <- readxl::read_excel("data/teamaggs.xlsx")
playerseasons <- readxl::read_excel("data/playerseasons.xlsx")

## Run regressions 1-3
model1 <- lm(wpct ~ obp + obpagnst + factor(year), data = teamaggs)
model2 <- lm(wpct ~ slg + slgagnst + factor(year), data = teamaggs)
model3 <- lm(wpct ~ slg + slgagnst + obp + obpagnst + factor(year), data = teamaggs)

## Add new variables for OBP & SLG differentials
teamaggs <- teamaggs %>%
  mutate(obpdiff = obp - obpagnst,
         slgdiff = slg - slgagnst)

## Run regression 4
model4 <- lm(wpct ~ slgdiff + obpdiff + factor(year), data = teamaggs)

## Display regression results
stargazer::stargazer(model1, model2, model3, model4, type = "latex", header = FALSE, font.size = "small",
                     star.cutoffs = NA, omit = "year", omit.stat = c("adj.rsq", "f", "ser"),
                     title = "The Impact of On-Base and Slugging Percentage on Winning", df = FALSE,
                     dep.var.caption = "Model", dep.var.labels.include = FALSE, intercept.bottom = FALSE,
                     covariate.labels = c("Constant",
                                          "On-Base",
                                          "On-Base against",
                                          "Slugging",
                                          "Slugging against",
                                          "On-Base diff",
                                          "Slugging diff"),
                     notes = c("Note: Aggregate statistics for teams from 1999-2003.",
                               "Models include year indicators. Dependent variable is win pct."),
                     notes.label = "",
                     notes.align = "l",
                     notes.append = FALSE)

## Create data frame of summary stats of yearly salaries
yearly_salaries <- lewis205 %>%
  filter(salyear >= 2000, salyear <= 2004, pa > 130) %>%
  select(salyear, salary) %>%
  group_by(salyear) %>%
  summarize(
    N = n(),
    Mean = mean(salary) / 10^6,
    `10th percentile` = quantile(salary, 0.10) / 10^6,
    Median = median(salary) / 10^6,
    `90th percentile` = quantile(salary, 0.90) / 10^6
  ) %>%
  select(-salyear) %>%
  t() %>%
  as.data.frame()

colnames(yearly_salaries) <- c(2000, 2001, 2002, 2003, 2004)

## Display df of yearly salary summary stats
yearly_salaries %>%
  gt(rownames_to_stub = T) %>%
  tab_header(
    title = "Table 2: Summary Statistics",
    subtitle = "Player salaries (in millions)"
  ) %>%
  cols_align(align = "center", columns = c(2:6)) %>%
  fmt_number(columns = 2:6, rows = 2:5, decimals = 2)

## Create data frame of means of positional salaries
positional_salaries <- lewis205 %>%
  filter(salyear >= 2000, salyear <= 2004, pa > 130) %>%
  mutate(pos = case_when(
    pripos == "SS" | pripos == "2B" | pripos == "3B" ~ "Infielders",
    pripos == "OF" ~ "Outfielders",
    pripos == "C" ~ "Catchers",
    pripos == "1B" | pripos == "DH" ~ "First basemen/DH"
  )) %>%
  select(pos, salary, salyear) %>%
  group_by(pos, salyear) %>%
  summarize(
    N = n(),
    Mean = mean(salary) / 10^6
  ) %>%
  as_latex()

## Create data frame of means of power/non-power hitter salaries
power_salaries <- lewis205 %>%
  filter(salyear >= 2000, salyear <= 2004, pa > 130) %>%
  mutate(pos = case_when(
    hr > 25 ~ "HR > 25",
    hr < 14 ~ "HR < 14"
  )) %>%
  filter(!is.na(pos)) %>%
  select(pos, salary, salyear) %>%
  group_by(pos, salyear) %>%
  summarize(
    N = n(),
    Mean = mean(salary) / 10^6
  )

## Bind positional/power hitter data together and rearrange using indicators
overall_salaries <- rbind(power_salaries, positional_salaries) %>%
  mutate(ind0 = ifelse(salyear == 2000, 1, 0),
         ind1 = ifelse(salyear == 2001, 1, 0),
         ind2 = ifelse(salyear == 2002, 1, 0),
         ind3 = ifelse(salyear == 2003, 1, 0),
         ind4 = ifelse(salyear == 2004, 1, 0)) %>%
  group_by(pos) %>%
  summarize(
    mean0 = sum(Mean*ind0),
    n0 = sum(N*ind0),
    mean1 = sum(Mean*ind1),
    n1 = sum(N*ind1),
    mean2 = sum(Mean*ind2),
    n2 = sum(N*ind2),
    mean3 = sum(Mean*ind3),
    n3 = sum(N*ind3),
    mean4 = sum(Mean*ind4),
    n4 = sum(N*ind4)
  )

## Display positional/power hitter averages
overall_salaries %>%
  gt() %>%
  cols_label(
    mean0 = "Mean",
    mean1 = "Mean",
    mean2 = "Mean",
    mean3 = "Mean",
    mean4 = "Mean",
    n0 = "N",
    n1 = "N",
    n2 = "N",
    n3 = "N",
    n4 = "N",
    pos = ""
  ) %>%
  tab_spanner(label = "2000", columns = 2:3) %>%
  tab_spanner(label = "2001", columns = 4:5) %>%
  tab_spanner(label = "2002", columns = 6:7) %>%
  tab_spanner(label = "2003", columns = 8:9) %>%
  tab_spanner(label = "2004", columns = 10:11) %>%
  fmt_number(columns = c(2,4,6,8,10), decimals = 2) %>%
  cols_align(align = "center", columns = c(2:11)) %>%
  tab_source_note("Note: Summary statistics for salaries of players with more than 130 plate appearances.") %>%
  as_latex()

## Filter to include only players with 130+ plate appearances
players <- subset(playerseasons, playerseasons$pa >= 130)

## Run models
salary_model1 <- lm(lnsal ~ obp + slg + pa + arbit + free + catch + inf + factor(year), data = players)
salary_model2 <- lm(lnsal ~ obp + slg + pa + arbit + free + catch + inf + factor(year), data = subset(players, players$year < 2003))
salary_model3 <- lm(lnsal ~ obp + slg + pa + arbit + free + catch + inf, data = subset(players, players$year == 1999))
salary_model4 <- lm(lnsal ~ obp + slg + pa + arbit + free + catch + inf, data = subset(players, players$year == 2000))
salary_model5 <- lm(lnsal ~ obp + slg + pa + arbit + free + catch + inf, data = subset(players, players$year == 2001))
salary_model6 <- lm(lnsal ~ obp + slg + pa + arbit + free + catch + inf, data = subset(players, players$year == 2002))
salary_model7 <- lm(lnsal ~ obp + slg + pa + arbit + free + catch + inf, data = subset(players, players$year == 2003))

## Assign years and run for loop to get salary predictions for 1 SD from mean
years <- c(1999, 2000, 2001, 2002, 2003)

for(i in 1:5){
  obp2 <- mean(players$obp[players$year == years[i]], na.rm = T)+sd(players$obp[players$year == years[i]], na.rm = T)
  obp1 <- mean(players$obp[players$year == years[i]], na.rm = T)
  assign(paste0("ob_", i), round(exp(get(paste0("salary_model", i+2))$coefficients[[2]]*obp2)-exp(get(paste0("salary_model", i+2))$coefficients[[2]]*obp1), 2))
  slg2 <- mean(players$slg[players$year == years[i]], na.rm = T)+sd(players$slg[players$year == years[i]], na.rm = T)
  slg1 <- mean(players$slg[players$year == years[i]], na.rm = T)
  assign(paste0("slg_", i), round(exp(get(paste0("salary_model", i+2))$coefficients[[3]]*slg2)-exp(get(paste0("salary_model", i+2))$coefficients[[3]]*slg1), 2))
}

## Display results & 1 SD deviation predictions
stargazer::stargazer(salary_model1, salary_model2, salary_model3, salary_model4, salary_model5, salary_model6, salary_model7,
                     type = "latex", header = FALSE, font.size = "small", omit.stat = c("adj.rsq", "f", "ser"),
                     column.sep.width = "0pt", star.cutoffs = NA, omit = "year",
                     dep.var.labels.include = FALSE, dep.var.caption = "", df = FALSE, model.numbers = FALSE,
                     title = "Table 3: The Baseball Labor Market's Valuation of On-Base and Slugging Percentage",
                     covariate.labels = c("On-Base",
                                          "Slugging",
                                          "Plate appearances",
                                          "Arbitration eligible",
                                          "Free agency",
                                          "Catcher dummy",
                                          "Infielder dummy"),
                     column.labels = c("All years",
                                       "2000-2003",
                                       "2000",
                                       "2001",
                                       "2002",
                                       "2003",
                                       "2004"),
                     add.lines = list(c("Value of 1 SD increase (millions)"),
                                      c("On-Base",  "", "", ob_1, ob_2, ob_3, ob_4, ob_5),
                                      c("Slugging", "", "", slg_1, slg_2, slg_3, slg_4, slg_5)),
                     table.layout = "=c-!t-s-a=n",
                     notes = c("Note: Dependent variable is ln(salary) in year t. Performance variables in year t-1.",
                               "First two models include year indicators. Includes players with 130+ plate appearances.",
                               "Final two rows show value of 1 standard deviation increase from average.",
                               "Standard errors in parentheses."),
                     notes.label = "",
                     notes.align = "l",
                     notes.append = FALSE)

## Store coefficients on OBP and assign type = 'On base %'
obp_coefs <- c(salary_model3$coefficients[2], salary_model4$coefficients[2],
               salary_model5$coefficients[2], salary_model6$coefficients[2], salary_model7$coefficients[2])
type_obp <- c(rep("On base %", times = 5))
obp_coefs <- cbind(obp_coefs, type_obp)

## Store coefficients on SLG and assign type = 'Slugging %'
slg_coefs <- c(salary_model3$coefficients[3], salary_model4$coefficients[3],
               salary_model5$coefficients[3], salary_model6$coefficients[3], salary_model7$coefficients[3])
type_slg <- c(rep("Slugging %", times = 5))
slg_coefs <- cbind(slg_coefs, type_slg)

## Bind two coefficient dfs together and add years
coefs <- rbind(slg_coefs, obp_coefs)
years <- c(2000, 2001, 2002, 2003, 2004)
coefs <- as.data.frame(cbind(years, coefs))
coefs$slg_coefs <- as.numeric(coefs$slg_coefs)

## Plot coefficients
coefs %>%
  ggplot(aes(years, slg_coefs, group = type_slg)) +
  geom_point(aes(shape = type_slg)) +
  geom_path(aes(linetype = type_slg)) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Impact on log salary",
    title = "Labor Market Returns to On-Base and Slugging Percentage Over Time"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = ggtext::element_markdown(size = 10, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 0.5))

frontier201 <- frontier201 %>%
  mutate(salary = case_when(
    !is.na(pay8693) ~ pay8693,
    !is.na(pay9497) ~ pay9497,
    !is.na(pay9803) ~ pay9803,
    !is.na(pay0406) ~ pay0406
  ))

frontier201 %>%
  ggplot(aes(wpct, salary)) +
  geom_point(shape = "diamond") +
  ggrepel::geom_text_repel(aes(label = ifelse(convex == 1, team, ""))) +
  theme_bw() +
  labs(
    x = "Team winning percentage",
    y = "Salary index",
    title = "Frontier for Efficient Conversion of Team Salary into Team Winning Percentage",
    subtitle = "Various teams, 1986 to 2006"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = ggtext::element_markdown(size = 12, hjust = 0.5),
    plot.subtitle = ggtext::element_markdown(hjust = 0.5))

## Get coefficient ratio for each year
coef_ratios <- c()
true_ratio <- model4$coefficients[[3]]/model4$coefficients[[2]]

for(i in 3:7){
  new_ratio <- get(paste0("salary_model", i))$coefficients[2] / get(paste0("salary_model", i))$coefficients[3]
  coef_ratios <- append(coef_ratios, new_ratio)
}

coef_ratios <- as.data.frame(cbind(coef_ratios, years))

## Plot coefficient ratios
coef_ratios %>%
  ggplot(aes(years, coef_ratios)) +
  geom_point() +
  geom_path() +
  geom_hline(yintercept = true_ratio, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Year",
    y = "Coefficient on on-base / coefficient on slugging",
    title = "Ratio of Labor Market Returns to On-Base vs. Slugging Percentage Over Time"
  ) +
  annotate(geom = "text", x = 2000.5, y = 2.2, label = "Equilibrium ratio") +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = ggtext::element_markdown(size = 10, hjust = 0.5))