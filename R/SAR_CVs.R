#
# Read in summary tables for marine mammal stock assessments.
#  Summarize CVs of abundance estimates.
#
# This 2014 summary table available from:
#  http://www.nmfs.noaa.gov/pr/sars/pdf/po2014_summarytable.pdf
#
library(magrittr)
library(dplyr)

# Read tables ------------------------------------------------------------------
setwd("./data") # working data directory

pacsar2014 = read.csv(file = "Pacific_summary_table_2014.csv", stringsAsFactors = FALSE)
atlsar2014 = read.csv(file = "Atlantic_summary_table_2014.csv", stringsAsFactors = FALSE)
aksar2014 = read.csv(file = "Alaska_summary_table_2014.csv", stringsAsFactors = FALSE)

# Extract CVs from each region and concontinate into single vector -------------
cv = c(aksar2014$CV_N, atlsar2014$CV_N, pacsar2014$CV_N)

# Edit entries that don't have reported CVs to be NA ---------------------------
na_ii = which(cv %in% c("", "N/A", "unk", "n/a"))
cv[na_ii] = NA
cv = cv[-na_ii]
cv = as.numeric(cv)

# Summary stats ----------------------------------------------------------------
summary(cv)

cv = tbl_df(data.frame(cv))
cv %<>% arrange(cv) %>% mutate(cumdist = cume_dist(cv)) # cumulative distribution of CV_N's
cv %<>% mutate(breaks = cut(cv, breaks = seq(0, max(cv) + 0.1, by = 0.1))) %>%
  mutate(cv_0.2 = ifelse(cv <= 0.2, FALSE, TRUE))
head(cv); nrow(cv)

# Percent of stocks with CV_N > 0.20
pct = cv %>% summarise(sum(cv_0.2)/length(cv)) %>% round(.,2)# percentage with CV_N <= 0.20
pct = round(pct*100)

# Plotting ---------------------------------------------------------------------
source("https://gist.githubusercontent.com/John-Brandon/484d152675507dd145fe/raw/2ecaf81c1b7948831efc1355823ec51e9cd5362b/mytheme_bw.R") # retrieve a custom theme for ggplotting (also loads 'ggplot2' package)

# Cumulative distribution of CV_N
ggplot(data = cv, aes(x = cv, y = cumdist)) + geom_line() + mytheme_bw + geom_vline(xintercept = 0.20, col = "red")

# Plot distribution of CV_N for marine mammal stocks in the U.S.
ggplot(data = cv, aes(x = cv, fill = cv_0.2)) +
  geom_histogram(colour = "black", binwidth = 0.201) +
  labs(x = expression(CV[N]), y = "Frequency", fill = expression(CV[N]>0.20),
       title = paste("U.S. Marine Mammal Stock Assessments:\n", pct, "% of stocks have CV > 0.2", sep = "")) +
  scale_fill_manual(values = c("sky blue", "orange")) + mytheme_bw +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme(legend.position = c(0.85,0.85), legend.justification = c(1,1)) +
  coord_cartesian(xlim = c(-0.05, 1.5))


