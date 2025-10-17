### --- Replication script for mañaneras text analysis --- ###

library(tidyverse)
library(lubridate)

# Load cleaned mañaneras sentence-level corpus
load("Data/text_analysis.RData")

### Figure 2 ###
text_analysis %>%
  # Subset to criticisms
  filter(criticism == 1) %>%
  # Code trash talking, polarization, mixture
  mutate(`Trash-talking` = ifelse(gov_institution == 1 & political_opponent == 0, 1, 0),
         Mixture = ifelse(gov_institution == 1 & political_opponent == 1, 1, 0),
         Polarization = ifelse(gov_institution == 0 & political_opponent == 1, 1, 0)) %>%
  # Count by month
  group_by(month = floor_date(date, "1 months")) %>%
  summarize_at(c("Trash-talking", "Polarization", "Mixture"), ~ sum(., na.rm = T)) %>%
  ungroup() %>%
  # Final month is partial, remove
  filter(month < ymd("2023-10-01")) %>%
  # Prepare plot
  pivot_longer(cols = 2:4, names_to = "strategy", values_to = "count") %>%
  transmute(month = month, `Rhetorical strategy` = factor(strategy, levels = c("Trash-talking", "Polarization", "Mixture")), count = count) %>%
  ggplot() + geom_line(aes(x = month, y = count, color = `Rhetorical strategy`)) + 
  scale_color_manual(values = c("darkblue", "darkorange", "darkgreen")) +
  # Note election dates
  geom_vline(xintercept = my("6/2019"), lty = 3) +
  geom_vline(xintercept = my("6/2020"), lty = 3) +
  geom_vline(xintercept = my("6/2021"), lty = 2) +
  geom_vline(xintercept = my("6/2022"), lty = 3) +
  geom_vline(xintercept = my("6/2023"), lty = 3) +
  theme_minimal() + theme(plot.title = element_text(face = "bold")) +
  labs(x = "Date", y = "Rhetorical attacks", title = "López Obrador's rhetorical attacks", subtitle = "Vertical lines denote local (dotted) and national (dashed) elections")
# Save out
ggsave("figure_2.png", height = 6, width = 9, bg = "white")


### Rhetoric toward military vs non-military institutions ###
# Load dataset with references to military
load("Data/military_coding.RData")
# And subset full corpus to references to non-military gov. institutions to compare
nonmilitary_coding <- text_analysis %>% filter(gov_institution == 1, !id %in% military_coding$id)
table(military_coding$criticism)
table(nonmilitary_coding$criticism)


### Classifier validation ###
# Load human validation set
load("Data/validation_set.RData")
validation_set <- left_join(text_analysis, validation_set) %>%
  drop_na(human_coding)
# Produce confusion matrix
table(validation_set$criticism, validation_set$human_coding)


### Sensitivity analysis ###
# Record false positive and false negative rates for democratic institutions and political opponents from confusion matrix
false_pos_gov <- 0.4691943
false_neg_gov <- 0.07617188
false_pos_opp <- 0.3529412
false_neg_opp <- 0.1736111

# Simulation (1000 iterations)
set.seed(12345)
sensitivity <- data.frame(ratio = rep(NA, 1000), prop_overlap = rep(NA, 1000))
for(i in 1:1000){
  simulation_results <- text_analysis 
  # Flip sentences at random according to false positive and negative rates
  flip_to_neg_gov <- simulation_results %>%
    filter(gov_institution == 1, criticism == 1) %>%
    sample_frac(size = false_pos_gov) %>%
    pull(id)
  
  flip_to_pos_gov <- simulation_results %>%
    filter(gov_institution == 1, criticism == 0) %>%
    sample_frac(size = false_neg_gov) %>%
    pull(id)
  
  flip_to_neg_opp <- simulation_results %>%
    filter(political_opponent == 1, criticism == 1) %>%
    sample_frac(size = false_pos_opp) %>%
    pull(id)
  
  flip_to_pos_opp <- simulation_results %>%
    filter(political_opponent == 1, criticism == 0) %>%
    sample_frac(size = false_neg_opp) %>%
    pull(id)
  
  # Flip them all
  simulation_results <- simulation_results %>%
    mutate(criticism = case_when(
      id %in% flip_to_pos_gov ~ 1,
      id %in% flip_to_neg_gov ~ 0,
      id %in% flip_to_pos_opp ~ 1,
      id %in% flip_to_neg_opp ~ 0,
      TRUE ~ criticism
    ))
  
  # Compute outcomes:
  # Ratio of trash-talking / polarization: 1.311027 in sample
  sensitivity$ratio[i] <- length(simulation_results$criticism[simulation_results$criticism == 1 & simulation_results$gov_institution == 1]) / 
    length(simulation_results$criticism[simulation_results$criticism == 1 & simulation_results$political_opponent == 1])
  # Proportion of attacks with mixed strategy: 0.3411297 in sample
  sensitivity$prop_overlap[i] <- length(simulation_results$criticism[simulation_results$criticism == 1 & simulation_results$gov_institution == 1 & simulation_results$political_opponent == 1]) /
    length(simulation_results$criticism[simulation_results$criticism == 1])
}

# Record true statistics in sample
baseline <- text_analysis %>%
  mutate(`Trash-talking` = ifelse(gov_institution == 1 & political_opponent == 0, 1, 0),
         Mixture = ifelse(gov_institution == 1 & political_opponent == 1, 1, 0),
         Polarization = ifelse(gov_institution == 0 & political_opponent == 1, 1, 0)) 
true_ratio <- length(baseline$criticism[baseline$criticism == 1 & baseline$gov_institution == 1]) / 
  length(baseline$criticism[baseline$criticism == 1 & baseline$political_opponent == 1])
true_overlap <- length(baseline$criticism[baseline$criticism == 1 & baseline$gov_institution == 1 & baseline$political_opponent == 1]) / 
  length(baseline$criticism[baseline$criticism == 1])

# Plot distributions with true statistics for reference
ggplot() + geom_density(data = sensitivity, aes(x = ratio)) + geom_vline(xintercept = true_ratio, lty = 3) + 
  labs(x = "Ratio of trash-talking to polarization", y = "Density", title = "Distribution of trash-talking-polarization ratio across 1000 simulations",
       subtitle = "Dotted line denotes estimated ratio in sample") + theme_minimal() +
  theme(plot.title = element_text(face = "bold")) 
ggsave("sensitivity_ratio.png", height = 6, width = 9, bg = "white")

ggplot() + geom_density(data = sensitivity, aes(x = prop_overlap)) + geom_vline(xintercept = true_overlap, lty = 3) + 
  labs(x = "Proportion of criticisms incorporating both trash-talking and polarization", y = "Density", 
       title = "Distribution of trash-talking-polarization overlap across 1000 simulations",
       subtitle = "Dotted line denotes estimated overlap in sample") + theme_minimal() +
  theme(plot.title = element_text(face = "bold")) 
ggsave("sensitivity_overlap.png", height = 6, width = 9, bg = "white")


