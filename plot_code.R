## ----set up, include = F-------------------------------------------------------------------------------------------------------------------------------
library("tidyverse")
library("cowplot")

## ----data input, include = F---------------------------------------------------------------------------------------------------------------------------
group_data <- read_csv("data1.csv")

## ----visualise without normalise, include = F----------------------------------------------------------------------------------------------------------
group_data <- mutate(group_data, agree = sd2 < sd1) # create a new variable to indicate whether group agreement formed

arrow_graph <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2), data = group_data, arrow = arrow(), alpha = 0.05) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error')
arrow_graph # original

arrow_graph_accuracy <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = as.factor(improve)), data = group_data, arrow = arrow(), alpha = 0.5) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error')
arrow_graph_accuracy # original broken down with whether group performance were improved

arrow_graph_agree <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = as.factor(agree)), data = group_data, arrow = arrow(), alpha = 0.5) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error')
arrow_graph_agree # original broken down with whether group agreement were formed

## ----normalised, include = F---------------------------------------------------------------------------------------------------------------------------
group_data_n <- group_data %>% 
  pivot_longer(cols = 8:9,   names_to ="acc_time", values_to = "acc_val") %>% 
  pivot_longer(cols = 12:13, names_to ="agr_time", values_to = "agr_val") %>% 
  group_by(qcode) %>% 
  mutate(acc_val = scale(acc_val), agr_val = scale(agr_val)) %>% 
  pivot_wider(names_from = "acc_time", values_from = "acc_val") %>% 
  pivot_wider(names_from = "agr_time", values_from = "agr_val")

## ----visualise with normalised, include = F------------------------------------------------------------------------------------------------------------
arrow_graph_n <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2), data = group_data_n, arrow = arrow(), alpha = 0.2) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error')
arrow_graph_n # normalised 

arrow_graph_accuracy_n <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = as.factor(improve)), data = group_data_n, arrow = arrow(), alpha = 0.5) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error')
arrow_graph_accuracy_n # normalised broken down with whether group performance were improved

arrow_graph_agree_n <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = as.factor(agree)), data = group_data_n, arrow = arrow(), alpha = 0.4) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error')
arrow_graph_agree_n # normalised broken down with whether group agreement were formed

## ----save images, include = F--------------------------------------------------------------------------------------------------------------------------
I_Mer <- plot_grid(arrow_graph, arrow_graph_accuracy, arrow_graph_agree, arrow_graph_n, arrow_graph_accuracy_n, arrow_graph_agree_n, labels = "AUTO")
save_plot('figure_11Oct.png',I_Mer, ncol = 3, base_height = 5, base_asp = 1)

