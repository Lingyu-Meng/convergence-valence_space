#' ---
#' title: "convergence-valence_space"
#' author: "L.-Y. M."
#' date: "2023-10-11"
#' output: html_document
#' ---
#' # input
## ----set up----------------------------------------------------------------------------------------------------------------------
setwd("/Users/owo/Desktop/convergence-valence_space/")
library("tidyverse")
library("cowplot")

#' 
## ----data input------------------------------------------------------------------------------------------------------------------
group_data <- read_csv("data1.csv")
# DATA1 <- read_csv("indS1dates1.csv")
# DATA2 <- read_csv("indS1dates2.csv")
# DATA  <- bind_rows(DATA1, DATA2)
group_data <- mutate(group_data, agree = sd2 < sd1) # create a new variable to indicate whether group agreement formed

#' 
#' # visualise
#' 
#' ### point1 = (Deviation1, Error1) = (sd1, aee1)
#' 
#' ### point2 = (Deviation2, Error2) = (sd2, aee2)
#' 
## ----visualise without normalise, eval = F---------------------------------------------------------------------------------------
## arrow_graph <- ggplot() +
##   geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2), data = group_data, arrow = arrow(), alpha = 0.05) +
##   theme_cowplot() + xlab('Disagreement') + ylab('Error')
## arrow_graph # original
## 
## arrow_graph_accuracy <- ggplot() +
##   geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = as.factor(improve)), data = group_data, arrow = arrow(), alpha = 0.5) +
##   theme_cowplot() + xlab('Disagreement') + ylab('Error')
## arrow_graph_accuracy # original broken down with whether group performance were improved
## 
## arrow_graph_agree <- ggplot() +
##   geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = as.factor(agree)), data = group_data, arrow = arrow(), alpha = 0.5) +
##   theme_cowplot() + xlab('Disagreement') + ylab('Error')
## arrow_graph_agree # original broken down with whether group agreement were formed

#' 
## ----normalised------------------------------------------------------------------------------------------------------------------
group_data_n <- group_data %>% 
  pivot_longer(cols = 8:9,   names_to ="acc_time", values_to = "acc_val") %>% 
  pivot_longer(cols = 12:13, names_to ="agr_time", values_to = "agr_val") %>% 
  group_by(qcode) %>% 
  mutate(acc_val = scale(acc_val), agr_val = scale(agr_val)) %>% 
  pivot_wider(names_from = "acc_time", values_from = "acc_val") %>% 
  pivot_wider(names_from = "agr_time", values_from = "agr_val")

#' 
## ----visualise with normalised---------------------------------------------------------------------------------------------------
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

#' 
## ----visualise for average movement----------------------------------------------------------------------------------------------
averaged_data_n <- group_data_n %>% 
  select(sd1, sd2, aee1, aee2, agree) %>% 
  group_by(agree) %>% 
  mutate(sd1 = mean(sd1), sd2 = mean(sd2), aee1 = mean(aee1), aee2 = mean(aee2))
arrow_graph_agree_n_average <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = as.factor(agree)), data = averaged_data_n, arrow = arrow()) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error')
arrow_graph_agree_n_average # normalised broken down with whether group agreement were formed

#' 
## ----save images-----------------------------------------------------------------------------------------------------------------
# I_Mer <- plot_grid(arrow_graph, arrow_graph_accuracy, arrow_graph_agree, arrow_graph_n, arrow_graph_accuracy_n, arrow_graph_agree_n, labels = "AUTO") # figures without normalised are removed.
I_Mer <- plot_grid(arrow_graph_n, arrow_graph_accuracy_n, arrow_graph_agree_n, arrow_graph_agree_n_average, labels = "AUTO", nrow = 1)
save_plot('figure_16Oct.png',I_Mer)

## ----save script, include = FALSE------------------------------------------------------------------------------------------------
knitr::purl(input = "plot_code.Rmd", output = "plot_code_16Oct.R",documentation = 2)

#' 
