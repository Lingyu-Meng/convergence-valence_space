## ----set up, message = FALSE-----------------------------------------------------------------------------------------------------
library("tidyverse")
library("cowplot")


## ----data input, warning = FALSE, message = FALSE--------------------------------------------------------------------------------
group_data <- read_csv("data1.csv")
group_data <- mutate(group_data, agree = sd2 < sd1) # create a new variable to indicate whether group agreement formed


## ----normalised------------------------------------------------------------------------------------------------------------------
group_data_n <- group_data %>% 
  pivot_longer(cols = 8:9,   names_to ="acc_time", values_to = "acc_val") %>% 
  pivot_longer(cols = 12:13, names_to ="agr_time", values_to = "agr_val") %>% 
  group_by(qcode) %>% 
  mutate(acc_val = scale(acc_val), agr_val = scale(agr_val)) %>% 
  pivot_wider(names_from = "acc_time", values_from = "acc_val") %>% 
  pivot_wider(names_from = "agr_time", values_from = "agr_val") %>% 
  mutate(agree = as.factor(agree), improve = as.factor(improve)) %>% 
  ungroup(qcode)


## ----figure 1. group data, warning = FALSE---------------------------------------------------------------------------------------
arrow_graph_n <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2), data = group_data_n, arrow = arrow(), alpha = 0.2, size = 0.1) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error') + ylim(-3, 5) + xlim(-2, 5) + theme(aspect.ratio=1) 
arrow_graph_n # normalised 


## ----figure 2. averaged movement breaked down on agreement, warning = FALSE------------------------------------------------------
averaged_data_n <- group_data_n %>% 
  select(sd1, sd2, aee1, aee2, agree) %>% 
  group_by(agree) %>% 
  summarise(sd1 = mean(sd1), sd2 = mean(sd2), aee1 = mean(aee1), aee2 = mean(aee2), case = n()/100)
line_thickness <- averaged_data_n %>% select(case) %>% unlist() %>% as.vector() # don't mapping thickness by aes(size = x)

arrow_graph_agree_n_average <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2), data = group_data_n, arrow = arrow(), alpha = 0.2, size = 0.1) + 
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = agree), data = averaged_data_n %>% filter(agree == 'TRUE'), arrow = arrow(), alpha = 1, lwd = line_thickness[2]) + 
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = agree), data = averaged_data_n %>% filter(agree == 'FALSE'), arrow = arrow(), alpha = 1, lwd = line_thickness[1]) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error') + ylim(-3, 5) + xlim(-2, 5) + guides(size = "none") + theme(aspect.ratio=1)
arrow_graph_agree_n_average # normalised broken down with whether group agreement were formed


## ----figure 3. averaged movement breaked down on improvement, warning = FALSE----------------------------------------------------
averaged_data_n <- group_data_n %>% 
  select(sd1, sd2, aee1, aee2, improve) %>% 
  group_by(improve) %>% 
  summarise(sd1 = mean(sd1), sd2 = mean(sd2), aee1 = mean(aee1), aee2 = mean(aee2), case = n()/100)

line_thickness <- averaged_data_n %>% select(case) %>% unlist() %>% as.vector() # assign thickness of arrows by case numbers

levels(averaged_data_n$improve) <- c("FALSE", "TRUE") # rename improvement level

arrow_graph_improve_n_average <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2), data = group_data_n, arrow = arrow(), alpha = 0.2, size = 0.1) +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = improve), data = averaged_data_n %>% filter(improve == 'TRUE'), arrow = arrow(), alpha = 1, lwd = line_thickness[2]) + 
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = improve), data = averaged_data_n %>% filter(improve == 'FALSE'), arrow = arrow(), alpha = 1, lwd = line_thickness[1]) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error') + ylim(-3, 5) + xlim(-2, 5) + guides(size = "none") + theme(aspect.ratio=1)
arrow_graph_improve_n_average # normalised broken down with whether group agreement were formed


## ----figure 4. group date breaked down separately, warning = FALSE---------------------------------------------------------------
levels(group_data_n$improve) <- c("FALSE", "TRUE") # rename improvement level

arrow_graph_agree_n <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = agree), data = group_data_n, arrow = arrow(), alpha = 0.4) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error') + ylim(-3, 5) + xlim(-2, 5) + theme(aspect.ratio=1)
arrow_graph_agree_n # normalised broken down with whether group agreement were formed


## ----figure 5. group date breaked down separately, warning = FALSE---------------------------------------------------------------
arrow_graph_accuracy_n <- ggplot() +
  geom_segment(aes(x = sd1, y = aee1, xend = sd2, yend = aee2, colour = improve), data = group_data_n, arrow = arrow(), alpha = 0.5) + 
  theme_cowplot() + xlab('Disagreement') + ylab('Error') + ylim(-3, 5) + xlim(-2, 5) + theme(aspect.ratio=1)
arrow_graph_accuracy_n # normalised broken down with whether group performance were improved


## ----save images, eval = FALSE, include = FALSE----------------------------------------------------------------------------------
## I_Mer <- plot_grid(arrow_graph_n, arrow_graph_agree_n_average, arrow_graph_improve_n_average, labels = "AUTO", nrow = 1, align = "v", axis = "lb")
## figure2 <- plot_grid(arrow_graph_agree_n, arrow_graph_accuracy_n, labels = "AUTO", nrow = 1, align = "v", axis = "lb")
## save_plot('figure_1.jpg',arrow_graph_n)
## save_plot('figure_2.jpg',arrow_graph_agree_n_average)
## save_plot('figure_3.jpg',arrow_graph_improve_n_average)
## save_plot('figure2_2Nev.jpg',figure2, ncol = 2)


## ----save script, eval = FALSE, include = FALSE----------------------------------------------------------------------------------
## knitr::purl(input = "plot_code.Rmd", output = "plot_code.R",documentation = 1)

