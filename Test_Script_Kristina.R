library(bayestestR) # for perfect distributions
library(tidyverse) # for everything
library(patchwork) # for the combination of plots
library(ggdist)
library(ggbeeswarm)
library(hrbrthemes)
library(bain)
library(BayesFactor)
library(lemon)

# Vector of sample sizes
mean_group_A <- 50
sd_group_A <- 10
B_greater_A <- FALSE

sample_sizes <- c(10, 20, 40, 80)
effect_sizes <- c(.1, .2, .5, .8)

# Initialize empty data frame
data <- tibble(
  A = numeric(0),
  B = numeric(0),
  `Sample Size` = numeric(0),
  cohen_d = numeric(0),
  `Overlap` = numeric(0),
)


# loop over sample sizes
for(i in sample_sizes){
  # loop over effect sizes
  for(j in effect_sizes){
    data <- 
      full_join(data,
                tibble(A = distribution_normal(i, 
                                               mean_group_A, 
                                               sd_group_A),
                       B = A - j*sd_group_A,
                       `Sample Size` = i,
                       cohen_d = j,
                       Overlap = round(2*pnorm(-j/2), 2),
                       ))
  }
}


## Visualize the data - dotplot
ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Sample Size` = as.factor(`Sample Size`)), 
       aes(Group, value)) + 
  geom_dots(side = "both",
            binwidth = 3) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "#8cd000"
  ) + 
  facet_grid(Overlap ~ `Sample Size`,
             labeller = label_both) + 
  theme_ipsum_rc() 

# To-do:
# - groups closer together -> to show the overlap
# - Matrix Charakter durch Farbe des Strips hervorheben
# - Tendenz: Entweder
#   - Nur Points mit Mittelwertsdifferenzlabel oder
#   - Points auf Violine mit Overlaplabel


## Visualize the data - beeswarm + violin plot groups on x axis
ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Sample Size` = as.factor(`Sample Size`)), 
       aes(Group, value)) + 
  geom_violin() +
  geom_quasirandom(                #draws jittered data points similarly to geom_jitter but reducing overplotting
    colour = "#848484",
    cex = 2) + 
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "#8cd000"
  ) + 
  facet_rep_grid(Overlap ~ `Sample Size`,
                 repeat.tick.labels = T,
                 labeller = label_both) + 
  theme_ipsum_rc() 


## Visualize the data - beeswarm + violin plot groups on y axis
ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Sample Size` = as.factor(`Sample Size`)), 
       aes(value, Group)) + 
  geom_violin() +
  geom_quasirandom(colour = "#848484",
                   groupOnX = F) + 
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "#8cd000"
  ) + 
  facet_rep_grid(Overlap ~ `Sample Size`,
                 repeat.tick.labels = T,
                 labeller = label_both) + 
  theme_ipsum_rc() 
  
## Inspect the evidence 
data %>% 
  gather(Group, value, A, B) %>% 
  nest_by(`Sample Size`, cohen_d) %>% 
  mutate(PmP = bain(t_test(value ~ Group, data = data), 
                    hypothesis = "groupA > groupB")$fit$PMPc[1],
         AABF = bain(t_test(value ~ Group, data = data), 
                   hypothesis = "groupA > groupB")$fit$BF[1],
         JZSBF = extractBF(ttestBF(formula = value ~ Group, data = data))$bf,
         pval = t.test(value ~ Group, data = data)$p.value)


## Visualize the data - beeswarm + violin plot groups on x axis, Points auf Violine mit Overlaplabel
# to add: Matrix Charakter durch Farbe der Strips hervorheben, ggf. anderes Design dafür wählen (Mark)

ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)) + 
  geom_violin() +
  geom_quasirandom(                #draws jittered data points similarly to geom_jitter but reducing overplotting
    colour = "#848484",
    cex = 2) + 
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "#8cd000"
  ) + 
  facet_wrap(Overlap ~ `Group Size`,
             labeller = labeller(`Group Size` = as_labeller(c(`10` = "Group size = 10",
                                                              `20` = "Group size = 20",
                                                              `40` = "Group size = 40",
                                                              `80` = "Group size = 80")),
                                 Overlap = as_labeller(c(`0.69` = "Overlap between groups A and B = 69%",
                                                         `0.8` = "Overlap between groups A and B = 80%",
                                                         `0.92` = "Overlap between groups A and B = 92%",
                                                         `0.96` = "Overlap between groups A and B = 96%"))),
             scales = "free") +
  ylab("Value") + 
  theme_ipsum_rc() 

#integrate Marc's label colors
ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)) + 
  geom_violin() +
  geom_quasirandom(                #draws jittered data points similarly to geom_jitter but reducing overplotting
    colour = "#848484",
    cex = 2) + 
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "#8cd000"
  ) + 
  facet_wrap2(vars(Overlap, `Group Size`),
             labeller = labeller(`Group Size` = as_labeller(c(`10` = "Group size = 10",
                                                              `20` = "Group size = 20",
                                                              `40` = "Group size = 40",
                                                              `80` = "Group size = 80")),
                                 Overlap = as_labeller(c(`0.69` = "Overlap between groups A and B = 69%",
                                                         `0.8` = "Overlap between groups A and B = 80%",
                                                         `0.92` = "Overlap between groups A and B = 92%",
                                                         `0.96` = "Overlap between groups A and B = 96%"))),
             scales = "free",
             strip = strip_themed( #integrate Marc's label colors
               background_x = elem_list_rect(fill = c("#fbda66","#fbda66","#fbda66","#fbda66",
                                                      "#ef9c47","#ef9c47","#ef9c47","#ef9c47",
                                                      "#e55e2c","#e55e2c","#e55e2c","#e55e2c",
                                                      "#88432c","#88432c","#88432c","#88432c",
                                                      
                                                      "#f0f6d7", "#80d5d5", "#3cadd3","#2c8bc3",
                                                      "#f0f6d7", "#80d5d5", "#3cadd3","#2c8bc3",
                                                      "#f0f6d7", "#80d5d5", "#3cadd3","#2c8bc3",
                                                      "#f0f6d7", "#80d5d5", "#3cadd3","#2c8bc3")))
  ) +
  ylab("Value") + 
  theme_ipsum_rc() 

## Visualize the data - beeswarm + violin plot groups on x axis, nur Points mit Mittelwertsdifferenzlabel
# to add: Matrix Charakter durch Farbe der Strips hervorheben, ggf. anderes Design dafür wählen (Mark)

#data_long <- data %>%
#  pivot_longer(cols = A:B,
#               names_to = "Group",
#               values_to = "Value"
#                 ) 

#integrierter p-Wert des Mittelwertsunterschieds... -> brauchen wir nicht
#ggplot(data_long,
#       aes(Group, Value)) +
#  geom_quasirandom() +
#  stat_compare_means()


#neuer Ansatz A: Mean difference in den Strips
#1. Mittelwertsdifferenzen berechnen
#2. in neuer Variable anlegen
#3. Variable als factor umbenennenn
#4. Überschriften als Kategorie des factors
#5. die Variable in Plot einbauen - danach facet_wrap aufteilen

ggplot(data %>% 
         mutate(Mean_difference = A-B) %>% #adding mean difference
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)),
       aes(Group, value)) + 
  geom_quasirandom(                #draws jittered data points similarly to geom_jitter but reducing overplotting
    colour = "#848484",
    cex = 2) + 
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "#8cd000"
  ) + 
  facet_wrap(Mean_difference ~ `Group Size`,
             labeller = labeller(`Group Size` = as_labeller(c(`10` = "Group size = 10",
                                                              `20` = "Group size = 20",
                                                              `40` = "Group size = 40",
                                                              `80` = "Group size = 80")),
                                 'Mean_difference' = as_labeller(c(`1` = "Mean difference between groups A and B = 1", #mean difference als labeller
                                                         `2` = "Mean difference between groups A and B = 2",
                                                         `5` = "Mean difference between groups A and B = 5",
                                                         `8` = "Mean difference between groups A and B = 8"))),
             scales = "free") +
  ylab("Value") +
  theme_ipsum_rc() 

#neuer Ansatz B: Mean difference integriert
# stat_summary

#integrate Marc's label colors
ggplot(data %>% 
         mutate(Mean_difference = A-B) %>% #adding mean difference
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)),
       aes(Group, value)) + 
  geom_quasirandom(                #draws jittered data points similarly to geom_jitter but reducing overplotting
    colour = "#848484",
    cex = 2) + 
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "#8cd000"
  ) + 
  facet_wrap2(vars(Mean_difference, `Group Size`),
             labeller = labeller(`Group Size` = as_labeller(c(`10` = "Group size = 10",
                                                              `20` = "Group size = 20",
                                                              `40` = "Group size = 40",
                                                              `80` = "Group size = 80")),
                                 'Mean_difference' = as_labeller(c(`1` = "Mean difference between groups A and B = 1", #mean difference als labeller
                                                                   `2` = "Mean difference between groups A and B = 2",
                                                                   `5` = "Mean difference between groups A and B = 5",
                                                                   `8` = "Mean difference between groups A and B = 8"))),
             scales = "free",
             strip = strip_themed( #integrate Marc's label colors
               background_x = elem_list_rect(fill = c("#fbda66","#fbda66","#fbda66","#fbda66",
                                                      "#ef9c47","#ef9c47","#ef9c47","#ef9c47",
                                                      "#e55e2c","#e55e2c","#e55e2c","#e55e2c",
                                                      "#88432c","#88432c","#88432c","#88432c",
                                                      
                                                      "#f0f6d7", "#80d5d5", "#3cadd3","#2c8bc3",
                                                      "#f0f6d7", "#80d5d5", "#3cadd3","#2c8bc3",
                                                      "#f0f6d7", "#80d5d5", "#3cadd3","#2c8bc3",
                                                      "#f0f6d7", "#80d5d5", "#3cadd3","#2c8bc3")))
             ) +
  ylab("Value") +
  theme_ipsum_rc() 

## Save a bunch of plots programmatically
for(j in effect_sizes){
  temporary_data <- 
    tibble(
      A = distribution_normal(50, 50, 10),
      B = distribution_normal(50, 50 + j*10, 10)) %>% 
    gather(group, variable)
  
  temporary_plot <- 
    ggplot(temporary_data, aes(group, variable)) + 
    geom_jitter()
      
  temporary_filename <- paste("demo_plots/jitterplot", 
                              "N50", 
                              "d", 
                              substr(j, 3, 3), # sonst wird alles nach dem Puntk as Dateiendung verstanden
                              ".png",
                              sep = "_")
  
  ggsave(temporary_filename, 
         temporary_plot, 
         dpi = 300)
}
