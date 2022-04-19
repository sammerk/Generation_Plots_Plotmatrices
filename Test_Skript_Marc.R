library(bayestestR) # for perfect distributions
library(tidyverse) # for everything
library(patchwork) # for the combination of plots
library(ggdist)
library(hrbrthemes)
library(bain)
library(grid)
library(ggdist)
library(ggridges)
library(ggbeeswarm)

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


# Visualize the data  
ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Sample Size` = as.factor(`Sample Size`)), 
       aes(Group, value)) + 
  geom_dots(side = "both",
            binwidth = 2.75,
            colour = "black",
            shape = 20,
            dotsize = 1.5) + 
  facet_grid(Overlap ~ `Sample Size`) +
  theme(panel.spacing = unit(0.35, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(color="black", fill="#5ebc3b", size=1.5, linetype="solid"),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold", angle = 360),
        axis.title.y = element_text(size = rel(1.25), angle = 90, vjust = 2),
        axis.title.x = element_text(size = rel(1.25))
        ) +
  labs(title = "Sample sizes") +
  ylab("Value")
  
# Add another title outside y-axis (right)


## DATA VIS WITH ggdist

#Ausprobieren:
#Titel in jedem Strip Wiederholen (?)
#Welche Vis ist konsistent/geeignet zur Verbalisierung “Overlap = 90%”
#Ausprobieren: quasirandom mit Violine im Hintergrund (symmetrisch und halbiert) (ggdist package)


ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)) +
  
  #symmetrisch
  #stat_slab(side = "both", scale = 0.3, position = "dodgejust") +
  #geom_violin() +
  #geom_quasirandom(width = 0.2) +
  
  #possibly get geom_violinhalf?
  
  #halbiert
  stat_slab(side = "left", scale = 0.3, fill = "black") +
  stat_dots(side = "right", scale = 0.25, quantiles = 50, fill = "black") +
  
  
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "orange"
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
  theme(panel.spacing = unit(0.35, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(color="black", fill="#5ebc3b", size=1.5, linetype="solid"),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold", angle = 360),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  ylim(7,83)

#es ist möglich, die Gruppen so anzuordnen, wie in facet_grid, ist aber ein bisschen umständlich:
#https://stackoverflow.com/questions/52706599/how-to-position-strip-labels-in-facet-wrap-like-in-facet-grid
#try to move the groups closer to one another


# Single graph

ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Sample Size` = as.factor(`Sample Size`)), 
       aes(Group, value)) + 
  geom_dots(side = "both",
            colour = "black",
            shape = 20) +
  theme(panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        axis.title.y = element_text(size = rel(1.25), angle = 90),
        axis.title.x = element_text(size = rel(1.25)),
        axis.text.x = element_text(face="bold", color="black", size=12)
        ) +
  ylab("Value") +
  scale_x_discrete(labels=c("Sparklies", "Jinglies"))

# needs sample size mentioned somewhere, as well as overlap
# add the squiggly line at the lower end of the y-axis


# Inspect the evidence 
data %>% 
  gather(Group, value, A, B) %>% 
  nest_by(`Sample Size`, cohen_d) %>% 
  mutate(PmP = bain(t_test(value ~ Group, data = data), 
                    hypothesis = "groupA > groupB")$fit$PMPc[1],
         AABF = bain(t_test(value ~ Group, data = data), 
                     hypothesis = "groupA > groupB")$fit$BF[1],
         JZSBF = extractBF(ttestBF(formula = value ~ Group, data = data))$bf,
         pval = t.test(value ~ Group, data = data)$p.value)


# Save a bunch of plots programmatically
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
