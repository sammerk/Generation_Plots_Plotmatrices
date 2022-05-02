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
library(ggh4x)

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
  test_divider = character(0)
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
                       test_divider = sample(c("A","B","C","D"),1)
                ))
  }
}

#add text-column for geom_texts
data$txt <- ifelse(data$`Sample Size` == 10, "Group Size = 10",
                                         ifelse(data$`Sample Size` == 20, "Group Size = 20",
                                                ifelse(data$`Sample Size` == 40, "Group Size = 40",
                                                       ifelse(data$`Sample Size` == 80, "Group Size = 80", "Other Group Size"))))


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


#for(j in unique(data$test_divider)){


  #plot_to_upload <- 
ggplot(data %>% 
         gather(Group, value, A, B) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
       ) +
  geom_quasirandom(
    colour = "#848484",
    cex = 2) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "green"
  ) +
  facet_wrap2(~interaction(`Group Size`, Overlap), #habe ich geändert, damit nur noch nach einer var gewrappt wird -> wird auch nur ein strip-panel erstellt.
              
              #wenn man hier die values der interaction-variable so umbenennt, dass alle, die den gleichen
              #Overlap (alle, die jeweils mit .69, .8, etc. enden) haben, auch den gleichen value haben,
              #dann sollte strip_nested() unten endlich machen was es soll.
              # -> den Factor recodieren hat mit recode_factor() funktioniert aber dann bin ich nicht mehr weitergekommen
             labeller = as_labeller(c(`10.0.69` = "Overlap between groups A and B = 69%",
                                                         `20.0.69` = "Overlap between groups A and B = 69%",
                                                         `40.0.69` = "Overlap between groups A and B = 69%",
                                                         `80.0.69` = "Overlap between groups A and B = 69%",
                                                         `10.0.8` = "Overlap between groups A and B = 80%",
                                                         `20.0.8` = "Overlap between groups A and B = 80%",
                                                         `40.0.8` = "Overlap between groups A and B = 80%",
                                                         `80.0.8` = "Overlap between groups A and B = 80%",
                                                         `10.0.92` = "Overlap between groups A and B = 92%",
                                                         `20.0.92` = "Overlap between groups A and B = 92%",
                                                         `40.0.92` = "Overlap between groups A and B = 92%",
                                                         `80.0.92` = "Overlap between groups A and B = 92%",
                                                         `10.0.96` = "Overlap between groups A and B = 96%",
                                                         `20.0.96` = "Overlap between groups A and B = 96%",
                                                         `40.0.96` = "Overlap between groups A and B = 96%",
                                                         `80.0.96` = "Overlap between groups A and B = 96%")),
             scales = "free",
             strip = strip_nested( #basiert auf strip_themed, deswegen geändert
               #macht nicht, was es soll, weil es glaube ich auf den vars, nach denen faccetiert wird basiert, nicht auf den tatsächlichen labeln in der Grafik
               #-> vars an overlaplabel anpassen!
               background_x = elem_list_rect(fill = c("#fbda66","#fbda66","#fbda66","#fbda66",
                                                      "#ef9c47","#ef9c47","#ef9c47","#ef9c47",
                                                      "#e55e2c","#e55e2c","#e55e2c","#e55e2c",
                                                      "#88432c","#88432c","#88432c","#88432c")))
  ) +
  ylab("Value"
       ) +
  geom_text(y = 12, x = 2,
    mapping = aes(label = txt),hjust = 0,
    fontface = "bold", color = "black",
  ) +
  theme(panel.spacing = unit(0.35, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  ylim(7,83)

  #plot_to_upload_filename <- paste("demo_plots/matrices", 
                            #"testing",
                            #"if",
                            #"it",
                            #"works",
                            #paste(as.character(j)),
                            #".png",
                            #sep = "_")
  
  #ggsave(plot_to_upload_filename, 
         #plot_to_upload, 
         #dpi = 300,
         #width = 40,
         #height = 25,
         #units = "cm")
#}  


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