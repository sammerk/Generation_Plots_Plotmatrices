library(bayestestR) # for perfect distributions
library(tidyverse) # for everything
library(patchwork) # for the combination of plots
library(ggdist)
library(hrbrthemes)
library(bain)
library(ggbeeswarm)
library(ggh4x)


#### Daten generieren ####
# Vector of sample sizes
average_jsp <- 50
average_height <- 168 #US mean for both mean and women together https://www.cdc.gov/nchs/data/nhsr/nhsr122-508.pdf
average_salary <- 4200 

sd_jsp <- 10
sd_height <- 8 
sd_salary <- 600 


sample_sizes_1 <- c(11, 25, 37, 74)
sample_sizes_3 <- c(9, 22, 40, 75)
sample_sizes_4 <- c(14, 21, 38, 79)
sample_sizes_5 <- c(15, 26, 41, 85)
sample_sizes_6 <- c(10, 23, 39, 81)
effect_sizes <- c(.1, .2, .5, .8)



# Initialize empty data frames
#sparklies jinglies
data_jinglies_betterthan_sparklies <- tibble(
  Jinglies = numeric(0),
  Sparklies = numeric(0),
  `Sample Size` = numeric(0),
  cohen_d = numeric(0),
  `Overlap` = numeric(0)
)


#height
data_women_tallerthan_men <- tibble( 
  Women = numeric(0),
  Men = numeric(0),
  `Sample Size` = numeric(0),
  cohen_d = numeric(0),
  `Overlap` = numeric(0)
)
data_men_tallertan_women <- tibble(
  Women = numeric(0),
  Men = numeric(0),
  `Sample Size` = numeric(0),
  cohen_d = numeric(0),
  `Overlap` = numeric(0)
)


#salary
data_dentists_morethan_teachers <- tibble(
  Dentists = numeric(0),
  Primary_Teachers = numeric(0),
  `Sample Size` = numeric(0),
  cohen_d = numeric(0),
  `Overlap` = numeric(0)
)
data_teachers_morethan_dentists <- tibble(
  Dentists = numeric(0),
  Primary_Teachers = numeric(0),
  `Sample Size` = numeric(0),
  cohen_d = numeric(0),
  `Overlap` = numeric(0)
)


# loop over sample sizes jinglies sparklies
for(i in sample_sizes_1){
  # loop over effect sizes
  for(j in effect_sizes){
    data_jinglies_betterthan_sparklies <- 
      full_join(data_jinglies_betterthan_sparklies,
                tibble(Jinglies = distribution_normal(i, 
                                               average_jsp+j*0.5*sd_jsp, 
                                               sd_jsp),
                       Sparklies = distribution_normal(i, 
                                                       average_jsp-j*0.5*sd_jsp, 
                                                       sd_jsp),
                       `Sample Size` = i,
                       cohen_d = j,
                       Overlap = round(2*pnorm(-j/2), 2)
                ))
  }
}

# loop over sample sizes height
for(i in sample_sizes_3){
  # loop over effect sizes
  for(j in effect_sizes){
    data_women_tallerthan_men <- 
      full_join(data_women_tallerthan_men,
                tibble(Women = distribution_normal(i, 
                                               average_height+j*0.5*sd_height, 
                                               sd_height),
                       Men = distribution_normal(i, 
                                                 average_height-j*0.5*sd_height, 
                                                 sd_height),
                       `Sample Size` = i,
                       cohen_d = j,
                       Overlap = round(2*pnorm(-j/2), 2)
                ))
  }
}
for(i in sample_sizes_4){
  # loop over effect sizes
  for(j in effect_sizes){
    data_men_tallertan_women <- 
      full_join(data_men_tallertan_women,
                tibble(Men = distribution_normal(i, 
                                               average_height+0.5*j*sd_height, 
                                               sd_height),
                       Women = distribution_normal(i, 
                                                   average_height-0.5*j*sd_height, 
                                                   sd_height),
                       `Sample Size` = i,
                       cohen_d = j,
                       Overlap = round(2*pnorm(-j/2), 2)
                ))
  }
}

# loop over sample sizes salary
for(i in sample_sizes_5){
  # loop over effect sizes
  for(j in effect_sizes){
    data_dentists_morethan_teachers <- 
      full_join(data_dentists_morethan_teachers,
                tibble(Dentists = distribution_normal(i, 
                                               average_salary+j*0.5*sd_salary, 
                                               sd_salary),
                       Primary_Teachers = distribution_normal(i, 
                                                              average_salary-j*0.5*sd_salary, 
                                                              sd_salary),
                       `Sample Size` = i,
                       cohen_d = j,
                       Overlap = round(2*pnorm(-j/2), 2)
                ))
  }
}
for(i in sample_sizes_6){
  # loop over effect sizes
    for(j in effect_sizes){ 
      data_teachers_morethan_dentists <- 
        full_join(data_teachers_morethan_dentists,
                  tibble(Primary_Teachers = distribution_normal(i, 
                                                average_salary+j*0.5*sd_salary, 
                                                sd_salary),
                        Dentists = distribution_normal(i, 
                                                       average_salary-j*0.5*sd_salary, 
                                                       sd_salary),
                        `Sample Size` = i,
                        cohen_d = j,
                        Overlap = round(2*pnorm(-j/2), 2)
                  ))
    }
  
}


#### Matrix mit overlapping overlaplabel Gefüllt angepasst für salary dentists more TRANSPOx2 ####

#transpo
data_dentists_morethan_teachers$Overlap_f <- factor(data_dentists_morethan_teachers$Overlap, levels=c(0.96,0.92,0.8,0.69))
data_dentists_morethan_teachers$`Sample Size_f` <- factor(data_dentists_morethan_teachers$`Sample Size`, levels=c(85,41,26,15))
  

ggplot(data_dentists_morethan_teachers %>% 
         gather(Group, value, Dentists, Primary_Teachers) %>% 
         mutate(Overlap = as.factor(Overlap_f),
                `Group Size` = as.factor(`Sample Size_f`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 2) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`15` = "Group size = 15",
                                                               `26` = "Group size = 26",
                                                               `41` = "Group size = 41",
                                                               `85` = "Group size = 85")),
                                  Overlap = as_labeller(c(`0.69` = "Overlap between Dentists and Primary Teachers = 69%",
                                                          `0.8` = "Overlap between Dentists and Primary Teachers = 80%",
                                                          `0.92` = "Overlap between Dentists and Primary Teachers = 92%",
                                                          `0.96` = "Overlap between Dentists and Primary Teachers = 96%"))),
              scales = "free",
              strip = strip_nested(
                background_x = elem_list_rect(fill = c("#fde725","#35b779","#31688e","#440154",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")),
                text_x = elem_list_text(colour = c("black", "white","white","white",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black"),
                                        face = c("bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold")))
  ) +
  ylab("Monthly salary in EUR"
  ) +
  xlab("Profession"
  ) +
  theme(panel.spacing = unit(0.35, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold")
  ) +
  scale_x_discrete(labels=c("Dentists" = "Dentists", "Primary_Teachers" = "Primary Teachers")
  ) +
  ylim(2000,6000)

ggsave(paste("demo_plots/matrices", 
       "dentists",
       "more",
       ".png",
       sep = "_"),
dpi = 600,
width = 40,
height = 25,
units = "cm")

#### Matrix mit overlapping overlaplabel Gefüllt angepasst für salary teachers more ####


ggplot(data_teachers_morethan_dentists %>% 
         gather(Group, value, Dentists, Primary_Teachers) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 2) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`10` = "Group size = 10",
                                                               `23` = "Group size = 23",
                                                               `39` = "Group size = 39",
                                                               `81` = "Group size = 81")),
                                  Overlap = as_labeller(c(`0.69` = "Overlap between Dentists and Primary Teachers = 69%",
                                                          `0.8` = "Overlap between Dentists and Primary Teachers = 80%",
                                                          `0.92` = "Overlap between Dentists and Primary Teachers = 92%",
                                                          `0.96` = "Overlap between Dentists and Primary Teachers = 96%"))),
              scales = "free",
              strip = strip_nested(
                background_x = elem_list_rect(fill = c("#fde725","#35b779","#31688e","#440154",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")),
                text_x = elem_list_text(colour = c("black", "white","white","white",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black"),
                                        face = c("bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold")))
  ) +
  ylab("Monthly salary in EUR"
  ) +
  xlab("Profession"
  ) +
  theme(panel.spacing = unit(0.35, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold")
  ) +
  scale_x_discrete(labels=c("Dentists" = "Dentists", "Primary_Teachers" = "Primary Teachers")
  ) +
  ylim(2000,6000)

ggsave(paste("demo_plots/matrices", 
             "teachers",
             "more",
             ".png",
             sep = "_"),
       dpi = 600,
       width = 40,
       height = 25,
       units = "cm")


#### Matrix mit overlapping overlaplabel Gefüllt angepasst für jinglies better TRANSPOx1 ####

#transpo
data_jinglies_betterthan_sparklies$`Sample Size_f` <- factor(data_jinglies_betterthan_sparklies$`Sample Size`, levels=c(74,37,25,11))


ggplot(data_jinglies_betterthan_sparklies %>% 
         gather(Group, value, Jinglies, Sparklies) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size_f`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 2) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`11` = "Group size = 11",
                                                               `25` = "Group size = 25",
                                                               `37` = "Group size = 37",
                                                               `74` = "Group size = 74")),
                                  Overlap = as_labeller(c(`0.69` = "Overlap between Jinglies and Sparklies = 69%",
                                                          `0.8` = "Overlap between Jinglies and Sparklies = 80%",
                                                          `0.92` = "Overlap between Jinglies and Sparklies = 92%",
                                                          `0.96` = "Overlap between Jinglies and Sparklies = 96%"))),
              scales = "free",
              strip = strip_nested(
                background_x = elem_list_rect(fill = c("#fde725","#35b779","#31688e","#440154",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")),
                text_x = elem_list_text(colour = c("black", "white","white","white",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black"),
                                        face = c("bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold")))
  ) +
  ylab("Minutes spent on building toy"
  ) +
  xlab("Type of Elf"
  ) +
  theme(panel.spacing = unit(0.35, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold")
  ) +
  ylim(7,83)

ggsave(paste("demo_plots/matrices", 
             "jingles",
             "better",
             ".png",
             sep = "_"),
       dpi = 600,
       width = 40,
       height = 25,
       units = "cm")


#### Matrix mit overlapping overlaplabel Gefüllt angepasst für women taller TRANSPOx2 ####

#transpo
data_women_tallerthan_men$Overlap_f <- factor(data_women_tallerthan_men$Overlap, levels=c(0.96,0.92,0.8,0.69))
data_women_tallerthan_men$`Sample Size_f` <- factor(data_women_tallerthan_men$`Sample Size`, levels=c(75,40,22,9))


ggplot(data_women_tallerthan_men %>% 
         gather(Group, value, Women, Men) %>% 
         mutate(Overlap = as.factor(Overlap_f),
                `Group Size` = as.factor(`Sample Size_f`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 2) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`9` = "Group size = 9",
                                                               `22` = "Group size = 22",
                                                               `40` = "Group size = 40",
                                                               `75` = "Group size = 75")),
                                  Overlap = as_labeller(c(`0.69` = "Overlap between Women and Men = 69%",
                                                          `0.8` = "Overlap between Women and Men = 80%",
                                                          `0.92` = "Overlap between Women and Men = 92%",
                                                          `0.96` = "Overlap between Women and Men = 96%"))),
              scales = "free",
              strip = strip_nested(
                background_x = elem_list_rect(fill = c("#fde725","#35b779","#31688e","#440154",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")),
                text_x = elem_list_text(colour = c("black", "white","white","white",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black"),
                                        face = c("bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold")))
  ) +
  ylab("Height in centimeters"
  ) +
  xlab("Gender"
  ) +
  theme(panel.spacing = unit(0.35, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold")
  ) +
  ylim(130,200)

ggsave(paste("demo_plots/matrices", 
             "women",
             "taller",
             ".png",
             sep = "_"),
       dpi = 600,
       width = 40,
       height = 25,
       units = "cm")


#### Matrix mit overlapping overlaplabel Gefüllt angepasst für men taller ####


ggplot(data_men_tallertan_women %>% 
         gather(Group, value, Women, Men) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 2) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 10, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`14` = "Group size = 9",
                                                               `21` = "Group size = 21",
                                                               `38` = "Group size = 38",
                                                               `79` = "Group size = 79")),
                                  Overlap = as_labeller(c(`0.69` = "Overlap between Women and Men = 69%",
                                                          `0.8` = "Overlap between Women and Men = 80%",
                                                          `0.92` = "Overlap between Women and Men = 92%",
                                                          `0.96` = "Overlap between Women and Men = 96%"))),
              scales = "free",
              strip = strip_nested(
                background_x = elem_list_rect(fill = c("#fde725","#35b779","#31688e","#440154",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF",
                                                       "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF")),
                text_x = elem_list_text(colour = c("black", "white","white","white",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black",
                                                   "black","black","black","black"),
                                        face = c("bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold",
                                                 "bold","bold","bold","bold")))
  ) +
  ylab("Height in centimeters"
  ) +
  xlab("Gender"
  ) +
  theme(panel.spacing = unit(0.35, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold")
  ) +
  ylim(130,200)

ggsave(paste("demo_plots/matrices", 
             "men",
             "taller",
             ".png",
             sep = "_"),
       dpi = 600,
       width = 40,
       height = 25,
       units = "cm")

####DEPRECATED####

#### first viz try

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

#### Matrix ohne durchg. Label

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


#### Matrix ohne durchg. Label angepasst für jinglies sparklies

#for(j in unique(data$test_divider)){


#plot_to_upload <- 
ggplot(data_jinglies_betterthan_sparklies %>% 
         gather(Group, value, Jinglies, Sparklies) %>% 
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
              labeller = as_labeller(c(`10.0.69` = "Overlap between Jinglies and Sparklies = 69%",
                                       `20.0.69` = "Overlap between Jinglies and Sparklies = 69%",
                                       `40.0.69` = "Overlap between Jinglies and Sparklies = 69%",
                                       `80.0.69` = "Overlap between Jinglies and Sparklies = 69%",
                                       `10.0.8` = "Overlap between Jinglies and Sparklies = 80%",
                                       `20.0.8` = "Overlap between Jinglies and Sparklies = 80%",
                                       `40.0.8` = "Overlap between Jinglies and Sparklies = 80%",
                                       `80.0.8` = "Overlap between Jinglies and Sparklies = 80%",
                                       `10.0.92` = "Overlap between Jinglies and Sparklies = 92%",
                                       `20.0.92` = "Overlap between Jinglies and Sparklies = 92%",
                                       `40.0.92` = "Overlap between Jinglies and Sparklies = 92%",
                                       `80.0.92` = "Overlap between Jinglies and Sparklies = 92%",
                                       `10.0.96` = "Overlap between Jinglies and Sparklies = 96%",
                                       `20.0.96` = "Overlap between Jinglies and Sparklies = 96%",
                                       `40.0.96` = "Overlap between Jinglies and Sparklies = 96%",
                                       `80.0.96` = "Overlap between Jinglies and Sparklies = 96%")),
              scales = "free",
              strip = strip_nested( #basiert auf strip_themed, deswegen geändert
                #macht nicht, was es soll, weil es glaube ich auf den vars, nach denen faccetiert wird basiert, nicht auf den tatsächlichen labeln in der Grafik
                #-> vars an overlaplabel anpassen!
                background_x = elem_list_rect(fill = c("#fbda66","#fbda66","#fbda66","#fbda66",
                                                       "#ef9c47","#ef9c47","#ef9c47","#ef9c47",
                                                       "#e55e2c","#e55e2c","#e55e2c","#e55e2c",
                                                       "#88432c","#88432c","#88432c","#88432c")))
  ) +
  ylab("Minutes spent on building toy"
  ) +
  xlab("Type of Elf"
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
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold")
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


#### Matrix mit overlapping overlaplabel LEER

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
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`10` = "",
                                                               `20` = "",
                                                               `40` = "",
                                                               `80` = "")),
                                  Overlap = as_labeller(c(`0.69` = "Overlap between groups A and B = 69%",
                                                          `0.8` = "Overlap between groups A and B = 80%",
                                                          `0.92` = "Overlap between groups A and B = 92%",
                                                          `0.96` = "Overlap between groups A and B = 96%"))),
              
              #strip_nested kann nur funktionieren, wenn es zwei levels an strip gibt,
              #aber man kann den unteren strip nicht separat vom oberen entfernen (soweit ich weiß), deswegen zwei Versionen als "Ersatz":
              #1. Unterer Strip hat die Informationen aus dem geom_text.
              #2. Unterer Strip ist leer.
              
              #habe ich geändert, damit nur noch nach einer var gewrappt wird -> wird auch nur ein strip-panel erstellt.
              
              #wenn man hier die values der interaction-variable so umbenennt, dass alle, die den gleichen
              #Overlap (alle, die jeweils mit .69, .8, etc. enden) haben, auch den gleichen value haben,
              #dann sollte strip_nested() unten endlich machen was es soll.
              # -> den Factor recodieren hat mit recode_factor() funktioniert aber dann bin ich nicht mehr weitergekommen
              # labeller = as_labeller(c(`10.0.69` = "Overlap between groups A and B = 69%",
              #                          `20.0.69` = "Overlap between groups A and B = 69%",
              #                          `40.0.69` = "Overlap between groups A and B = 69%",
              #                          `80.0.69` = "Overlap between groups A and B = 69%",
              #                          `10.0.8` = "Overlap between groups A and B = 80%",
              #                          `20.0.8` = "Overlap between groups A and B = 80%",
              #                          `40.0.8` = "Overlap between groups A and B = 80%",
              #                          `80.0.8` = "Overlap between groups A and B = 80%",
              #                          `10.0.92` = "Overlap between groups A and B = 92%",
              #                          `20.0.92` = "Overlap between groups A and B = 92%",
              #                          `40.0.92` = "Overlap between groups A and B = 92%",
              #                          `80.0.92` = "Overlap between groups A and B = 92%",
              #                          `10.0.96` = "Overlap between groups A and B = 96%",
              #                          `20.0.96` = "Overlap between groups A and B = 96%",
              #                          `40.0.96` = "Overlap between groups A and B = 96%",
              #                          `80.0.96` = "Overlap between groups A and B = 96%")),
              scales = "free",
              strip = strip_nested( #basiert auf strip_themed, deswegen geändert
                #macht nicht, was es soll, weil es glaube ich auf den vars, nach denen faccetiert wird basiert, nicht auf den tatsächlichen labeln in der Grafik
                #-> vars an overlaplabel anpassen!
                background_x = elem_list_rect(fill = c("#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c")))
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

#### Matrix mit overlapping overlaplabel LEER angepasst für height

#for(j in unique(data$test_divider)){


#plot_to_upload <- 
ggplot(data_women_tallerthan_men %>% 
         gather(Group, value, Women, Men) %>% 
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
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`10` = "",
                                                               `20` = "",
                                                               `40` = "",
                                                               `80` = "")),
                                  Overlap = as_labeller(c(`0.69` = "Overlap between Women and Men = 69%",
                                                          `0.8` = "Overlap between Women and Men = 80%",
                                                          `0.92` = "Overlap between Women and Men = 92%",
                                                          `0.96` = "Overlap between Women and Men = 96%"))),
              
              #strip_nested kann nur funktionieren, wenn es zwei levels an strip gibt,
              #aber man kann den unteren strip nicht separat vom oberen entfernen (soweit ich weiß), deswegen zwei Versionen als "Ersatz":
              #1. Unterer Strip hat die Informationen aus dem geom_text.
              #2. Unterer Strip ist leer.
              
              #habe ich geändert, damit nur noch nach einer var gewrappt wird -> wird auch nur ein strip-panel erstellt.
              
              #wenn man hier die values der interaction-variable so umbenennt, dass alle, die den gleichen
              #Overlap (alle, die jeweils mit .69, .8, etc. enden) haben, auch den gleichen value haben,
              #dann sollte strip_nested() unten endlich machen was es soll.
              # -> den Factor recodieren hat mit recode_factor() funktioniert aber dann bin ich nicht mehr weitergekommen
              # labeller = as_labeller(c(`10.0.69` = "Overlap between groups A and B = 69%",
              #                          `20.0.69` = "Overlap between groups A and B = 69%",
              #                          `40.0.69` = "Overlap between groups A and B = 69%",
              #                          `80.0.69` = "Overlap between groups A and B = 69%",
              #                          `10.0.8` = "Overlap between groups A and B = 80%",
              #                          `20.0.8` = "Overlap between groups A and B = 80%",
              #                          `40.0.8` = "Overlap between groups A and B = 80%",
              #                          `80.0.8` = "Overlap between groups A and B = 80%",
              #                          `10.0.92` = "Overlap between groups A and B = 92%",
              #                          `20.0.92` = "Overlap between groups A and B = 92%",
              #                          `40.0.92` = "Overlap between groups A and B = 92%",
              #                          `80.0.92` = "Overlap between groups A and B = 92%",
              #                          `10.0.96` = "Overlap between groups A and B = 96%",
              #                          `20.0.96` = "Overlap between groups A and B = 96%",
              #                          `40.0.96` = "Overlap between groups A and B = 96%",
              #                          `80.0.96` = "Overlap between groups A and B = 96%")),
              scales = "free",
              strip = strip_nested( #basiert auf strip_themed, deswegen geändert
                #macht nicht, was es soll, weil es glaube ich auf den vars, nach denen faccetiert wird basiert, nicht auf den tatsächlichen labeln in der Grafik
                #-> vars an overlaplabel anpassen!
                background_x = elem_list_rect(fill = c("#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c")))
  ) +
  ylab("Height in centimeters"
  ) +
  xlab("Gender"
  ) +
  geom_text(y = 135, x = 2,
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
        axis.title.y = element_text(size = 17, color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold")
  ) +
  ylim(130,200)

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


#### Matrix mit overlapping overlaplabel Gefüllt

#for(j in unique(data$test_divider)){


#plot_to_upload <- 
ggplot(data_jinglies_betterthan_sparklies %>% 
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
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`10` = "Group size = 10",
                                                               `20` = "Group size = 20",
                                                               `40` = "Group size = 40",
                                                               `80` = "Group size = 80")),
                                  Overlap = as_labeller(c(`0.69` = "Overlap between groups A and B = 69%",
                                                          `0.8` = "Overlap between groups A and B = 80%",
                                                          `0.92` = "Overlap between groups A and B = 92%",
                                                          `0.96` = "Overlap between groups A and B = 96%"))),
              
              #strip_nested kann nur funktionieren, wenn es zwei levels an strip gibt,
              #aber man kann den unteren strip nicht separat vom oberen entfernen (soweit ich weiß), deswegen zwei Versionen als "Ersatz":
              #1. Unterer Strip hat die Informationen aus dem geom_text.
              #2. Unterer Strip ist leer.
              
              #habe ich geändert, damit nur noch nach einer var gewrappt wird -> wird auch nur ein strip-panel erstellt.
              
              #wenn man hier die values der interaction-variable so umbenennt, dass alle, die den gleichen
              #Overlap (alle, die jeweils mit .69, .8, etc. enden) haben, auch den gleichen value haben,
              #dann sollte strip_nested() unten endlich machen was es soll.
              # -> den Factor recodieren hat mit recode_factor() funktioniert aber dann bin ich nicht mehr weitergekommen
              # labeller = as_labeller(c(`10.0.69` = "Overlap between groups A and B = 69%",
              #                          `20.0.69` = "Overlap between groups A and B = 69%",
              #                          `40.0.69` = "Overlap between groups A and B = 69%",
              #                          `80.0.69` = "Overlap between groups A and B = 69%",
              #                          `10.0.8` = "Overlap between groups A and B = 80%",
              #                          `20.0.8` = "Overlap between groups A and B = 80%",
              #                          `40.0.8` = "Overlap between groups A and B = 80%",
              #                          `80.0.8` = "Overlap between groups A and B = 80%",
              #                          `10.0.92` = "Overlap between groups A and B = 92%",
              #                          `20.0.92` = "Overlap between groups A and B = 92%",
              #                          `40.0.92` = "Overlap between groups A and B = 92%",
              #                          `80.0.92` = "Overlap between groups A and B = 92%",
              #                          `10.0.96` = "Overlap between groups A and B = 96%",
              #                          `20.0.96` = "Overlap between groups A and B = 96%",
              #                          `40.0.96` = "Overlap between groups A and B = 96%",
              #                          `80.0.96` = "Overlap between groups A and B = 96%")),
              scales = "free",
              strip = strip_nested( #basiert auf strip_themed, deswegen geändert
                #macht nicht, was es soll, weil es glaube ich auf den vars, nach denen faccetiert wird basiert, nicht auf den tatsächlichen labeln in der Grafik
                #-> vars an overlaplabel anpassen!
                background_x = elem_list_rect(fill = c("#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c",
                                                       "#fbda66","#ef9c47","#e55e2c","#88432c")))
  ) +
  ylab("Value"
  ) +
  # geom_text(y = 12, x = 2,
  #           mapping = aes(label = txt),hjust = 0,
  #           fontface = "bold", color = "black",
  # ) +
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


#### Single graph ####

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

#### rest ####

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