library(bayestestR) # for perfect distributions
library(tidyverse) # for everything
library(patchwork) # for the combination of plots
library(ggdist)
library(hrbrthemes)
library(bain)
library(ggbeeswarm)
library(ggh4x)
library(BayesFactor)


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
sample_sizes_7 <- c(12, 24, 42, 79)
effect_sizes <- c(.1, .2, .5, .8)



# Initialize empty data frames
#sparklies jinglies
data_jinglies_fasterthan_sparklies <- tibble(
  Jinglies = numeric(0),
  Sparklies = numeric(0),
  `Sample Size` = numeric(0),
  cohen_d = numeric(0),
  `Overlap` = numeric(0)
)

data_sparklies_fasterthan_jinglies <- tibble(
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

### Jinglies & Sparklies 

#Sparklies faster

# loop over sample sizes jinglies sparklies
for(i in sample_sizes_1){
  # loop over effect sizes
  for(j in effect_sizes){
    data_sparklies_fasterthan_jinglies <- 
      full_join(data_sparklies_fasterthan_jinglies,
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

data_sparklies_fasterthan_jinglies %>% 
  skimr::skim()

# Inspect the evidence jinglies sparklies 
evidence_sparklies_faster <-
data_sparklies_fasterthan_jinglies %>% 
  pivot_longer(c(Jinglies, Sparklies), names_to = "Group", values_to = "value") %>%
  nest_by(`Sample Size`, cohen_d) %>% 
  mutate(PmP = bain(t_test(value ~ Group, data = data), 
                    hypothesis = "groupJinglies > groupSparklies")$fit$PMPc[1],
         AABF = bain(t_test(value ~ Group, data = data), 
                     hypothesis = "groupJinglies > groupSparklies")$fit$BF[1],
         JZSBF = extractBF(ttestBF(formula = value ~ Group, data = data))$bf,
         pval = t.test(value ~ Group, data = data)$p.value,
         cohenD = effsize::cohen.d(value ~ Group, data = data)$estimate,
         matrix = "sparklies_faster")

write_csv(evidence_sparklies_faster, "evidence_sparklies_faster.csv")

#Jinglies faster

# loop over sample sizes jinglies sparklies
for(i in sample_sizes_7){
  # loop over effect sizes
  for(j in effect_sizes){
    data_jinglies_fasterthan_sparklies <- 
      full_join(data_jinglies_fasterthan_sparklies,
                tibble(Sparklies = distribution_normal(i, 
                                                      average_jsp+j*0.5*sd_jsp, 
                                                      sd_jsp),
                       Jinglies = distribution_normal(i, 
                                                       average_jsp-j*0.5*sd_jsp, 
                                                       sd_jsp),
                       `Sample Size` = i,
                       cohen_d = j,
                       Overlap = round(2*pnorm(-j/2), 2)
                ))
  }
}


### height

# loop over sample sizes height - women taller
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

# Inspect the evidence height - women taller 
evidence_women_taller <- 
  data_women_tallerthan_men %>% 
  pivot_longer(c(Women, Men), names_to = "Group", values_to = "value") %>%
  nest_by(`Sample Size`, cohen_d) %>% 
  mutate(PmP = bain(t_test(value ~ Group, data = data), 
                    hypothesis = "groupWomen > groupMen")$fit$PMPc[1],
         AABF = bain(t_test(value ~ Group, data = data), 
                     hypothesis = "groupWomen > groupMen")$fit$BF[1],
         JZSBF = extractBF(ttestBF(formula = value ~ Group, data = data))$bf,
         pval = t.test(value ~ Group, data = data)$p.value,
         cohenD = effsize::cohen.d(value ~ Group, data = data)$estimate,
         matrix = "women_taller") 

write_csv(evidence_women_taller, "evidence_women_taller.csv")

# loop over sample sizes height - men taller
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

# Inspect the evidence height - men taller
evidence_men_taller <-
data_men_tallertan_women %>% 
  gather(Group, value, Women, Men) %>% 
  nest_by(`Sample Size`, cohen_d) %>% 
  mutate(PmP = bain(t_test(value ~ Group, data = data), 
                    hypothesis = "groupMen > groupWomen")$fit$PMPc[1],
         AABF = bain(t_test(value ~ Group, data = data), 
                     hypothesis = "groupMen > groupWomen")$fit$BF[1],
         JZSBF = extractBF(ttestBF(formula = value ~ Group, data = data))$bf,
         pval = t.test(value ~ Group, data = data)$p.value,
         cohenD = effsize::cohen.d(value ~ Group, data = data)$estimate,
         matrix = "men_taller") 

write_csv(evidence_men_taller, "evidence_men_taller.csv")



### salary

# loop over sample sizes salary - dentists more
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

# Inspect the evidence salary - dentists more 
evidence_dentists_more <-
data_dentists_morethan_teachers %>% 
  pivot_longer(c(Dentists, Primary_Teachers), names_to = "Group", values_to = "value") %>%
  nest_by(`Sample Size`, cohen_d) %>% 
  mutate(PmP = bain(t_test(value ~ Group, data = data), 
                    hypothesis = "groupDentists > groupPrimary_Teachers")$fit$PMPc[1],
         AABF = bain(t_test(value ~ Group, data = data), 
                     hypothesis = "groupDentists > groupPrimary_Teachers")$fit$BF[1],
         JZSBF = extractBF(ttestBF(formula = value ~ Group, data = data))$bf,
         pval = t.test(value ~ Group, data = data)$p.value,
         cohenD = effsize::cohen.d(value ~ Group, data = data)$estimate,
         matrix = "dentists_more")

write_csv(evidence_dentists_more, "evidence_dentists_more.csv")

# loop over sample sizes salary - teachers more
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

# Inspect the evidence salary - teachers more 
evidence_teachers_more <-
data_teachers_morethan_dentists %>% 
  pivot_longer(c(Dentists, Primary_Teachers), names_to = "Group", values_to = "value") %>%
  nest_by(`Sample Size`, cohen_d) %>% 
  mutate(PmP = bain(t_test(value ~ Group, data = data), 
                    hypothesis = "groupPrimary_Teachers > groupDentists")$fit$PMPc[1],
         AABF = bain(t_test(value ~ Group, data = data), 
                     hypothesis = "groupPrimary_Teachers > groupDentists")$fit$BF[1],
         JZSBF = extractBF(ttestBF(formula = value ~ Group, data = data))$bf,
         pval = t.test(value ~ Group, data = data)$p.value,
         cohenD = effsize::cohen.d(value ~ Group, data = data)$estimate,
         matrix = "teachers_more")

write_csv(evidence_teachers_more, "evidence_teachers_more.csv")


#### Matrix mit overlapping overlaplabel gefüllt angepasst für salary dentists more TRANSPOx2 ####

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
    cex = 1.25,
    alpha = 2/5
  ) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 8, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`15` = "Group size = 15",
                                                               `26` = "Group size = 26",
                                                               `41` = "Group size = 41",
                                                               `85` = "Group size = 85")),
                                  Overlap = as_labeller(c(`0.69` = "Difference between the groups = 31% (69% overlap)",
                                                          `0.8` = "Difference between the groups = 20% (80% overlap)",
                                                          `0.92` = "Difference between the groups = 8% (92% overlap)",
                                                          `0.96` = "Difference between the groups = 4% (96% overlap)"))),
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
                                        face = c("bold","bold","bold","bold"
                                                 )))
  ) +
  ylab("Monthly salary in EUR"
  ) +
  xlab("Profession"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.text.y = element_text(size = 6.5, color = "black"),
        axis.text.x = element_text(size = 6.5, color = "black") #, face = "bold", angle = X)
        
  ) +
  scale_x_discrete(labels=c("Dentists" = "Dentists", "Primary_Teachers" = "Primary Teachers")
  ) +
  ylim(2000,6000)

ggsave(paste("demo_plots/matrices", 
       "dentists",
       "more",
       ".svg",
       sep = "_"),
       dpi = 300,
       width = 2000,
       height = 1900,
       units = "px")


#### Matrix mit overlapping overlaplabel gefüllt angepasst für salary teachers more ####

ggplot(data_teachers_morethan_dentists %>% 
         gather(Group, value, Dentists, Primary_Teachers) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 1.25,
    alpha = 2/5
  ) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 8, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`10` = "Group size = 10",
                                                               `23` = "Group size = 23",
                                                               `39` = "Group size = 39",
                                                               `81` = "Group size = 81")),
                                  Overlap = as_labeller(c(`0.69` = "Difference between the groups = 31% (69% overlap)",
                                                          `0.8` = "Difference between the groups = 20% (80% overlap)",
                                                          `0.92` = "Difference between the groups = 8% (92% overlap)",
                                                          `0.96` = "Difference between the groups = 4% (96% overlap)"))),
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
                                        face = c("bold","bold","bold","bold"
                                                 )))
  ) +
  ylab("Monthly salary in EUR"
  ) +
  xlab("Profession"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.text.y = element_text(size = 6.5, color = "black"),
        axis.text.x = element_text(size = 6.5, color = "black") #, face = "bold", angle = X)
  ) +
  scale_x_discrete(labels=c("Dentists" = "Dentists", "Primary_Teachers" = "Primary Teachers")
  ) +
  ylim(2000,6000)

ggsave(paste("demo_plots/matrices", 
             "teachers",
             "more",
             ".svg",
             sep = "_"),
       dpi = 300,
       width = 2000,
       height = 1900,
       units = "px")

#### Matrix mit overlapping overlaplabel gefüllt angepasst für jinglies faster ####


ggplot(data_jinglies_fasterthan_sparklies %>% 
         gather(Group, value, Jinglies, Sparklies) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 1.25,
    alpha = 2/5
  ) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 95, size = 8, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`12` = "Group size = 12",
                                                               `24` = "Group size = 24",
                                                               `42` = "Group size = 42",
                                                               `79` = "Group size = 79")),
                                  Overlap = as_labeller(c(`0.69` = "Difference between the groups = 31% (69% overlap)",
                                                          `0.8` = "Difference between the groups = 20% (80% overlap)",
                                                          `0.92` = "Difference between the groups = 8% (92% overlap)",
                                                          `0.96` = "Difference between the groups = 4% (96% overlap)"))),
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
                                        face = c("bold","bold","bold","bold"
                                        )))
  ) +
  ylab("Minutes spent on building toy"
  ) +
  xlab("Type of Elf"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black")
  ) +
  ylim(20,80)

ggsave(paste("demo_plots/matrices", 
             "jinglies",
             "superior",
             ".svg",
             sep = "_"),
       dpi = 300,
       width = 2000,
       height = 1900,
       units = "px")



#### Matrix mit overlapping overlaplabel gefüllt angepasst für sparklies faster TRANSPOx1 ####

#transpo
data_sparklies_fasterthan_jinglies$`Sample Size_f` <- factor(data_sparklies_fasterthan_jinglies$`Sample Size`, levels=c(74,37,25,11))


ggplot(data_sparklies_fasterthan_jinglies %>% 
         gather(Group, value, Jinglies, Sparklies) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size_f`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 1.25,
    alpha = 2/5
  ) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 8, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`11` = "Group size = 11",
                                                               `25` = "Group size = 25",
                                                               `37` = "Group size = 37",
                                                               `74` = "Group size = 74")),
                                  Overlap = as_labeller(c(`0.69` = "Difference between the groups = 31% (69% overlap)",
                                                          `0.8` = "Difference between the groups = 20% (80% overlap)",
                                                          `0.92` = "Difference between the groups = 8% (92% overlap)",
                                                          `0.96` = "Difference between the groups = 4% (96% overlap)"))),
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
                                        face = c("bold","bold","bold","bold"
                                                 )))
  ) +
  ylab("Minutes spent on building toy"
  ) +
  xlab("Type of Elf"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black")
  ) +
  ylim(20,80)

ggsave(paste("demo_plots/matrices", 
             "sparklies",
             "superior",
             ".svg",
             sep = "_"),
       dpi = 300,
       width = 2000,
       height = 1900,
       units = "px")


#### Matrix mit overlapping overlaplabel gefüllt angepasst für women taller TRANSPOx2 ####

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
    cex = 1.25,
    alpha = 2/5
    ) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 8, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`9` = "Group size = 9",
                                                               `22` = "Group size = 22",
                                                               `40` = "Group size = 40",
                                                               `75` = "Group size = 75")),
                                  Overlap = as_labeller(c(`0.69` = "Difference between the groups = 31% (69% overlap)",
                                                          `0.8` = "Difference between the groups = 20% (80% overlap)",
                                                          `0.92` = "Difference between the groups = 8% (92% overlap)",
                                                          `0.96` = "Difference between the groups = 4% (96% overlap)"))),
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
                                        face = c("bold","bold","bold","bold"
                                                 )))
  ) +
  ylab("Height in centimeters"
  ) +
  xlab("Gender"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black")
  ) +
  ylim(130,200)

ggsave(paste("demo_plots/matrices", 
             "women",
             "taller",
             ".svg",
             sep = "_"),
       dpi = 300,
       width = 2000,
       height = 1900,
       units = "px")



#### Matrix mit overlapping overlaplabel gefüllt angepasst für men taller ####

ggplot(data_men_tallertan_women %>% 
         gather(Group, value, Women, Men) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 1.25,
    alpha = 2/5
  ) +
  stat_summary(
    fun = mean, geom = "point", 
    shape = 95, size = 8, color = "black"
  ) +
  facet_wrap2(vars(Overlap ,`Group Size`),
              labeller = labeller(`Group Size` = as_labeller(c(`14` = "Group size = 14",
                                                               `21` = "Group size = 21",
                                                               `38` = "Group size = 38",
                                                               `79` = "Group size = 79")),
                                  Overlap = as_labeller(c(`0.69` = "Difference between the groups = 31% (69% overlap)",
                                                          `0.8` = "Difference between the groups = 20% (80% overlap)",
                                                          `0.92` = "Difference between the groups = 8% (92% overlap)",
                                                          `0.96` = "Difference between the groups = 4% (96% overlap)"))),
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
                                        face = c("bold","bold","bold","bold"
                                                 )))
  ) +
  ylab("Height in centimeters"
  ) +
  xlab("Gender"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black")
  ) +
  ylim(130,200)

ggsave(paste("demo_plots/matrices", 
             "men",
             "taller",
             ".svg",
             sep = "_"),
       dpi = 300,
       width = 2000,
       height = 1900,
       units = "px")

#### Single graphs for single graph study ####

dentists_more_ol96_ss85 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.96 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 85,]
dentists_more_ol96_ss41 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.96 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 41,]
dentists_more_ol96_ss26 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.96 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 26,]
dentists_more_ol96_ss15 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.96 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 15,]
#
dentists_more_ol92_ss85 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.92 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 85,]
dentists_more_ol92_ss41 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.92 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 41,]
dentists_more_ol92_ss26 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.92 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 26,]
dentists_more_ol92_ss15 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.92 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 15,]
#
dentists_more_ol80_ss85 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.8 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 85,]
dentists_more_ol80_ss41 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.8 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 41,]
dentists_more_ol80_ss26 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.8 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 26,]
dentists_more_ol80_ss15 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.8 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 15,]
#
dentists_more_ol69_ss85 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.69 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 85,]
dentists_more_ol69_ss41 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.69 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 41,]
dentists_more_ol69_ss26 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.69 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 26,]
dentists_more_ol69_ss15 <- data_dentists_morethan_teachers[data_dentists_morethan_teachers$Overlap == 0.69 & 
                                                             data_dentists_morethan_teachers$`Sample Size` == 15,]
##
teachers_more_ol69_ss10 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.69 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 10,]
teachers_more_ol69_ss23 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.69 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 23,]
teachers_more_ol69_ss39 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.69 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 39,]
teachers_more_ol69_ss81 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.69 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 81,]
#
teachers_more_ol80_ss10 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.8 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 10,]
teachers_more_ol80_ss23 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.8 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 23,]
teachers_more_ol80_ss39 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.8 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 39,]
teachers_more_ol80_ss81 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.8 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 81,]
#
teachers_more_ol92_ss10 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.92 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 10,]
teachers_more_ol92_ss23 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.92 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 23,]
teachers_more_ol92_ss39 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.92 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 39,]
teachers_more_ol92_ss81 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.92 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 81,]
#
teachers_more_ol96_ss10 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.96 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 10,]
teachers_more_ol96_ss23 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.96 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 23,]
teachers_more_ol96_ss39 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.96 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 39,]
teachers_more_ol96_ss81 <- data_teachers_morethan_dentists[data_teachers_morethan_dentists$Overlap == 0.96 & 
                                                             data_teachers_morethan_dentists$`Sample Size` == 81,]
###
men_taller_ol69_ss14 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.69 & 
                                                   data_men_tallertan_women$`Sample Size` == 14,]
men_taller_ol69_ss21 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.69 & 
                                                   data_men_tallertan_women$`Sample Size` == 21,]
men_taller_ol69_ss38 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.69 & 
                                                   data_men_tallertan_women$`Sample Size` == 38,]
men_taller_ol69_ss79 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.69 & 
                                                   data_men_tallertan_women$`Sample Size` == 79,]
#
men_taller_ol80_ss14 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.8 & 
                                                   data_men_tallertan_women$`Sample Size` == 14,]
men_taller_ol80_ss21 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.8 & 
                                                   data_men_tallertan_women$`Sample Size` == 21,]
men_taller_ol80_ss38 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.8 & 
                                                   data_men_tallertan_women$`Sample Size` == 38,]
men_taller_ol80_ss79 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.8 & 
                                                   data_men_tallertan_women$`Sample Size` == 79,]
#
men_taller_ol92_ss14 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.92 & 
                                                   data_men_tallertan_women$`Sample Size` == 14,]
men_taller_ol92_ss21 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.92 & 
                                                   data_men_tallertan_women$`Sample Size` == 21,]
men_taller_ol92_ss38 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.92 & 
                                                   data_men_tallertan_women$`Sample Size` == 38,]
men_taller_ol92_ss79 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.92 & 
                                                   data_men_tallertan_women$`Sample Size` == 79,]
#
men_taller_ol96_ss14 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.96 & 
                                                  data_men_tallertan_women$`Sample Size` == 14,]
men_taller_ol96_ss21 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.96 & 
                                                  data_men_tallertan_women$`Sample Size` == 21,]
men_taller_ol96_ss38 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.96 & 
                                                   data_men_tallertan_women$`Sample Size` == 38,]
men_taller_ol96_ss79 <- data_men_tallertan_women[data_men_tallertan_women$Overlap == 0.96 & 
                                                   data_men_tallertan_women$`Sample Size` == 79,]
##
women_taller_ol96_ss75 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.96 & 
                                                      data_women_tallerthan_men$`Sample Size` == 75,]
women_taller_ol96_ss40 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.96 & 
                                                      data_women_tallerthan_men$`Sample Size` == 40,]
women_taller_ol96_ss22 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.96 & 
                                                      data_women_tallerthan_men$`Sample Size` == 22,]
women_taller_ol96_ss9 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.96 & 
                                                     data_women_tallerthan_men$`Sample Size` == 9,]
#
women_taller_ol92_ss75 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.92 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 75,]
women_taller_ol92_ss40 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.92 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 40,]
women_taller_ol92_ss22 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.92 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 22,]
women_taller_ol92_ss9 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.92 & 
                                                                               data_women_tallerthan_men$`Sample Size` == 9,]
#
women_taller_ol80_ss75 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.8 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 75,]
women_taller_ol80_ss40 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.8 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 40,]
women_taller_ol80_ss22 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.8 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 22,]
women_taller_ol80_ss9 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.8 & 
                                                                               data_women_tallerthan_men$`Sample Size` == 9,]
#
women_taller_ol69_ss75 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.69 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 75,]
women_taller_ol69_ss40 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.69 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 40,]
women_taller_ol69_ss22 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.69 & 
                                                                                data_women_tallerthan_men$`Sample Size` == 22,]
women_taller_ol69_ss9 <- data_women_tallerthan_men[data_women_tallerthan_men$Overlap == 0.69 & 
                                                                               data_women_tallerthan_men$`Sample Size` == 9,]
###
jinglies_superior_ol69_ss12 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.69 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 12,]
jinglies_superior_ol69_ss24 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.69 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 24,]
jinglies_superior_ol69_ss42 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.69 & 
                                                                   data_jinglies_fasterthan_sparklies$`Sample Size` == 42,]
jinglies_superior_ol69_ss79 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.69 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 79,]
#
jinglies_superior_ol80_ss12 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.8 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 12,]
jinglies_superior_ol80_ss24 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.8 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 24,]
jinglies_superior_ol80_ss42 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.8 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 42,]
jinglies_superior_ol80_ss79 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.8 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 79,]
#
jinglies_superior_ol92_ss12 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.92 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 12,]
jinglies_superior_ol92_ss24 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.92 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 24,]
jinglies_superior_ol92_ss42 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.92 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 42,]
jinglies_superior_ol92_ss79 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.92 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 79,]
#
jinglies_superior_ol96_ss12 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.96 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 12,]
jinglies_superior_ol96_ss24 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.96 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 24,]
jinglies_superior_ol96_ss42 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.96 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 42,]
jinglies_superior_ol96_ss79 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.96 & 
                                                                    data_jinglies_fasterthan_sparklies$`Sample Size` == 79,]
###

#big list of all those objects above:
test_list <- c(
  dentists_more_ol96_ss85, dentists_more_ol96_ss41, dentists_more_ol96_ss26, dentists_more_ol96_ss15,
  dentists_more_ol92_ss85, dentists_more_ol92_ss41, dentists_more_ol92_ss26, dentists_more_ol92_ss15,
  dentists_more_ol80_ss85, dentists_more_ol80_ss41, dentists_more_ol80_ss26, dentists_more_ol80_ss15,
  dentists_more_ol69_ss85, dentists_more_ol69_ss41, dentists_more_ol69_ss26, dentists_more_ol69_ss15,
  ##
  teachers_more_ol69_ss10, teachers_more_ol69_ss23, teachers_more_ol69_ss39, teachers_more_ol69_ss81, 
  teachers_more_ol80_ss10, teachers_more_ol80_ss23, teachers_more_ol80_ss39, teachers_more_ol80_ss81, 
  teachers_more_ol92_ss10, teachers_more_ol92_ss23, teachers_more_ol92_ss39, teachers_more_ol92_ss81, 
  teachers_more_ol96_ss10, teachers_more_ol96_ss23, teachers_more_ol96_ss39, teachers_more_ol96_ss81, 
  ###
  men_taller_ol69_ss14, men_taller_ol69_ss21, men_taller_ol69_ss38, men_taller_ol69_ss79, 
  men_taller_ol80_ss14, men_taller_ol80_ss21, men_taller_ol80_ss38, men_taller_ol80_ss79, 
  men_taller_ol92_ss14, men_taller_ol92_ss21, men_taller_ol92_ss38, men_taller_ol92_ss79,
  men_taller_ol96_ss14, men_taller_ol96_ss21, men_taller_ol96_ss38, men_taller_ol96_ss79,
  ##
  women_taller_ol96_ss75, women_taller_ol96_ss40, women_taller_ol96_ss22, women_taller_ol96_ss9, 
  women_taller_ol92_ss75, women_taller_ol92_ss40, women_taller_ol92_ss22, women_taller_ol92_ss9,
  women_taller_ol80_ss75, women_taller_ol80_ss40, women_taller_ol80_ss22, women_taller_ol80_ss9,
  women_taller_ol69_ss75, women_taller_ol69_ss40, women_taller_ol69_ss22, women_taller_ol69_ss9,
  ###
  jinglies_superior_ol69_ss12, jinglies_superior_ol69_ss24, jinglies_superior_ol69_ss42, jinglies_superior_ol69_ss79,
  jinglies_superior_ol80_ss12, jinglies_superior_ol80_ss24, jinglies_superior_ol80_ss42, jinglies_superior_ol80_ss79,
  jinglies_superior_ol92_ss12, jinglies_superior_ol92_ss24, jinglies_superior_ol92_ss42, jinglies_superior_ol92_ss79,
  jinglies_superior_ol96_ss12, jinglies_superior_ol96_ss24, jinglies_superior_ol96_ss42, jinglies_superior_ol96_ss79
  ###
  
)


#big function:
epic_function <- function(subset) {
  
subset_title_string <- deparse(substitute(subset))
  
  #if subset starts with  x, then make xlab and ylab this
  if (startsWith(subset_title_string, "jinglies")) { 
    x_lab <- "Type of Elf"
    y_lab <- "Minutes spent on building toy"
    gather_placeholder1 <- "Jinglies"
    gather_placeholder2 <- "Sparklies"
    ylim_par1 <-20
    ylim_par2 <-80
  } else if (startsWith(subset_title_string, "women")) {
    x_lab <- "Gender"
    y_lab <- "Height in centimeters"
    gather_placeholder1 <- "Women"
    gather_placeholder2 <- "Men"
    ylim_par1 <-130
    ylim_par2 <-200
  } else if  (startsWith(subset_title_string, "men")) {
    x_lab <- "Gender"
    y_lab <- "Height in centimeters"
    gather_placeholder1 <- "Women"
    gather_placeholder2 <- "Men"
    ylim_par1 <-130
    ylim_par2 <-200
  } else if (startsWith(subset_title_string, "dentists")) {
    x_lab <- "Profession"
    y_lab <- "Monthly salary in EUR"
    gather_placeholder1 <- "Dentists"
    gather_placeholder2 <- "Primary_Teachers"
    ylim_par1 <-2000
    ylim_par2 <-6000
  } else if (startsWith(subset_title_string, "teachers")) {
    x_lab <- "Profession"
    y_lab <- "Monthly salary in EUR"
    gather_placeholder1 <- "Dentists"
    gather_placeholder2 <- "Primary_Teachers"
    ylim_par1 <-2000
    ylim_par2 <-6000
  }

#if olXX, then change subtitle accordingly (including difference)
if (str_detect(subset_title_string, "ol69")) { 
  overlap_placeholder <- "69"
  difference_placeholder <- "31"
} else if (str_detect(subset_title_string, "ol80")) {
  overlap_placeholder <- "80"
  difference_placeholder <- "20"
} else if (str_detect(subset_title_string, "ol92")) {
  overlap_placeholder <- "92"
  difference_placeholder <- "8"
}  else {
  overlap_placeholder <- "96"
  difference_placeholder <- "4"
}

#if ssXX, then change subtitle accordingly

group_size_placeholder <- sub(".*ss", "", subset_title_string)


#ggplot
if (gather_placeholder2 == "Primary_Teachers") {
  ggplot(subset %>% 
           gather(Group, value, gather_placeholder1, gather_placeholder2) %>% 
           mutate(Overlap = as.factor(Overlap),
                  `Group Size` = as.factor(`Sample Size`)), 
         aes(Group, value)
  ) +
    geom_quasirandom(
      colour = "#848484",
      cex = 4,
      alpha = 2/5
    ) +
    stat_summary(
      fun = mean, geom = "point",
      shape = 95, size = 15, color = "black"
    ) +
    ylab(y_lab
    ) +
    xlab(x_lab
    ) +
    labs(
      title = paste("Difference between the groups = ",difference_placeholder,"% (",overlap_placeholder,"% overlap)", sep = ""),
      subtitle = paste("Group size = ",group_size_placeholder, sep = "")
    ) +
    theme(panel.spacing = unit(0.10, "cm"),
          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"),
          strip.text.x = element_text(size = 8),
          axis.title.y = element_text(size = 12, color = "black", face = "bold"),
          axis.title.x = element_text(size = 12, color = "black", face = "bold"),
          axis.text.x = element_text(size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 12, face = "bold")
    ) +
    scale_x_discrete(labels=c("Dentists" = "Dentists", "Primary_Teachers" = "Primary Teachers"
    )) +
    ylim(ylim_par1,ylim_par2)
} else {
  ggplot(subset %>% 
           gather(Group, value, gather_placeholder1, gather_placeholder2) %>% 
           mutate(Overlap = as.factor(Overlap),
                  `Group Size` = as.factor(`Sample Size`)), 
         aes(Group, value)
  ) +
    geom_quasirandom(
      colour = "#848484",
      cex = 4,
      alpha = 2/5
    ) +
    stat_summary(
      fun = mean, geom = "point",
      shape = 95, size = 15, color = "black"
    ) +
    ylab(y_lab
    ) +
    xlab(x_lab
    ) +
    labs(
      title = paste("Difference between the groups = ",difference_placeholder,"% (",overlap_placeholder,"% overlap)", sep = ""),
      subtitle = paste("Group size = ",group_size_placeholder, sep = "")
    ) +
    theme(panel.spacing = unit(0.10, "cm"),
          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"),
          strip.text.x = element_text(size = 8),
          axis.title.y = element_text(size = 13, color = "black", face = "bold"),
          axis.title.x = element_text(size = 13, color = "black", face = "bold"),
          axis.text.x = element_text(size = 13, color = "black"),
          axis.text.y = element_text(size = 13, color = "black"),
          plot.subtitle = element_text(size = 13, face = "bold"),
          plot.title = element_text(size = 13, face = "bold")
    ) +
    ylim(ylim_par1,ylim_par2)
}

ggsave(paste("demo_plots/single_plots", 
             subset_title_string,
             ".svg",
             sep = "_"),
       dpi = 100,
       width = 500,
       height = 450,
       units = "px")

}

epic_function(dentists_more_ol96_ss85)
epic_function(dentists_more_ol96_ss41)
epic_function(dentists_more_ol96_ss26)
epic_function(dentists_more_ol96_ss15)
#
epic_function(dentists_more_ol92_ss85)
epic_function(dentists_more_ol92_ss41)
epic_function(dentists_more_ol92_ss26)
epic_function(dentists_more_ol92_ss15)
#
epic_function(dentists_more_ol80_ss85)
epic_function(dentists_more_ol80_ss41)
epic_function(dentists_more_ol80_ss26)
epic_function(dentists_more_ol80_ss15)
#
epic_function(dentists_more_ol69_ss85)
epic_function(dentists_more_ol69_ss41)
epic_function(dentists_more_ol69_ss26)
epic_function(dentists_more_ol69_ss15)
##
epic_function(teachers_more_ol69_ss10)
epic_function(teachers_more_ol69_ss23)
epic_function(teachers_more_ol69_ss39)
epic_function(teachers_more_ol69_ss81)
#
epic_function(teachers_more_ol80_ss10)
epic_function(teachers_more_ol80_ss23)
epic_function(teachers_more_ol80_ss39)
epic_function(teachers_more_ol80_ss81)
#
epic_function(teachers_more_ol92_ss10)
epic_function(teachers_more_ol92_ss23)
epic_function(teachers_more_ol92_ss39)
epic_function(teachers_more_ol92_ss81)
#
epic_function(teachers_more_ol96_ss10)
epic_function(teachers_more_ol96_ss23)
epic_function(teachers_more_ol96_ss39)
epic_function(teachers_more_ol96_ss81)
###
epic_function(men_taller_ol69_ss14)
epic_function(men_taller_ol69_ss21)
epic_function(men_taller_ol69_ss38)
epic_function(men_taller_ol69_ss79)
#
epic_function(men_taller_ol80_ss14)
epic_function(men_taller_ol80_ss21)
epic_function(men_taller_ol80_ss38)
epic_function(men_taller_ol80_ss79)
#
epic_function(men_taller_ol92_ss14)
epic_function(men_taller_ol92_ss21)
epic_function(men_taller_ol92_ss38)
epic_function(men_taller_ol92_ss79)
#
epic_function(men_taller_ol96_ss14)
epic_function(men_taller_ol96_ss21)
epic_function(men_taller_ol96_ss38)
epic_function(men_taller_ol96_ss79)
##
epic_function(women_taller_ol96_ss75)
epic_function(women_taller_ol96_ss40)
epic_function(women_taller_ol96_ss22)
epic_function(women_taller_ol96_ss9)
#
epic_function(women_taller_ol92_ss75)
epic_function(women_taller_ol92_ss40)
epic_function(women_taller_ol92_ss22)
epic_function(women_taller_ol92_ss9)
#
epic_function(women_taller_ol80_ss75)
epic_function(women_taller_ol80_ss40)
epic_function(women_taller_ol80_ss22)
epic_function(women_taller_ol80_ss9)
#
epic_function(women_taller_ol69_ss75)
epic_function(women_taller_ol69_ss40)
epic_function(women_taller_ol69_ss22)
epic_function(women_taller_ol69_ss9)
###
epic_function(jinglies_superior_ol69_ss12)
epic_function(jinglies_superior_ol69_ss24)
epic_function(jinglies_superior_ol69_ss42)
epic_function(jinglies_superior_ol69_ss79)
#
epic_function(jinglies_superior_ol80_ss12)
epic_function(jinglies_superior_ol80_ss24)
epic_function(jinglies_superior_ol80_ss42)
epic_function(jinglies_superior_ol80_ss79)
#
epic_function(jinglies_superior_ol92_ss12)
epic_function(jinglies_superior_ol92_ss24)
epic_function(jinglies_superior_ol92_ss42)
epic_function(jinglies_superior_ol92_ss79)
#
epic_function(jinglies_superior_ol96_ss12)
epic_function(jinglies_superior_ol96_ss24)
epic_function(jinglies_superior_ol96_ss42)
epic_function(jinglies_superior_ol96_ss79)

#### Single graphs for intro ####

# create subsets of data for each plot:

jinglies_faster_ol69_ss42 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.69 & 
                                                              data_jinglies_fasterthan_sparklies$`Sample Size` == 42,]
sparklies_faster_ol92_ss25 <- data_sparklies_fasterthan_jinglies[data_sparklies_fasterthan_jinglies$Overlap == 0.92 & 
                                                                   data_sparklies_fasterthan_jinglies$`Sample Size` == 25,]
jinglies_faster_ol80_ss_79 <- data_jinglies_fasterthan_sparklies[data_jinglies_fasterthan_sparklies$Overlap == 0.8 & 
                                                                   data_jinglies_fasterthan_sparklies$`Sample Size` == 79,]



ggplot(jinglies_faster_ol69_ss42 %>% 
         gather(Group, value, Jinglies, Sparklies) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 4,
    alpha = 2/5
  ) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 95, size = 15, color = "black"
  ) +
  ylab("Minutes spent on building toy"
  ) +
  xlab("Type of Elf"
  ) +
  labs(
    title = "Graph 1:",
    subtitle = "Difference between the groups = 31% (69% overlap)\nGroup size = 42"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"),
        plot.subtitle = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")
  ) +
  ylim(20,80)

ggsave(paste("demo_plots/matrices", 
             "jinglies_faster_ol69_ss42",
             ".svg",
             sep = "_"),
       dpi = 100,
       width = 500,
       height = 450,
       units = "px")


ggplot(sparklies_faster_ol92_ss25 %>% 
         relocate(Jinglies, .before = Sparklies) %>%
         gather(Group, value, Jinglies, Sparklies) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 4,
    alpha = 2/5
  ) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 95, size = 15, color = "black"
  ) +
  ylab("Minutes spent on building toy"
  ) +
  xlab("Type of Elf"
  ) +
  labs(
    title = "Graph 2:",
    subtitle = "Difference between the groups = 8% (92% overlap)\nGroup size = 25"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"),
        plot.subtitle = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")
  ) +
  ylim(20,80)

ggsave(paste("demo_plots/matrices", 
             "sparklies_faster_ol92_ss25",
             ".svg",
             sep = "_"),
       dpi = 100,
       width = 500,
       height = 450,
       units = "px")



ggplot(jinglies_faster_ol80_ss_79 %>% 
         gather(Group, value, Jinglies, Sparklies) %>% 
         mutate(Overlap = as.factor(Overlap),
                `Group Size` = as.factor(`Sample Size`)), 
       aes(Group, value)
) +
  geom_quasirandom(
    colour = "#848484",
    cex = 4,
    alpha = 2/5
  ) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 95, size = 15, color = "black"
  ) +
  ylab("Minutes spent on building toy"
  ) +
  xlab("Type of Elf"
  ) +
  labs(
    title = "Graph 3:",
    subtitle = "Difference between the groups = 20% (80% overlap)\nGroup size = 79"
  ) +
  theme(panel.spacing = unit(0.10, "cm"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 13, color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"),
        plot.subtitle = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 13, face = "bold")
  ) +
  ylim(20,80)

ggsave(paste("demo_plots/matrices", 
             "jinglies_faster_ol80_ss_79",
             ".svg",
             sep = "_"),
       dpi = 100,
       width = 500,
       height = 450,
       units = "px")


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
