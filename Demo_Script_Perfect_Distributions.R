library(bayestestR) # for perfect distributions
library(tidyverse) # for everything
library(patchwork) # for the combination of plots
library(ggdist)
library(hrbrthemes)

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
                       B = case_when(T ~ A - j*sd_group_A),
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
  geom_dots(side = "both") + 
  facet_grid(Overlap ~ `Sample Size`) + 
  theme_ipsum_rc()
  
# Inspect the evidence 
data %>% 
  gather(Group, value, A, B) %>% 
  nest_by(`Sample Size`, cohen_d) %>% 
  mutate(PmP = bain(t_test(value ~ Group, data = data), hypothesis = "groupA > groupB")$fit$PMPc[1],
         BF = bain(t_test(value ~ Group, data = data), hypothesis = "groupA > groupB")$fit$BF[1],
         BFBF = extractBF(ttestBF(formula = value ~ Group, data = data))$bf,
         pval = t.test(value ~ Group, data = data)$p.value)
