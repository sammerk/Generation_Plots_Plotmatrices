#### Merging evidence ####

library(tidyverse)
library(plyr)

evidence_merged <- 
  full_join(evidence_sparklies_faster, evidence_women_taller) %>%
  full_join(., evidence_men_taller) %>%
  full_join(., evidence_teachers_more) %>%
  full_join(., evidence_dentists_more)

write_csv(evidence_merged, "evidence_merged.csv")
