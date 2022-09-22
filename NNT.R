library(tidyverse)
library(readxl)
library(ggsci)

NNT <- read_excel("NNT.xlsx", sheet = "Sheet1") 

NNT %>%
  ggplot()+
  geom_line(aes(x= pct_omicron, y=NNT, group = Priority, color = Priority), size=3) +
  coord_cartesian(xlim = c(0, 1), ylim= c(0, 50)) +
  scale_color_simpsons() +
  labs(title="Number Needed to Treat in Setting of mAb-Resistant Variant",
       subtitle = "Assuming no activity against resistant variant",
       x = "Prevalence of Resistant Variant (eg, Omicron)") +
  theme_minimal() 

ggsave("NNT in rising resistant variant prev.pdf", height=8, width=10)