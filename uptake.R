library(tidyverse)
d0 <- read_csv("uptake_data_all.csv")
head(d0)

d <- d0 %>% mutate(
  tentacle = as.factor(ifelse(grepl("t",type),"tentacle","no_tentacle")),
                       symbiotic = as.factor(ifelse(grepl("s",sym),"yes","no")),
                                             recruit = as.factor(ifelse(grepl("s",swim),"recruit","larvae")))
head(d)

d %>% ungroup() %>% group_by(day, clade,symbiotic) %>% 
  summarize(sum(count))

# plot as proportions  -----
d %>% ungroup() %>% group_by(day, clade,symbiotic) %>% 
  filter(!clade=="NoSym") %>%
  summarize(count_sum = sum(count)) %>% 
  mutate(freq = count_sum / sum(count_sum)) %>%
  ggplot(aes(x=clade,y=freq,group=symbiotic,fill=symbiotic))+
  scale_fill_manual(values=c("grey","darkgreen"))+
  facet_grid(. ~ day)+
  geom_bar(position="fill", stat = "identity")+
  theme_bw()

d %>% ungroup() %>% group_by(day, clade,recruit) %>% 
  summarize(count_sum = sum(count)) %>% 
  filter(!clade=="NoSym") %>%
  mutate(freq = count_sum / sum(count_sum)) %>%
  ggplot(aes(x=clade,y=freq,group=recruit,fill=recruit))+
  scale_fill_manual(values=c("grey","brown"))+
  facet_grid(. ~ day)+
  geom_bar(position="fill", stat = "identity")+
  theme_bw()

d %>% ungroup() %>% group_by(day, clade,tentacle) %>% 
  summarize(count_sum = sum(count)) %>% 
  filter(!clade=="NoSym") %>%
  mutate(freq = count_sum / sum(count_sum)) %>%
  ggplot(aes(x=clade,y=freq,group=tentacle,fill=tentacle))+
  scale_fill_manual(values=c("grey","black"))+
  facet_grid(. ~ day)+
  geom_bar(position="fill", stat = "identity")+
  theme_bw()

# stats -------------
library(MCMCglmm)

d_tentacle <- d %>% filter(!clade=="NoSym") %>% ungroup() %>% group_by(day, clade,plate,tentacle) %>% 
  summarize(tentacle_count = sum(count))
head(d_tentacle)

set.seed(1)
mcmcTentacle <- MCMCglmm(tentacle_count ~ clade*day,
                   random = ~plate,
                   family = "poisson",
                   data = d_tentacle)
summary(mcmcTentacle)

#                   post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# (Intercept)      3.10855  2.41597  3.81769     1000 <0.001 ***
# cladeD          -0.15743 -0.51574  0.18780     1000  0.376    
# day8_31         -0.66744 -1.06329 -0.30175     1000 <0.001 ***
# cladeD:day8_31   0.02943 -0.44643  0.58529     1000  0.914

d_recruit <- d %>% filter(!clade=="NoSym") %>% ungroup() %>% group_by(day,clade,plate,recruit) %>% 
  summarize(recruit_count = sum(count))
head(d_recruit)

set.seed(1)
mcmcRecruit <- MCMCglmm(recruit_count ~ clade*day,
                         random = ~plate,
                         family = "poisson",
                         data = d_recruit)
summary(mcmcRecruit)
#                   post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# (Intercept)       3.0324   2.2739   3.8568     1000 <0.001 ***
# cladeD           -0.2711  -0.6737   0.1681     1000  0.222    
# day8_31          -0.6878  -1.0918  -0.2522     1000  0.006 ** 
# cladeD:day8_31    0.1654  -0.4187   0.7709     1000  0.600

d_symbiotic <- d %>% filter(!clade=="NoSym") %>% ungroup() %>% group_by(day,clade,plate,symbiotic) %>% 
  summarize(symbiotic_count = sum(count))
head(d_symbiotic)

set.seed(1)
mcmcSymbiotic <- MCMCglmm(symbiotic_count ~ clade*day,
                        random = ~plate,
                        family = "poisson",
                        data = d_symbiotic)
summary(mcmcSymbiotic)
#                 post.mean l-95% CI u-95% CI eff.samp  pMCMC    
# (Intercept)      2.66795  1.96568  3.55106   1095.1 <0.001 ***
# cladeD          -0.05927 -0.94323  0.84665   1000.0  0.886    
# day8_31         -0.56422 -1.38773  0.39873   1000.0  0.234    
# cladeD:day8_31   0.18714 -1.16861  1.54020    795.8  0.774
