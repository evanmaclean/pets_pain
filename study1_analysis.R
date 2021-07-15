library(tidyverse)
library(here)
library(lmerTest)
library(car)
library(emmeans)
library(forcats)

my.data <- read_csv(here('data','S1_data.csv'))

# remove exclusions
my.data <- filter(my.data, include == 1)
colnames(my.data) <- tolower(colnames(my.data))

table(my.data$pet)
table(my.data$female) # Only two cases of others, remove. 
my.data <- filter(my.data, female != "2")

# make long
my.data <- pivot_longer(my.data, cols = c(pain1, pain2), names_to = "time", values_to = "pain", names_prefix = "pain") %>% mutate(pain = as.numeric(pain))

# set named variable for condition 
my.data$condition = ifelse(my.data$pet == 1, yes = 'pet', no = 'control')

my.data$female <- as_factor(my.data$female)

# mixed model
my.lm <- lmer(pain ~ condition*time + age + female + (1|responseid), data = my.data)
summary(my.lm)
anova(my.lm)

# contrasts
my.emm <- emmeans(my.lm, pairwise~time | condition)
my.emm
eff_size(my.emm, sigma = sigma(my.lm), edf = df.residual(my.lm))

# interaction contrast
tmp <- contrast(my.emm[[1]], interaction = 'pairwise', by = NULL)
tmp

# comparison of pain between conditions at each time point
emm2 <- emmeans(my.lm, pairwise~condition | time)
emm2
eff_size(emm2, sigma = sigma(my.lm), edf = df.residual(my.lm))

theme_set(theme_classic(18))
plotmeans <- as.data.frame(emmeans(my.lm, c('time','condition')))
plotmeans$condition <- fct_recode(plotmeans$condition, distraction = 'control')

# plot
ggplot(plotmeans, aes(x = time, y = emmean, group = condition, color = condition)) +
  geom_point() + 
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1) + 
  geom_line() + coord_cartesian(ylim = c(1,6)) + 
  theme(legend.position = 'top', legend.title = element_blank()) + 
  labs(y = "pain") + scale_y_continuous(breaks = seq(1,6,1))  + 
  scale_color_brewer(type = 'qual', palette = 6) 

ggsave('figures/fig1.png', height = 5, width = 5)
