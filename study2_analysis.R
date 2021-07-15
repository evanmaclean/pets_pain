library(tidyverse)
library(here)
library(lmerTest)
library(car)
library(emmeans)

my.data <- read_csv(here('data','S2_data.csv'))

# remove exclusions
my.data <- filter(my.data, include == 1)
colnames(my.data) <- tolower(colnames(my.data))

table(my.data$female) # 3 cases of other; remove due to small N
my.data <- filter(my.data, female != "2")

# make long
my.data <- pivot_longer(my.data, cols = c(pain1, pain2), values_to = 'pain', names_to = 'time', names_prefix = 'pain')
# consolidate frequency variable
my.data <- unite(my.data, col = 'frequency', c(pet_frequency, human_frequency), na.rm = T)

# make companion a factor
my.data$companion = as_factor(my.data$companion)
levels(my.data$companion)
my.data$companion <- fct_recode(my.data$companion, animal = '2', human = '1', control = '0')

mod1 <- lmer(pain ~ companion*time + age + female + (1|responseid), data = my.data)
summary(mod1)
anova(mod1)

emm1 <- emmeans(mod1, pairwise~time | companion)
emm1
eff_size(emm1, sigma = sigma(mod1), edf = df.residual(mod1))

# interaction contrast
tmp <- contrast(emm1[[1]], interaction = 'pairwise', adjust = 'tukey', by = NULL)
print(tmp)

#' comparison of pain between conditions at timepoint 1 and 2
emm2 <- emmeans(mod1, pairwise~companion | time)
emm2

# plot
theme_set(theme_classic(18))
plotmeans <- as.data.frame(emmeans(mod1, c('time','companion')))

# relevel conditions to match earlier colors
levels(plotmeans$companion)
plotmeans$companion <- fct_relevel(plotmeans$companion, "control","animal","human")

ggplot(plotmeans, aes(x = time, y = emmean, group = companion, color = companion)) +
  geom_point() + 
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1) + 
  geom_line() + coord_cartesian(ylim = c(3,5)) + 
  theme(legend.position = c(0.25,0.25), legend.title = element_blank()) + 
  labs(y = "pain") + scale_y_continuous(breaks = seq(3,5,1)) + 
  scale_color_brewer(type = 'qual', palette = 6) 

ggsave('figures/fig2.png', width = 5, height = 5)
