library(tidyverse)
library(here)
library(lmerTest)
library(car)
library(emmeans)
library(psych)
library(forcats)

my.data <- read_csv(here('data','S3_data.csv'))

# remove exclusions
my.data <- filter(my.data, filter == 1)
table(my.data$Female) # only two cases of other; drop due to small N
my.data <- filter(my.data, Female!='2')

colnames(my.data) <- tolower(colnames(my.data))

# Cronbach's alpha on unconditional love measures
alpha(select(my.data, love1,love2,love3))

my.data$pet <- as_factor(my.data$pet)
levels(my.data$pet)
my.data$pet <- fct_recode(my.data$pet, human = '0', pet = '1')

my.data <- pivot_longer(my.data, cols = c(pain1, pain2), values_to = 'pain', names_to = 'time', names_prefix = 'pain')
my.data$female <- as_factor(my.data$female)
table(my.data$female)
my.data$female <- fct_recode(my.data$female, male = "0", female = "1")
table(my.data$female)

# load r script available from processmacro.org/download.html
# For tutorial see: http://www.regorz-statistik.de/en/mediation_process_for_r.html
source('process.R')
# The process file needs factors to be integers.  make a copy of the data and adjust as needed.

my.data <- distinct(my.data, responseid, .keep_all = T)
my.data$pet <- as.numeric(p.data$pet) # needed for process

mod1 <- lm(paindrop~factor(pet), data = my.data)
summary(mod1)

# Model 4
process(data = my.data, y = "paindrop", x = "pet", m = "loveavg", total = 1, model = 4)

# total = 1 gives us the total effect

# Model 8
process(data = my.data, y = "paindrop", x = "pet", m = "loveavg", w = "frequ", model = 8, total = 1)

theme_set(theme_classic(18))

# Plots of the moderator relationship
my.mod <- lm(loveavg ~ pet*frequ, data = my.data)

p1 <- emmip(my.mod, pet ~ frequ, cov.reduce = range)
p1 <- p1 + scale_color_manual(values = c('orange',"blue")) +
  labs(x = 'frequency of interaction', y = 'unconditional love') +
  theme(legend.position = c(0.6, 0.2), legend.title = element_blank())
p1

mod2 <- lm(paindrop ~ pet*frequ, data = my.data)
p2 <- emmip(mod2, pet ~ frequ, cov.reduce = range)
p2 <- p2 + scale_color_manual(values = c('orange',"blue")) +
  labs(x = 'frequency of interaction', y = 'pain reduction') +
  theme(legend.position = c(0.6, 0.2), legend.title = element_blank())
p2

library(gridExtra)
my.grobs <- arrangeGrob(p1, p2, nrow = 1)
  ggsave(my.grobs, filename = 'figures/fig4.png', width = 8, height = 4)
