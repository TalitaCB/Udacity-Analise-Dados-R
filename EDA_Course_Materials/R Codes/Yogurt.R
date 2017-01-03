yo <- read.csv('yogurt.csv')

summary(yo)


yo$id <- factor(yo$id)


ggplot(data = yo, aes(x = price)) + 
      geom_histogram(binwidth = 10)


yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)

ggplot(data = yo, aes(x = time, y = price)) + 
  geom_jitter(alpha = 1/10)

set.seed(4230)

sample.ids <- sample(levels(yo$id), 16)

ggplot(data = subset(yo, id %in% sample.ids), aes(x = time, y = price)) +
  geom_line() + 
  facet_wrap(~id) +
  geom_point(aes(size = all.purchases), pch = 1)

library(GGally)
theme_set(theme_minimal(20))

set.seed(1836)
pf_subset <- pf[,c(2:15)]
names(pf_subset)

ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000),])



