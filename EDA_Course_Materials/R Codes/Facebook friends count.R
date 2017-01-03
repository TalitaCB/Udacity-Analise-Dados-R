pf <- read.csv('pseudo_facebook.tsv', sep='\t')

ggplot(aes(x=age, y=friend_count), data=pf) +
  geom_jitter(alpha=1/20) +
  xlim(13,90) 

ggplot(aes(x=age, y=friend_count), data=pf) +
  geom_point(alpha=1/20) +
  xlim(13,90) +
  coord_trans (y = "sqrt")


ggplot(aes(x=age, y=friend_count), data=pf) +
   geom_point(alpha=1/20, position = position_jitter(h=0)) +
   xlim(13,90) +
   coord_trans (y = "sqrt")

age_groups <- group_by(pf, age)

pf.fc_by_age <- summarise(age_groups, 
                          friend_count_mean = mean(friend_count), 
                          friend_count_median = median(friend_count), 
                          n=n())

pf.fc_by_age <- pf %>%
   group_by(age) %>%
   summarise(friend_count_mean = mean(friend_count), 
             friend_count_median = median(friend_count), 
             n = n()) %>%
   arrange(age)

ggplot(aes(x=age, y=friend_count_median), data=pf.fc_by_age) +
  geom_line() +
  xlim(13,90) 

ggplot(aes(x=age, y=friend_count_mean), data=pf.fc_by_age) +
       geom_line() +
       xlim(13,90) 

ggplot(aes(x=age, y=friend_count), data=pf) +
       geom_point(alpha=0.05, position = position_jitter(h=0), color = 'orange') +
       xlim(13,90) +
       coord_trans (y = "sqrt") + 
      geom_line(stat='summary', fun.y = mean) +
      geom_line(stat='summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color='blue') +
      geom_line(stat='summary', fun.y = quantile, fun.args = list(probs = .5), linetype = 2, color='blue') +
      geom_line(stat='summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color='blue')


ggplot(aes(x=age, y=friend_count), data=pf) +
     geom_point(alpha=0.05, position = position_jitter(h=0), color = 'orange') +
     coord_cartesian (xlim = c(13,90))+
     coord_trans (y = "sqrt") + 
     geom_line(stat='summary', fun.y = mean) +
     geom_line(stat='summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color='blue') +
     geom_line(stat='summary', fun.y = quantile, fun.args = list(probs = .5), linetype = 2, color='blue') +
     geom_line(stat='summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color='blue')

cor.test(pf$age, pf$friend_count, method = 'pearson')

with(pf, cor.test(age, friend_count, method = 'pearson')  )
with(subset(pf, age <= 70), cor.test(age, friend_count, method = 'pearson')  )

ggplot(aes(x=www_likes_received, y=likes_received), data=pf) +
                 geom_point(alpha=0.05, position = position_jitter(h=0)) +
             ylim(1,10000) +
             xlim(1,5000) +
        coord_trans(y = "sqrt") +
        coord_trans(x = "sqrt") 


ggplot(aes(x=www_likes_received, y=likes_received), data=pf) +
       geom_point(alpha=0.05, position = position_jitter(h=0)) +
       xlim(0,quantile(pf$www_likes_received, 0.95)) +
       ylim(0,quantile(pf$likes_received, 0.95)) +
  geom_smooth(method = 'lm', color = 'red')

pf$age_with_months <- pf$age + (12 - pf$dob_month) / 
  
age_months_groups <- group_by(pf, age_with_months)

pf.fc_by_age_months <- summarise(age_months_groups, 
                        friend_count_mean = mean(friend_count), 
                        friend_count_median = median(friend_count), 
                        n=n())

pf.fc_by_age_months <-arrange(pf.fc_by_age_months,age_with_months )

ggplot(data=subset(pf.fc_by_age_months,age_with_months <= 71 ), aes(y=friend_count_mean, x = age_with_months)) +
  geom_point()

ggplot(data=subset(pf.fc_by_age_months,age_with_months <= 71 ), aes(y=friend_count_mean, x = age_with_months)) +
  geom_line()