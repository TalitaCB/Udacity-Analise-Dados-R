bt <- read.csv("birthdays.csv",sep = ";")

bt$date <- as.character(bt$Start)

bt$month <- substr(bt$date,0,2)
bt$day <- substr(bt$date,4,5)

bt$day <- as.numeric(bt$day)
bt$month <- as.numeric(bt$month)

ggplot(data = subset(bt, !is.na(day)), aes(x = day)) + 
  + geom_histogram(binwidth = 1)

ggplot(data = subset(bt, !is.na(month)), aes(x = month)) +
  + geom_histogram(binwidth = 1)

ggplot(data = subset(bt, !is.na(day)), aes(x = day)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~month)

pf.fc_by_age_gender <- group_by(pf, age, gender)
pf.fc_by_age_gender <- summarise(pf.fc_by_age_gender,
                                 mean_friend_count = mean(friend_count), 
                                 median_friend_count = median(friend_count), 
                                 n = n()  )

pf.fc_by_age_gender <- ungroup(pf.fc_by_age_gender)
pf.fc_by_age_gender <- arrange(pf.fc_by_age_gender, age)

ggplot(data = subset(pf.fc_by_age_gender, !is.na(gender)), aes(x = age, y = median_friend_count) ) +
     geom_line(aes(color = gender))

library('reshape2')

pf.fc_by_age_gender.wide <- dcast(subset(pf.fc_by_age_gender, !is.na(gender)),
                                  age ~gender, 
                                  value.var = 'median_friend_count')



ggplot(data = pf.fc_by_age_gender.wide, aes(x = age, y= female/male) ) +
  geom_line()  +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)

pf$year_joined <- 2014 - (pf$tenure/365)
pf$year_joined <- floor(pf$year_joined)
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))


ggplot(data = subset(pf, !is.na(year_joined.bucket)), aes(x = age, y = friend_count) ) +
      geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median) +
 geom_line(stat = 'summary', fun.y = mean, linetype = 2)
 
 ggplot(data = subset(pf, !is.na(year_joined.bucket)), aes(x = age, y = friend_count) ) +
   geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean) +
 geom_line(stat = 'summary', fun.y = mean, linetype = 2)
 
 ggplot(data = subset(pf, tenure >= 1), aes(y = friendships_initiated/ tenure, x = tenure)) +
    geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)
 
 ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
                data = subset(pf, tenure > 0)) +
        geom_line(aes(color = year_joined.bucket),
                                  stat = "summary",
                                  fun.y = mean) + 
    geom_smooth(alpha = 0.8)

 
 pf$prop_initiated = pf$friendships_initiated/pf$friend_count
 
 ggplot(data = pf, aes(x = tenure, y = prop_initiated, color = year_joined.bucket )) +
   geom_line(stat = 'summary', fun.y=median)  +
   geom_smooth(method='lm', color='red')
 
 with(subset(pf, year_joined > 2012), summary(prop_initiated))
 ggplot(data = diamonds, aes(y = price/carat, x = cut)) +
             geom_point(aes(colour = color)) +
         facet_wrap(~clarity) + 
             scale_color_brewer(type = 'div')
 