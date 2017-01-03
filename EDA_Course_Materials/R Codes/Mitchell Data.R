ggplot(aes(x=Month, y=Temp), data=Mitchell) +
       geom_point() +
   scale_x_continuous(breaks = 1)

 ggplot(aes(x=Month, y=Temp), data=Mitchell) +
     geom_point() +
 scale_x_discrete(breaks = seq(0,203,12))
 
 ggplot(aes(x=(Month%%12), y=Temp), data=Mitchell)  +
 geom_point()