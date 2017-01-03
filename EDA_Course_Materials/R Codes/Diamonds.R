ggplot(data = diamonds, aes(x = price, fill = cut)) +
        geom_histogram() +
        facet_wrap(~color) + 
        scale_fill_brewer(type = 'qual') +
   scale_x_log10()

ggplot(data = diamonds, aes(y = price, x = table, color = cut)) +
       geom_point() +
       scale_color_brewer(type = 'qual') +
       scale_x_continuous(breaks = seq(50,80,2), limits = c(50,80))

ggplot(data = diamonds, aes(y = price, x = volume, color = clarity)) +
            geom_point() +
            scale_color_brewer(type = 'div') +
            scale_y_log10() +
            coord_cartesian(xlim = c(0,quantile(diamonds$volume, 0.99)))


