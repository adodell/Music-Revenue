suppressMessages(library(ggplot2))
suppressMessages(library(ggthemes))
suppressMessages(library(extrafont))
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
suppressMessages(library(tictoc))

# setup
tic()

main_df = data.frame(Year = 1999:2016,
                     Physical = c(23.8,23.3,23.0,21.4,19.8,19.2,17.9,16.3,14.1,11.9,10.4,8.9,8.2,7.6,6.8,6.1,5.8,5.4),
                     Digital = c(0,0, 0, 0, 0, 0.4,1.1,2.1,2.9,3.7,4.1,4.3,4.9,5.4,5.7,6.0,6.6,7.8),
                     PerformanceRights = c(0, 0,0.6,0.7,0.8,0.9,0.9,1.0,1.2,1.3,1.3,1.4,1.4,1.6,1.7,1.9,2.0,2.2),
                     Synchronisation = c(0,0,0,0,0,0,0,0,0,0,0,0.3,0.3,0.3,0.3,0.3,0.4,0.4))

main_talldf = melt(main_df, id.vars = 'Year',
                   measure.vars = c('Physical','Digital','PerformanceRights','Synchronisation'),
                   variable.name = 'RevenueSource', value.name = 'BillionsUSD')
main_talldf$RevenueSource = factor(main_talldf$RevenueSource,
                                   levels = c('Synchronisation', 'PerformanceRights','Digital','Physical'))


main_plot =
  ggplot(main_talldf, aes(x = Year, y = BillionsUSD)) +
  geom_area(aes(fill = RevenueSource), stat = 'identity', alpha = 0.7) +
  geom_label(data = filter(main_talldf, BillionsUSD > 0),
            aes(label = BillionsUSD, size = 10),
            fontface = 'bold',
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  scale_x_continuous(expand = c(0.02,0.02), breaks = seq(1996,2017)) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_wsj(labels = c('Synchronisation (use in ads, film, games, TV)',
                            'Performance Rights (broadcast and venues)',
                            'Digital (download and streaming)',
                            'Physical'))+
  guides(fill = guide_legend(title = 'Revenue Source', keyheight = 1)) + 
  labs(title = 'Global Recorded Music Industry Revenues (1999-2016)',
       y = 'US Dollars (Billions)', x = 'Year',
       caption = 'Source: IFPI Global Music Report 2017') +
  theme(plot.title = element_text(size=30, face="bold", family = 'Garamond'),
        panel.border = element_rect(fill = NA, linetype = 1),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = 'grey'),
        legend.position = c(0.95,0.95), legend.justification = c('right','top'),
        legend.box.background = element_rect(),
        legend.text = element_text(family = 'Garamond',size = 18),
        legend.margin = margin(10,10,10,10),
        legend.title = element_text(family = 'Garamond', size = 28),
        axis.text.x = element_text(family = 'Garamond', face = 'bold', size = 18),
        axis.text.y = element_text(family = 'Garamond', face = 'bold', size = 18),
        axis.title.x = element_text(family = 'Garamond', face = 'bold', size = 22, margin = margin(t = 15)),
        axis.title.y = element_text(family = 'Garamond', face = 'bold', size = 22, margin = margin(r = 15)))

png('MusicRevenue_BySource.png', width = 18, height = 10, units = 'in', res = 240)
print(main_plot)
dev.off()

toc()
