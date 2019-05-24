ggplot(aes(x = S020, y = ideo_age_adj, color = country), data = ideo_age_adj) +
  geom_point_interactive(aes(tooltip = country_wave), size = 2) + 
  geom_path(arrow = arrow(length = unit(.1, "inches")), size = 1) + 
  labs(caption = "Linear regression coefficients controled for education, higher coefficients indicate stronger association between older age-right wing and younger age-left wing") +
  scale_y_continuous("Age-Ideology association") +
  scale_x_date("Year", date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("1989-01-01", "2013-01-01"))) + 
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 13), 
        legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
        legend.key.size = unit(.15, "inches"), axis.text.x = element_text(size = 7.9), 
        axis.text.y = element_text(size = 7), 
        panel.grid.major.x = element_line(color = "black", size = .25), 
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  scale_color_manual(values = c(rainbow_hcl(length(unique(ideo_age_adj$country)))), 
                     name = "Country", drop = F) -> gg_plot_age

girafe(code = print(gg_plot_age), width_svg = 15)


ideo_age_adj <- ideo_age_adj[order(ideo_age_adj$country, ideo_age_adj$S020), ]

ideo_age_list <- ideo_age_adj %>% dlply("country")

lapply(ideo_age_list, function(x) {
  delt <- (x[dim(x)[1], 2]) - (x[1, 2])
  return(data.frame(coeff_delt = delt))
}) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country = row.names(.), .) -> coeff_delt_age

ideo_age_adj <- left_join(ideo_age_adj, coeff_delt_age)

ideo_edu_list <- ideo_edu_adj %>% dlply("country")

lapply(ideo_edu_list, function(x) {
  delt <- (x[dim(x)[1], 2]) - (x[1, 2])
  return(data.frame(coeff_delt = delt))
}) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country = row.names(.), .) -> coeff_delt_edu

ideo_age_adj <- left_join(ideo_age_adj, coeff_delt_age)
