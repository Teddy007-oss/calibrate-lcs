library(openair)
library(tidyverse)
library(ggpmisc)
library(ggpubr)
#load the data (this should contain the data used for the model
#.. and the calibrated data)

df <- read.csv("C:\\Users\\USER\\Documents\\Afri-SET\\Calibrations\\data\\Zefan_Wet_calibrated_data.csv")

summary(df)

#parsing the date column as date-time
df$date <- lubridate::ymd_hms(df$date)

#renaming columns, the are subject to change based on the data
colnames(df)[6] <- "pm25_cal"
colnames(df)[3] <- "pm25_raw"

summary(df)

#function to plot time series
time_series <- function(df, start_date, end_date) {
  timePlot(df, pollutant = c("PM2.5", "pm25_raw", "pm25_cal"),
           stack = FALSE, group = TRUE, date.breaks = 10,
           par.settings = list(fontsize = list(text = 20, family = "Serif"),
                               axis.line = list(col = "black", lwd = 2),
                               add.text = list(fontface = "bold"),
                               axis.text = list(fontface = "bold"), 
                               par.ylab.text = list(fontface = "bold")),
           y.relation = "free", lwd = 3, lty = 1, avg.time = "day",
           date.format = "%b %d,%Y", scales = list(x = list(rot = 50)),
           ylab = "PM2.5 (ug/m3)", ylim = c(0, 50),
           key.columns = 2, key.font = 2, ci = TRUE,
           xlim = as.POSIXct(c(start_date, end_date)),
           cols = c("red", "black", "goldenrod"), key.position = "inside")
}

#plotting the time series of raw and calibrated data
#be sure to confirm the date range
#where the image is stored


jpeg("figures/Zefan/Zefan_wet_calibrated lineplot.jpeg",
     units = "cm", width = 25, height = 20, res = 270)
time_series(df, start_date = "2025-03-30", end_date = "2025-05-10")
dev.off()
 
#Codeblock to make the scatterplot
a <- ggplot(df, aes(y = pm25_raw, x = PM2.5)) +
  stat_poly_eq(label.y = 0.9, size = 7, family = "serif", vjust = 0.4) +
  geom_point(fill = "yellow", size = 4.5,
             colour = "black",pch = 21, stroke = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.9) +
  theme_test() +
  theme(text = element_text(family = "serif", size = 20),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.6, 0.6, 0.2, 0.2, "cm"),
        axis.title.y = element_text(margin = unit(c(0, 1, 0, 0), "mm"),
                                    face = "bold", color = "black"),
        axis.text.y = element_text(size = 19, colour = "black",
                                   margin = unit(c(1, 1, 1, 1), "mm")),
        axis.title.x = element_text(face = "bold", size = 19,
                                    margin = unit(c(1, 0, 0, 0), "mm")),
        legend.title = element_blank(),
        plot.title = element_text(color = "black",hjust = 0,
                                  size = 30, face = "bold"),
        plot.background = element_rect(fill = "white", color = "white",
                                       size = 1.5),
        axis.text.x = element_text(vjust = 0.5, size = 19,
                                   angle = 0, colour = "black"),
        strip.text.x = element_text(size = 25, face = "bold"),
        strip.background = element_rect(fill = "orange", linewidth = 1.3),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width  = unit(1.1, "cm"),
        legend.text = element_text(size = 15)) +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 100, 10), expand = c(0, 0),
                     limits = c(0, 100)) +
  xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3)))) +
  ylab(expression(bold(Raw~PM[2.5]~(mu*g/m^3))))

#viewing the plot
a
b

#saving the scatterplots
jpeg("figures/Zefan/Zefan Wet Calibrated PM2.5 ScatterPlot.jpeg",units="cm", width=40, height=15, res=300)
ggarrange(a, b, ncol = 2)
dev.off()


#calaculating the stats

#selecting the relevant columns and removing NAs
a4 <- df %>%
  select(pm25_cal, PM2.5)

a4 <- na.omit(a4)

cor(a4$pm25_cal, a4$PM2.5, method = "pearson")^2 #R-squared pearson
sqrt(mean((a4$pm25_cal - a4$PM2.5)^2)) #RMSE
mean(abs(a4$pm25_cal - a4$PM2.5)) #MAE
mean(a4$pm25_cal - a4$PM2.5) #MBE
