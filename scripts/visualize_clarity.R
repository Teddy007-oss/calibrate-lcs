
library(tidyverse)
library(ggplot2)
library(reshape2)
library(readr)
library(ggpmisc)
library(xts)
library(data.table)
library(openair)
library(wesanderson)
library(FSA)
library(grafify)
#load the data
df_wet <- read.csv("C:\\Users\\USER\\Documents\\Afri-SET\\Calibrations\\clarity_wet_corrected.csv")
df_dry <- read.csv("C:\\Users\\USER\\Documents\\Afri-SET\\Calibrations\\clarity_dry_corrected.csv")

#lets work on the datetime
#sometimes the 00:00:00 is ignored giving us...
#NA for some dates when converted to asPoXICT.

df_wet$date <- lubridate::ymd_hms(df_wet$date)
df_dry$date <- lubridate::ymd_hms(df_dry$date)

#lets convert the data into daily means
df_dry <- data.frame(period.apply(df_dry, endpoints(df_dry, "day"), colMeans, na.rm = TRUE))

setDT(df_dry, keep.rownames = "date")
df_dry <- df_dry %>%
  mutate(date1 = lubridate::ymd_hms(date))

#move column 'column' to first position
df_dry <- df_dry %>% select(date1, everything())
df_dry <- df_dry[, -2]
colnames(df_dry)[1] <- "date"

df_wet$season <- "Wet"
df_dry$season <- "Dry"

df <- rbind(df_dry, df_wet)

df <- df %>%
  select(date, CPC_corrected, St_corrected, Bet_corrected,
         Com_corrected, TMA_corrected, Fre_corrected, Val_corrected,
         Chal_corrected, Ade_corrected, San_corrected,
         Sua_corrected, Sep_corrected, Aso_corrected, season)

colnames(df) <- c("date", "CPC_Tm", "St_Joseph_sch_Acc", "Bethlehem_Tm1",
                  "Comm2_Tm", "TMA_Tm", "FreeZones_Tm", "Valco_Rbt_Tm",
                  "Chalton_Cl_Acc", "Adenta_MA_Acc", "Santasi_Ksi",
                  "Suame_MA_Ksi", "Sepe_Dote_Ksi", "Asokwa_Shell_Ksi", "Season")

df <- df[, c(1, 3, 2, 8, 9, 5, 6, 7, 4, 10, 11, 12, 13, 14, 15)]

#lets do the summary
df_sum <- df %>% tidyr::gather(site, pm25, St_Joseph_sch_Acc:Asokwa_Shell_Ksi, na.rm = TRUE)
sum_results <- Summarize(pm25 ~ site, data = df_sum, na.rm = TRUE)

#selecting just the accra sites
df_accra <- df %>%
  select(date, St_Joseph_sch_Acc, CPC_Tm, Valco_Rbt_Tm,
         Chalton_Cl_Acc, Comm2_Tm, TMA_Tm, FreeZones_Tm, Bethlehem_Tm1,
         Adenta_MA_Acc, Season)

#selecting just the kumasi sites
df_kumasi <- df %>%
  select(date ,Santasi_Ksi, Suame_MA_Ksi, Sepe_Dote_Ksi, Asokwa_Shell_Ksi, Season)

df_kumasi$GKMA <- rowMeans(df_kumasi[ ,2:5], na.rm = TRUE)
df_accra$GAMA <- rowMeans(df_accra[, 2:9], na.rm =TRUE)

allw <- df %>%
  filter(Season == "Dry")

df_long <- df_kumasi %>%
  select(-date) %>%
  pivot_longer(cols = - Season,
               names_to = "Site",
               values_to = "pm25")

new <-df %>% tidyr::gather(Site, pm25, St_Joseph_sch_Acc:Asokwa_Shell_Ksi, na.rm = TRUE)


##-------------------#BOXPLOT-----------------------------------
jpeg("figures/Clarity/boxplot4ksi.jpeg", units = "cm", width = 40, height = 25, res = 300)
ggplot(df_long, aes(x = fct_inorder(Site), y = pm25, fill = Season)) +
  stat_boxplot(geom = "errorbar", lwd = 1,
               width = 0.2, position = position_dodge(width = 0.5)) +
  geom_boxplot(aes(), width = 0.5, lwd = 1, outlier.shape = NA,
               position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5,
               position = position_dodge(width = 0.5), show.legend = FALSE) +
  geom_hline(aes(yintercept = 35, linetype = "GHANA STD (24-h)"),
             colour = "black", linewidth = 1) +
  geom_hline(aes(yintercept = 15, linetype = "WHO Guideline (24-h)"),
             colour = "black", linewidth = 1) +
  scale_fill_grafify(palette = "vibrant") +
  #okabe_ito,  vibrant, bright, safe, fishy, muted , kelly
  theme_classic() +
  theme(text = element_text(family = "serif"),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.title.y = element_text(color = "black", size = 30,  face = "bold"),
        axis.text.y = element_text(size = 25, face = "bold", color = "black"),
        plot.margin = margin(0.7, 1.5, 0.7, 0.3, "cm"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(2.5, "line"),
        plot.title = element_text(color = "black", hjust = 0.5),
        legend.text = element_text(size = 25,  face = "bold"),
        axis.text.x = element_text(color = "black", vjust = 1,
                                   face = "bold", size = 20, angle = 45, hjust = 1),
        legend.position = c(0.40, 0.85)) +
  scale_y_continuous(breaks = seq(0, 600, 50),
                     expand = c(0, 0), limits = c(0, 600)) +
  ylab(expression(bold(Daily ~ PM[2.5] ~ (mu * g / m^3))))
dev.off()
##------------------------------------------------------


##------Box plot---------
jpeg("figures/Clarity/boxplot for GKMA GAMA boxcombi.jpeg",units="cm", width=40, height=25, res=300)
ggplot(new,aes(x=fct_inorder(Site),y=pm25, fill=Season)) + 
  stat_boxplot(geom = "errorbar", lwd=1,width= 0.2,position=position_dodge(width = 0.5))+
  geom_boxplot(aes(),width=0.5, lwd=1,outlier.shape = NA,position=position_dodge(width = 0.5)) +
  stat_summary(fun=mean, geom='point', shape=20, size=5, position=position_dodge(width = 0.5), show.legend=FALSE) +
  geom_hline(aes(yintercept=35,linetype= "GHANA STD (24-h)"), colour="black",linewidth=1)+
  geom_hline(aes(yintercept=15,linetype= "WHO Guideline (24-h)"), colour="black",linewidth=1)+
  #scale_fill_manual(values=wes_palette(name="FantasticFox1"))+
  #scale_fill_manual(values=wes_palette(name="Darjeeling1"))+
  #scale_fill_manual(values=wes_palette(name="Cavalcanti1"))+
  #scale_fill_manual(values = c( "#E7B800", "#FC4E07","#999999", "#56B4E9"))+
  scale_fill_grafify(palette = "safe" )+ #okabe_ito,  vibrant, bright, safe, fishy, muted , kelly
  #scale_color_brewer(palette = "Dark2")+
  #stat_compare_means(aes(group = Season), label = "p.signif")+
  #stat_compare_means(label = "p.format", paired = FALSE)+
  #stat_compare_means(label = "p.format")+
  #theme_bw()+
  theme_classic()+
  theme(text = element_text(family = "serif"),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.title.y = element_text(color = "black", size = 30,  face = "bold"),
        axis.text.y = element_text(size = 25, face = "bold", color = "black"),
        plot.margin = margin(0.7,1.5,0.7,0.3, "cm"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(color = "black", hjust = 0.5),
        legend.text= element_text(size=25,  face = "bold"),
       axis.text.x= element_text(color= "black",vjust = 1,  face = "bold", size = 20, angle = 60, hjust = 1),
      #strip.text.x = element_text(size = 20, face = "bold"),
      strip.background = element_rect(fill = "white", linewidth = .8),
      legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.key.height = unit(0.6, "cm"),
      legend.key.width  = unit(1.1, "cm"),
      legend.position = c(0.28,0.85))+
  scale_y_continuous(expand = c(0, 0))+
  ylab(expression(bold(Daily~PM[2.5]~(mu*g/m^3))))+
 facet_wrap(~Season,nrow =2 , scales =  "free_y")
dev.off()
#-----------------------
#--------------------LINEPLOT---------------------------
jpeg("figures/Clarity/lineplot4accra2.jpeg", units = "cm", width = 50, height = 25, res = 300)
ggplot(df, aes(x = date, group = 1)) +
  geom_line(aes(y = CPC_Tm, colour = "CPC_Tm"), lwd = 1.5, lty = 1) +
  geom_line(aes(y = St_Joseph_sch_Acc, colour = "St_Joseph_sch_Acc"),
            lwd = 1.5, lty = 1) +
  geom_line(aes(y = Bethlehem_Tm1, colour = "Bethlehem_Tm1"),
            lwd = 1.5, lty = 1) +
  geom_line(aes(y = Comm2_Tm, colour = "Comm2_Tm"), lwd = 1.5, lty = 1) +
  geom_line(aes(y = TMA_Tm, colour = "TMA_Tm"),
            lwd = 1.5,lty = 1) +
  geom_line(aes(y = FreeZones_Tm, colour = "FreeZones_Tm"),
            lwd = 1.5, lty = 1) +
  geom_line(aes(y = Valco_Rbt_Tm, colour = "Valco_Rbt_Tm"),
            lwd = 1.5, lty = 1) +
  geom_line(aes(y = Chalton_Cl_Acc, colour = "Chalton_Cl_Acc"),
            lwd = 1.5, lty = 1) +
  geom_line(aes(y = Adenta_MA_Acc, colour = "Adenta_MA_Acc"),
            lwd = 1.5, lty = 1) +
  #geom_line(aes(y = Santasi_Ksi, colour = "Santasi_Ksi"),
            #lwd = 1.5, lty = 1) +
  #geom_line(aes(y = Suame_MA_Ksi, colour = "Suame_MA_Ksi"),
            #lwd = 1.5, lty = 1) +
  #geom_line(aes(y = Sepe_Dote_Ksi, colour = "Sepe_Dote_Ksi"),
            #lwd = 1.5, lty = 1) +
  #geom_line(aes(y = Asokwa_Shell_Ksi, colour = "Asokwa_Shell_Ksi"),
            #lwd = 1.5, lty = 1) +
  geom_hline(aes(yintercept = 35, linetype = "GHANA STD (24-h)"),
             colour = "black", linewidth = 1) +
  geom_hline(aes(yintercept = 15, linetype = "WHO AQG (24-h)"),
             colour = "black", linewidth = 1) +
  scale_shape_manual(values = 11) +
  scale_colour_manual(values = c("orange2", "violetred", "darkorchid",
                                 "dodgerblue", "blue3", "orange3", "darkgreen",
                                 "springgreen", "red2", "cyan", "olivedrab",
                                 "violet", "black")) +
  guides(colour = guide_legend(ncol = 2)) +
  #scale_fill_manual(values=wes_palette(name="FantasticFox1"))+
  theme_classic() +
  #theme_bw()+
  theme(text = element_text(family = "serif"),
        axis.title.y = element_text(color = "black", size = 25, face = "bold"),
        axis.text.y = element_text(size = 25, face = "bold", color = "black"),
        plot.margin = margin(0.7, 1.0, 0.7, 0.3, "cm"),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(color = "black", hjust = 0.5),
        legend.key.size = unit(2.0, "line"),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(color = "black", face = "bold",
                                   vjust = 1, size = 20, angle = 60, hjust = 1),
        legend.position = c(0.8, 0.75)) +
  scale_y_continuous(breaks = seq(0, 500, 50), expand = c(0, 0),
                     limits = c(0, 500)) +
  ylab(expression(bold(PM[2.5]~(mu*g/m^3)))) +
  scale_x_datetime(date_breaks = "14 days",
                   date_labels = "%b %d, %Y", expand = c(0, 0))
dev.off()
#-----------------------------------------------------

#--Time Variation Pot--------
jpeg("timevariation drrrrr allnew.jpeg",units="cm", width=56, height=35, res=300)
timeVariation(df_kumasi, pollutant =  'GKMA', key.columns = 2,key.position="bottom",
              par.settings=list(fontsize=list(text=22, family="Serif")),
              xlab = c("Hourly/Daily","Mean hour of the day (Jan-Jun 2022)","Monthly Average(Jan-Jun 2022)","Days of the Week(Jan-Jun 2022)"),
              cols= c("red","blue"),ylab="PM2.5(ug/m3)",ci=TRUE,avg.time = "hour",main="(PM 2.5) Time variation plot ",auto.text = TRUE, statistic = "mean")
dev.off()
#--------------------------

#----Calenderplot-------
jpeg("figures/Clarity/accracalenderplot.jpeg", units = "cm", width = 35, height = 20, res = 300)
calendarPlot(df_accra, pollutant = "GAMA", breaks = c(0, 12.1, 35.4, 55.5, 150.5, 250.4, 500),
             par.settings = list(fontsize = list(text = 22, family = "Serif")),
             main = "Greater Accra Municipal Area (GAMA)",
             labels = c('Good','Moderate','Unhealthy Sens.Groups','Unhealthy','Very unhealthy','Hazardous'),
             cols = c("green", "yellow", "orange", "red", "purple", "maroon"))
dev.off()
#---------------------
