library(readr)
library(openair)
library(tidyverse)

adabraka <- read.csv("C:\\Users\\USER\\Downloads\\Data export (46).csv")
colnames(adabraka) <- c("date", "FEM(T640)")

adabraka$date <- lubridate::ymd_hm(adabraka$date)



lcs <- df %>%
    select(date, St_Joseph_sch_Acc)

adabraka <- data.frame(period.apply(adabraka, endpoints(adabraka, "day"), colMeans, na.rm = TRUE))

setDT(adabraka, keep.rownames = "date")
adabraka <- adabraka %>%
  mutate(date1 = lubridate::ymd_hms(date))

#move column 'column' to first position
adabraka <- adabraka %>% select(date1, everything())
adabraka <- adabraka[, -2]
colnames(adabraka)[1] <- "date"

adabraka$FEM.T640.[adabraka$FEM.T640. < 1] <- NA

adabraka <- na.omit(adabraka)
summary(adabraka)
summary(lcs)

n <- Reduce(function(x,y) merge(x,y, all=T, by=c("date")), list(adabraka, lcs)) # nolint
colnames(n) <- c("date", "FEM", "lcs")
jpeg("figures/Clarity/test PM2.5 Time series plot.jpeg",units="cm", width=25, height=20, res=270)

timePlot(n, pollutant = c("FEM", "lcs"),
         stack =  FALSE, group = T, date.breaks = 10,
         par.settings = list(fontsize = list(text = 20, family = "Serif"),
         axis.line = list(col = "black", lwd = 2), add.text = list(fontface = "bold"),
         axis.text = list(fontface = "bold"), par.ylab.text = list(fontface = "bold")),
         y.relation = "free", lwd = 3,lty=1, avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM2.5 (ug/m3)",ylim=c(0,400),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2022-01-01", "2022-06-30")),
         cols = c("red","blue"),key.position = "inside")
dev.off()

a2 <- na.omit(n)

summary(a2)

cor(a2$lcs, a2$FEM, method = "pearson")^2
sqrt(mean((a2$lcs - a2$FEM)^2))
mean(abs(a2$lcs - a2$FEM))
#sqrt(mean((a1$Sensor_002 - a1$`FEM(T640)`)^2))/mean(a1$`FEM(T640)`)
mean(a2$lcs - a2$FEM)