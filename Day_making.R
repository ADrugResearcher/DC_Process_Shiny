library(magrittr)
library(lubridate)
library(tibble)
#Creates a data frame of date ranges to be used later for making sliding scale in network graph and line chart
everydate <- seq(ymd('2019-12-29'),floor_date(Sys.Date(), unit = "week", week_start = 1)+days(7),by = '1 day')
poss.w <- data.frame(Days = everydate, 
                     Bottom = floor_date(everydate, unit = "week", week_start = getOption("lubridate.week.start", 1)))
poss.w$Top <- poss.w$Bottom+days(6)

new_dat <- data.frame(value = unique(poss.w$Top)) %>%
  rowid_to_column("ID")
poss.w$ID <- new_dat$ID[match(poss.w$Top, new_dat$value)]
rm(new_dat)
poss.w$Days2 <- paste(month(poss.w$Bottom, label = TRUE)," ", day(poss.w$Bottom),
                      "-\n", month(poss.w$Top, label = TRUE)," ", day(poss.w$Top),"\n", year(poss.w$Top),sep = "")
poss.w$Days2 <- factor(poss.w$ID, labels = unique(poss.w$Days2), ordered = TRUE)
cheque <- read.csv("cheque_days.csv")
cheque$Days <- mdy(cheque$Days)
cheque$lower <- floor_date(cheque$Days, unit = "week", week_start = 1)
cheque$Days <- "cheque"
poss.w$Cheque <- cheque$Days[match(poss.w$Bottom, cheque$lower)]
poss.w$Cheque[is.na(poss.w$Cheque)] <- "Not Cheque"
