library(tidyverse)
library(magrittr)
library(lubridate)

data.file.names <- c("yemen-cholera-2017-governorate-level.csv", "yemen-cholera-2017-country-level.csv")

download.file("https://docs.google.com/spreadsheets/d/1P0ob0sfz3xqG8u_dxT98YcVTMwzPSnya_qx6MbX-_Z8/pub?gid=0&single=true&output=csv",
              destfile=data.file.names[1])
download.file("https://docs.google.com/spreadsheets/d/1P0ob0sfz3xqG8u_dxT98YcVTMwzPSnya_qx6MbX-_Z8/pub?gid=27806487&single=true&output=csv",
              destfile=data.file.names[2])

fill.in.gaps <- function (incomplete.data, columns) {
  output <- lapply(columns, function (i) {
    na.spline <- splinefun(incomplete.data$Date, incomplete.data[[i]])
    missing.pos <- which(is.na(incomplete.data[[i]]))
    incomplete.data[[i]][missing.pos] <- na.spline(incomplete.data$Date[missing.pos])
    incomplete.data[[i]][missing.pos] <- 
      mapply(function (x, y, z) {
        output <- min(z, max(x, rnorm(1, y, (z-y)/20)))
        if (is.na(output)) return (y)
        output
      }, 
      incomplete.data[[i]][missing.pos-1], 
      incomplete.data[[i]][missing.pos], 
      incomplete.data[[i]][missing.pos+1])
    if (names(incomplete.data)[i] %in% c("Cases", "Deaths")) incomplete.data[[i]] %<>% round()
    incomplete.data[[i]]
  }) %>% 
    do.call(what=cbind)
  incomplete.data[, columns] <- output
  incomplete.data[, grep("CFR", names(incomplete.data))] <- incomplete.data$Deaths/incomplete.data$Cases %>%
    sapply(function (x) ifelse(is.na(x), 0, x))
  return (incomplete.data)
}


# Country-wide data -------------------------------------------------------

timeseries.yemen <- readr::read_csv(data.file.names[2])
complete.dates <- seq.Date(min(timeseries.yemen$Date), max(timeseries.yemen$Date), by="day")
added.dates <- data.frame(Date=complete.dates[!(complete.dates %in% timeseries.yemen$Date)]) 
added.dates %<>% data.frame(as.data.frame(lapply(1:(ncol(timeseries.yemen)-1), function (x) rep(NA, nrow(added.dates)))))
names(added.dates) <- names(timeseries.yemen)
timeseries.yemen %<>% rbind(., added.dates) %>%
  slice(order(Date))

timeseries.yemen %<>% fill.in.gaps(., 2:5)

timeseries.yemen$NewCases <- c(abs(diff(timeseries.yemen$Cases)), NA)
timeseries.yemen$NewDeaths <- c(abs(diff(timeseries.yemen$Deaths)), NA)

ggplot(timeseries.yemen %>% reshape2::melt(id.vars=c("Date", "Bulletin Type", "Bulletin URL"))) +
  theme_light() +
  geom_point(aes(x=Date, y=value)) +
  facet_wrap(~variable, ncol=1, scales="free_y")

ggplot(timeseries.yemen) +
  theme_light() +
  geom_point(aes(x=Date, y=NewCases))

timeseries.yemen.plot <- group_by(timeseries.yemen, aggDate=week(Date)) %>%
  summarize(NewCases=sum(NewCases), minDate=min(Date)) %>%
  ggplot(aes(x=minDate, y=NewCases))
timeseries.yemen.plot <- timeseries.yemen.plot + 
  theme_light() +
  geom_bar(stat="identity") +
  scale_x_date(date_labels="%b %d\n%Y") + 
  xlab("Date") + ylab("New Cases Per Week") + ggtitle("National Data From Yemen")
print(timeseries.yemen.plot)
ggsave("timeseries-national.pdf", timeseries.yemen.plot, width=4, height=3)



# Governorate-level data --------------------------------------------------
timeseries <- readr::read_csv(data.file.names[1])[, -7:-9]
timeseries$Governorate %<>% 
  gsub("_", " ", .) %>%
  gsub("-", " ", .) %>%
  gsub("Mahrah", "Maharah", .) %>%
  gsub("AL", "Al", .) %>%
  gsub("Ma'areb", "Marib", .) %>%
  factor()
governorates <- levels(timeseries$Governorate)

complete.empty.df <- data.frame(matrix(nrow=length(complete.dates), ncol=ncol(timeseries)))
names(complete.empty.df) <- names(timeseries)

timeseries <- lapply(1:length(governorates), function (i) {
  subdata <- filter(timeseries, (Governorate==governorates[i]))
  data.range <- range(subdata$Date)
  complete.dates <- seq.Date(data.range[1], data.range[2], "day")
  complete.empty.df %<>% slice(1:length(complete.dates))
  complete.empty.df$Date <- complete.dates
  complete.empty.df$Governorate <- governorates[i]
  date.matches <- match(complete.dates, subdata$Date)
  date.matches <- date.matches[!is.na(date.matches)]
  complete.empty.df[complete.dates %in% subdata$Date, c("Cases", "Deaths", "CFR (%)", "Attack Rate (per 1000)")] <-
    subdata[date.matches, -1:-2]
  complete.empty.df %<>% fill.in.gaps(., 3:6)
  complete.empty.df$NewCases <- c(abs(diff(complete.empty.df$Cases)), NA)
  complete.empty.df$NewDeaths <- c(abs(diff(complete.empty.df$Deaths)), NA)
  return(complete.empty.df)
}) %>%
  do.call(what=rbind.data.frame, .)



timeseries %<>% group_by(Governorate) %>%
  mutate(NewCases=c(abs(diff(Cases)), NA), NewDeaths=c(abs(diff(Deaths)), NA)) %>%
  ungroup()

timeseries %>% group_by(Month=format(Date, "%Y%m")) %>% summarize(num.loc=length(unique(Governorate)))

timeseries.plot <- group_by(timeseries, aggDate=week(Date), Governorate) %>%
  summarize(NewCases=sum(NewCases), minDate=min(Date)) %>%
  ggplot(aes(x=minDate, y=NewCases))
timeseries.plot <- timeseries.plot + 
  theme_light() +
  geom_bar(stat="identity") +
  scale_x_date(date_labels="%b") + 
  xlab("Date") + ylab("New Cases Per Week") + ggtitle("Governorates in Yemen") +
  facet_wrap(~Governorate, ncol=3)
print(timeseries.plot)
ggsave("timeseries-governorate.pdf", timeseries.plot, width=6, height=10)

saveRDS(timeseries.yemen, "timeseries-national.rds")
saveRDS(timeseries, "timeseries-governorate.rds")

write.csv(timeseries.yemen, "timeseries-national.csv", quote = FALSE, row.names = FALSE)
write.csv(timeseries, "timeseries-governorate.csv", quote = FALSE, row.names = FALSE)
