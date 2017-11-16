aggregate.by.week <- function (x) {
  require(magrittr)
  require(lubridate)
  if ("Governorate" %in% names(x)) {
    x <- split(x, week(x$Date)) %>%
      lapply(function (y) {
        notin <- !(unique(x$Governorate) %in% y$Governorate)
        if (any(notin)) {
          y <- rbind(y, y[1:sum(notin), ])
          y[nrow(y)-(sum(notin):1)+1, "Governorate"] <- unique(x$Governorate)[notin]
          y[nrow(y)-(sum(notin):1)+1, "NewCases"] <- 0
        }
        y
      }) %>%
      do.call(what=rbind)
    groupby <- group_by(x, aggDate=week(Date), Governorate)
  } else {
    groupby <- group_by(x, aggDate=week(Date))
  }
  output <- groupby %>% summarize(NewCases=sum(NewCases)) %>% 
    mutate(minDate=floor_date(as.Date("2016-12-31")+aggDate*7, unit="weeks"))
}

plot.weekly.ts <- function (x, xlab="Date", ylab="New Cases Per Week", 
                            title="Incidence of cholera") {
  require(ggplot2)
  ggplot(x, aes(x=minDate, y=NewCases)) + 
    theme_light() +
    geom_bar(stat="identity") +
    scale_x_date(date_labels="%b %d\n%Y") +
    xlab(xlab) + ylab(ylab) + ggtitle(title)
}

combine.spatiotemporal <- function (spatial, temporal.df) {
  output <- temporal.df %>%
    mutate(id=as.character(as.numeric(match(Governorate, spatial$shp$name_en)-1))) %>%
    full_join(spatial$shpdf, .)
  output$NewCases[is.na(output$NewCases)] <- 0
  output
}

get.week.string <- function (input.date) {
  input.date %>% 
    floor_date(unit="weeks") %>% 
    `+`(c(0, 6)) %>% 
    format("%b %d") %>% 
    paste(., collapse=" to ") %>% 
    paste0(., " 2017")
}

plot.spatiotemporal <- function (x, cols=c("#FFF5EE", "#CD3333")) {
  require(ggplot2)
  title.vec <- sapply(unique(x$minDate), get.week.string)
  positions <- x %>% 
    split(., .$id) %>% 
    lapply(function (x) c(x=mean(range(x$long)), y=mean(range(x$lat)), id=as.character(x$Governorate[1]))) %>% 
    do.call(what=rbind)
  x %<>% filter(!is.na(aggDate))
  ggplot(x, aes(x = long, y = lat, group = group, fill=NewCases, 
                frame=paste0("Week ", aggDate, ": ", title.vec[aggDate-min(aggDate, na.rm=TRUE)+1]))) + 
    theme_bw() +
    geom_polygon(color = 'gray') +
    annotate("text", x=as.numeric(positions[, 1]), y=as.numeric(positions[, 2]), label=positions[, 3]) +
    theme(axis.text=element_blank(), 
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          title=element_text(hjust=0.5)) +
    scale_fill_gradient("New Cases", low=cols[1], high=cols[2])
}