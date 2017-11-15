aggregate.by.week <- function (x) {
  require(magrittr)
  if ("Governorate" %in% names(x)) {
    groupby <- group_by(x, aggDate=week(Date), Governorate)
  } else {
    groupby <- group_by(x, aggDate=week(Date))
  }
  output <- groupby %>% summarize(NewCases=sum(NewCases), minDate=min(Date))
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

combine.spatiotemporal <- function (spatial, temporal.df, time) {
  output <- temporal.df %>%
    filter(aggDate==time) %>%
    mutate(id=as.character(match(Governorate, spatial$info$admin1Name_en))) %>%
    inner_join(spatial$shp, .)
  output$NewCases[is.na(output$NewCases)] <- 0
  output
}

get.week.string <- function (input.date) {
  input.date %>% 
    floor_date(unit="weeks") %>% 
    `+`(c(0, 6)) %>% 
    format("%b %d") %>% 
    paste(., collapse=" to ") %>% 
    paste0("Week of ", ., " 2017")
}

plot.spatiotemporal <- function (x, cols=c("#FFF5EE", "#CD3333")) {
  require(ggplot2)
  ggplot(x, aes(x = long, y = lat, group = group, fill=NewCases, frame=aggDate)) + 
    theme_classic() +
    geom_polygon(color = 'gray') +
    theme(axis.text=element_blank(), 
          axis.title=element_blank(),
          axis.ticks=element_blank()) +
    scale_fill_gradient(low=cols[1], high=cols[2]) +
    ggtitle(get.week.string(x$minDate[1]))
}