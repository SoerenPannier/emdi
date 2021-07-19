library("ggplot2")
library("dlstats")

x <- cran_stats(c("emdi"))
x$downloads <- cumsum(x$downloads)


theme_set(theme_gray(base_size = 16))

ggplot(x, aes(end, downloads)) +
  geom_line(size = 2, color="#4F728E")  + ylab("Cumulative downloads") +
  xlab("Year") 
