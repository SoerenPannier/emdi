library("ggplot2")
library("dlstats")

x <- cran_stats(c("emdi"))
x$downloads <- cumsum(x$downloads)


theme_set(theme_gray(base_size = 40))

ggplot(x, aes(end, downloads)) +
  geom_line(size = 2, color="#4F728E")  + ylab("Cumulative downloads") +
  xlab("Year") 
ggsave("../Thesis/Meine/Presentation/Figures/emdi_downloads.pdf", 
       width = 10, height = 10)
