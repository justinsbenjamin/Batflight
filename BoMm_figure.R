library(ggplot2)

meaningful_flight <- ggplot()  + 
  theme(axis.title.y =element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_x_continuous(limits = c(-1, 10), breaks = seq(-1,10,1)) +
  ylab("") + xlab("Flight Time (s)")  +
  geom_vline(xintercept = -1, linetype = "dashed") + geom_rect(aes(xmin = -1, xmax = 2, ymin = 0, ymax = 10), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 2, linetype = "dashed") + geom_rect(aes(xmin = 2, xmax = 5, ymin = 0, ymax = 10), fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = 5, linetype = "dashed") + geom_rect(aes(xmin = 5, xmax = 10, ymin = 0, ymax = 10), fill = "purple", alpha = 0.5) + 
  annotate("text", x = 0.5, y = 8, label = "Judged to not be of practical \n or meaningful biological \n relevance") +
  annotate("text", x = 3.5, y = 8, label = "Grey Area") +
  annotate("text", x = 7, y = 8, label = "Judged to be of practical \n or meaningful biological \n relevance") 

meaningful_flight
