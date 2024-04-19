library(ggplot2)

meaningful_flight <- ggplot()  + 
  theme(axis.title.y =element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_x_continuous(limits = c(-100, 300), breaks = seq(-100,300,50)) +
  ylab("") + xlab("Percent Change in Flight Time")  +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red", size =1.5) + geom_rect(aes(xmin = -100, xmax = 30, ymin = 0, ymax = 10), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 70, linetype = "dashed") + geom_rect(aes(xmin = 30, xmax = 70, ymin = 0, ymax = 10), fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = , linetype = "dashed") + geom_rect(aes(xmin = 70, xmax = 300, ymin = 0, ymax = 10), fill = "purple", alpha = 0.5) + 
  annotate("text", x = -35, y = 5, label = "Judged to not be of practical \n or meaningful biological \n relevance") +
  annotate("text", x = 50, y = 5, label = "Grey \nArea") +
  annotate("text", x = 175, y = 5, label = "Judged to be of practical \n or meaningful biological \n relevance") 

meaningful_flight

meaningful_mass <- ggplot()  + 
  theme(axis.title.y =element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_x_continuous(limits = c(-25, 5), breaks = seq(-25,5,5)) +
  ylab("") + xlab("Percent Change in Mass")  +
  geom_vline(xintercept = , linetype = "dashed") + geom_rect(aes(xmin = -25, xmax = -7, ymin = 0, ymax = 10), fill = "purple", alpha = 0.5) +
  geom_vline(xintercept = -7, linetype = "dashed") + geom_rect(aes(xmin = -7, xmax = -5, ymin = 0, ymax = 10), fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = -5, linetype = "dashed", color = "red", size =1.5) + geom_rect(aes(xmin =-5 , xmax = 5, ymin = 0, ymax = 10), fill = "blue", alpha = 0.5) + 
  annotate("text", x = -16, y = 5, label = "Judged to \n be of practical \n or meaningful \n biological \n relevance") +
  annotate("text", x = -6, y = 5, label = "Grey\nArea") +
  annotate("text", x = -0, y = 5, label = "Judged to \n not be of practical \n or meaningful \n biological \n relevance") 

meaningful_mass


