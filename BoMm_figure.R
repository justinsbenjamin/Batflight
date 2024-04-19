library(ggplot2)

meaningful_flight <- ggplot()  + 
  theme(axis.title.y =element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0,10,1)) +
  ylab("") + xlab("Flight Time (s)")  +
  geom_vline(xintercept = 0, linetype = "dashed") + geom_rect(aes(xmin = 0, xmax = 2, ymin = 0, ymax = 10), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 2, linetype = "dashed") + geom_rect(aes(xmin = 2, xmax = 5, ymin = 0, ymax = 10), fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = 5, linetype = "dashed") + geom_rect(aes(xmin = 5, xmax = 10, ymin = 0, ymax = 10), fill = "purple", alpha = 0.5) + 
  annotate("text", x = 1, y = 8, label = "Judged to not be of practical \n or meaningful biological \n relevance") +
  annotate("text", x = 3.5, y = 8, label = "Grey Area") +
  annotate("text", x = 7.5, y = 8, label = "Judged to be of practical \n or meaningful biological \n relevance") 

meaningful_flight

meaningful_mass_fasting <- ggplot()  + 
  theme(axis.title.y =element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5,5,1)) +
  ylab("") + xlab("Change in Mass (g)")  +
  geom_vline(xintercept = -5, linetype = "dashed") + geom_rect(aes(xmin = -5, xmax = -1, ymin = 0, ymax = 10), fill = "purple", alpha = 0.5) +
  geom_vline(xintercept = -1, linetype = "dashed") + geom_rect(aes(xmin = -0.1, xmax = -1, ymin = 0, ymax = 10), fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = -0.1, linetype = "dashed") + geom_rect(aes(xmin = -0.1, xmax = 5, ymin = 0, ymax = 10), fill = "blue", alpha = 0.5) + 
  annotate("text", x = -3, y = 8, label = "Judged to be of practical \n or meaningful biological \n relevance") +
  annotate("text", x = -0.55, y = 8, label = "Grey\nArea") +
  annotate("text", x = 2.5, y = 8, label = "Judged to not be of practical \n or meaningful biological \n relevance") 

meaningful_mass_fasting

meaningful_mass_treatment <- ggplot()  + 
  theme(axis.title.y =element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2,2,1)) +
  ylab("") + xlab("Additional Change in Mass (g) for Treatment Group")  +
  geom_vline(xintercept = -2, linetype = "dashed") + geom_rect(aes(xmin = -2, xmax = -0.25, ymin = 0, ymax = 10), fill = "purple", alpha = 0.5) +
  geom_vline(xintercept = -0.25, linetype = "dashed") + geom_rect(aes(xmin = -0.1, xmax = -0.25, ymin = 0, ymax = 10), fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = -0.1, linetype = "dashed") + geom_rect(aes(xmin = -0.1, xmax = 2, ymin = 0, ymax = 10), fill = "blue", alpha = 0.5) + 
  annotate("text", x = -1.1, y = 8, label = "Judged to \n be of practical \n or meaningful \n biological \n relevance") +
  annotate("text", x = -0.175, y = 8, label = "Grey\nArea") +
  annotate("text", x = 1, y = 8, label = "Judged to \n not be of practical \n or meaningful \n biological \n relevance") 

meaningful_mass_treatment
