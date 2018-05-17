# |------------------------------------------------------------------------|
# | Project: Batlight Plot                                                 |
# | Script:   POC quick-n-dirty script                                     |
# | Authors:  Davit Sargsyan, Traymon Beavers, ...                         |   
# | Created:  05/16/2018                                                   |
# | Modified:                                                              |
# |------------------------------------------------------------------------|
sink(file = "tmp/log_batlight.plot_poc_v1.txt")

# Header----
require(data.table)
require(ggplot2)
require(ggforce)

# Simulate data----
dt1 <- data.table(trt = factor(LETTERS[1:5]),
                  est = c(0.9, 1.1, 1.3, 0.8, 2),
                  errbar = c(0.01, 0.15, 0.2, 0.15, 0.2),
                  sign = c("**", "", "**", "**", "**"))
dt1[, lb := est - errbar]
dt1[, ub := est + errbar]
dt1

# Forest plot----
p1 <- ggplot(dt1) +
  geom_errorbar(aes(x = trt,
                    ymin = lb, 
                    ymax = ub), 
                width = 0.05) +
  geom_point(aes(x = trt,
                 y = est), 
             shape = 21,
             size = 3,
             fill = "red") +
  geom_text(aes(x = trt,
                y = ub + 0.02,
                label = sign)) +
  geom_hline(yintercept = 1,
             linetype = "dashed")
p1

# Forest plot with magnification----
light.origin <- list(x = 1, 
                     y = 1)
light.circle.r <- 0.5
light.circle.origin <- list(x = light.origin$x + light.circle.r,
                            y = 2.5) 
delta <- 0.3


p2 <- ggplot(dt1) + 
  geom_segment(aes(x = light.origin$x,
                   xend = light.origin$x,
                   y = light.origin$y,
                   yend = light.circle.origin$y),
               linetype = "dashed") +
  geom_segment(aes(x = light.origin$x,
                   xend = light.origin$x + 2*light.circle.r,
                   y = light.origin$y,
                   yend = light.circle.origin$y),
               linetype = "dashed") +
  geom_circle(aes(x0 = light.circle.origin$x,
                  y0 = light.circle.origin$y,
                  r = light.circle.r),
              linetype = "dotted",
              fill = "white") +
  geom_segment(aes(x = light.circle.origin$x - light.circle.r,
                   xend = light.circle.origin$x + light.circle.r,
                   y = light.circle.origin$y,
                   yend = light.circle.origin$y),
               linetype = "dashed") +
  geom_point(aes(x = light.circle.origin$x,
                 y = light.circle.origin$y - delta),
             size = 2) +
  geom_errorbar(aes(x = light.circle.origin$x,
                    ymin = light.circle.origin$y - delta + 0.1, 
                    ymax = light.circle.origin$y - delta - 0.1), 
                width = 0.05) +
  geom_text(aes(x = light.circle.origin$x,
                y = light.circle.origin$y - delta + 0.15,
                label = "**")) +
  geom_point(aes(x = as.numeric(trt),
                 y = est),
             size = 2) +
  geom_errorbar(aes(x = as.numeric(trt),
                    ymin = lb, 
                    ymax = ub), 
                width = 0.05) +
  geom_text(aes(x = as.numeric(trt),
                y = 1.05*ub,
                label = sign)) + 
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_continuous("Treatment",
                     breaks = unique(as.numeric(dt1$trt)),
                     labels = levels(dt1$trt)) +
  scale_y_continuous("Estimate") +
  ggtitle("Batlight Plot") +
  theme(axis.text.x = element_text(angle = 0,
                                   hjust = 0.5),
        plot.title = element_text(hjust = 0.5))
p2

sessionInfo()
sink()