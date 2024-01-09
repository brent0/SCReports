require(aegis)
require(bio.snowcrab)
require(lubridate)

Sensor.validate = function () {
set = snowcrab.db( DS="set.clean", p=p )

set$spread = set$spread*1000

table1 <- set %>%
  group_by(yr) %>%
  summarise(mean.spread = mean(spread), sd = sd(spread))
table1



ggplot(table1, aes(yr, mean.spread), col = "blue") +
  ggtitle("Snowcrab Survey (Mean Spread) ") +
  geom_point(size = 1, stroke = .5) + geom_line(size = .5) + xlab("Year") + ylab("Mean Spread (m)") +
  geom_errorbar(aes(ymin=mean.spread-sd, ymax=mean.spread+sd), width=.2)+ 
  annotate("rect", xmin = 1995, xmax = 2013.5, ymin = 6, ymax = 12, fill = "yellow", alpha = .2)+
  annotate("text", x = 2003, y = 7, label = "Netmind")+
  annotate("rect", xmin = 2013.5, xmax = 2019.5, ymin = 6, ymax = 12, fill = "green", alpha = .2)+
  annotate("text", x = 2016, y = 7, label = "E-sonar")+
  annotate("rect", xmin = 2019.5, xmax = 2023.5, ymin = 6, ymax = 12, fill = "blue", alpha = .2)+
  annotate("text", x = 2022, y = 7, label = "Marport")+
  geom_hline(yintercept = mean(table1$mean.spread))+
  theme_bw()



ggplot(set, aes(yr, spread), col = "blue") +
  ggtitle("Snowcrab Survey (Mean Spread @ Station) ") +
  geom_point(size = 1, stroke = .5, alpha = .1) + xlab("Year") + ylab("Mean Spread @ Station (m)") +
  annotate("rect", xmin = 1995, xmax = 2013.5, ymin = 6, ymax = 12, fill = "yellow", alpha = .2)+
  annotate("text", x = 2003, y = 7, label = "Netmind")+
  annotate("rect", xmin = 2013.5, xmax = 2019.5, ymin = 6, ymax = 12, fill = "green", alpha = .2)+
  annotate("text", x = 2016, y = 7, label = "E-sonar")+
  annotate("rect", xmin = 2019.5, xmax = 2023.5, ymin = 6, ymax = 12, fill = "blue", alpha = .2)+
  annotate("text", x = 2022, y = 7, label = "Marport")+
  geom_hline(yintercept = mean(set$spread))+
  theme_bw()


setsub = set[which(set$z <= 100),]


ggplot(setsub, aes(yr, spread), col = "blue") +
  ggtitle("Snowcrab Survey (Mean Spread) ") +
  geom_point(size = 1, stroke = .5, alpha = .2) + xlab("Year") + ylab("Mean Spread @ Station (m)") +
  annotate("rect", xmin = 1995, xmax = 2013.5, ymin = 6, ymax = 12, fill = "yellow", alpha = .2)+
  annotate("text", x = 2003, y = 7, label = "Netmind")+
  annotate("rect", xmin = 2013.5, xmax = 2019.5, ymin = 6, ymax = 12, fill = "green", alpha = .2)+
  annotate("text", x = 2016, y = 7, label = "E-sonar")+
  annotate("rect", xmin = 2019.5, xmax = 2023.5, ymin = 6, ymax = 12, fill = "blue", alpha = .2)+
  annotate("text", x = 2022, y = 7, label = "Marport")+
  geom_hline(yintercept = mean(setsub$spread))+
  theme_bw()






table1 <- set %>%
  group_by(yr) %>%
  summarise(mean.spread = mean(spread))
table1

table1$mean.spread = table1$mean.spread*1000 
ggplot(table1, aes(yr, mean.spread), col = "blue") +
  ggtitle("Snowcrab Survey (Mean Spread) ") +
  geom_point(size = 1, stroke = .5) + geom_line(size = .5) + xlab("Year") + ylab("Mean Spread (m)") +
  annotate("rect", xmin = 1995, xmax = 2013.5, ymin = 6, ymax = 12, fill = "yellow", alpha = .2)+
  annotate("text", x = 2003, y = 7, label = "Netmind")+
  annotate("rect", xmin = 2013.5, xmax = 2019.5, ymin = 6, ymax = 12, fill = "green", alpha = .2)+
  annotate("text", x = 2016, y = 7, label = "E-sonar")+
  annotate("rect", xmin = 2019.5, xmax = 2023.5, ymin = 6, ymax = 12, fill = "blue", alpha = .2)+
  annotate("text", x = 2022, y = 7, label = "Marport")+
  theme_bw()



table1 <- set %>%
  group_by(yr) %>%
  summarise(mean.distance = mean(distance))
table1

table1$mean.spread = table1$mean.distance*1000 
ggplot(table1, aes(yr, mean.distance), col = "blue") +
  ggtitle("Snowcrab Survey (Mean Distance) ") +
  geom_point(size = 1, stroke = .5) + geom_line(size = .5) + xlab("Year") + ylab("Mean diatance (km)") +
  annotate("rect", xmin = 1995, xmax = 2013.5, ymin = .60, ymax = .62, fill = "yellow", alpha = .2)+
  annotate("text", x = 2003, y = .61, label = "Netmind")+
  annotate("rect", xmin = 2013.5, xmax = 2019.5, ymin = .60, ymax = .62, fill = "green", alpha = .2)+
  annotate("text", x = 2016, y = .61, label = "E-sonar")+
  annotate("rect", xmin = 2019.5, xmax = 2023.5, ymin = .60, ymax = .62, fill = "blue", alpha = .2)+
  annotate("text", x = 2022, y = .61, label = "Marport")+
  
  annotate("rect", xmin = 1995, xmax = 2003.5, ymin = .57, ymax = .59, fill = "pink", alpha = .2)+
  annotate("text", x = 1999, y = .58, label = "Marco Michel")+
  annotate("rect", xmin = 2003.5, xmax = 2013.5, ymin = .57, ymax = .59, fill = "darkolivegreen1", alpha = .2)+
  annotate("text", x = 2009, y = .58, label = "Gentle Lady")+
  annotate("rect", xmin = 2013.5, xmax = 2022.5, ymin = .57, ymax = .59, fill = "lightblue2", alpha = .2)+
  annotate("text", x = 2020, y = .58, label = "Miss Jessie")+
  
  
  annotate("rect", xmin = 1995, xmax = 2003.5, ymin = .54, ymax = .56, fill = "magenta2", alpha = .2)+
  annotate("text", x = 1999, y = .55, label = "Luke Savoie")+
  annotate("rect", xmin = 2003.5, xmax = 2020, ymin = .54, ymax = .56, fill = "seagreen2", alpha = .2)+
  annotate("text", x = 2010, y = .55, label = "Ben Zisserson")+
  annotate("rect", xmin = 2020, xmax = 2022.5, ymin = .54, ymax = .56, fill = "steelblue1", alpha = .2)+
  annotate("text", x = 2023, y = .55, label = "Amy Glass\n & Brent Cameron")+
  
  annotate("rect", xmin = 1995, xmax = 2003.5, ymin = .51, ymax = .53, fill = "pink", alpha = .2)+
  annotate("text", x = 1999, y = .52, label = "???")+
  annotate("rect", xmin = 2003.5, xmax = 2019.5, ymin = .51, ymax = .53, fill = "darkolivegreen1", alpha = .2)+
  annotate("text", x = 2009, y = .52, label = "John Baker")+
  annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = .51, ymax = .53, fill = "lightblue2", alpha = .2)+
  annotate("text", x = 2023, y = .52, label = "Travis Lennon(3/4 2021)\n Coalie Deon (1/4 2021, 2022)") +
  
  theme_bw()



table1 <- set %>%
  group_by(yr) %>%
  summarise(mean.dt = mean(dt))
table1

table1$mean.spread = table1$mean.dt 
ggplot(table1, aes(yr, mean.dt), col = "blue") +
  ggtitle("Snowcrab Survey (Mean Down Time) ") +
  geom_point(size = 1, stroke = .5) + geom_line(size = .5) + xlab("Year") + ylab("Mean down time (mins)") +
  annotate("rect", xmin = 1995, xmax = 2013.5, ymin = 6, ymax = 6.2, fill = "yellow", alpha = .2)+
  annotate("text", x = 2003, y = 6.1, label = "Netmind")+
  annotate("rect", xmin = 2013.5, xmax = 2019.5, ymin = 6, ymax = 6.2, fill = "green", alpha = .2)+
  annotate("text", x = 2016, y = 6.1, label = "E-sonar")+
  annotate("rect", xmin = 2019.5, xmax = 2023.5, ymin = 6, ymax = 6.2, fill = "blue", alpha = .2)+
  annotate("text", x = 2022, y = 6.1, label = "Marport")+
  
  annotate("rect", xmin = 1995, xmax = 2003.5, ymin = 5.7, ymax = 5.9, fill = "pink", alpha = .2)+
  annotate("text", x = 1999, y = 5.8, label = "Marco Michel")+
  annotate("rect", xmin = 2003.5, xmax = 2013.5, ymin = 5.7, ymax = 5.9, fill = "darkolivegreen1", alpha = .2)+
  annotate("text", x = 2009, y = 5.8, label = "Gentle Lady")+
  annotate("rect", xmin = 2013.5, xmax = 2022.5, ymin = 5.7, ymax = 5.9, fill = "lightblue2", alpha = .2)+
  annotate("text", x = 2020, y = 5.8, label = "Miss Jessie")+
  
  
  annotate("rect", xmin = 1995, xmax = 2003.5, ymin = 5.4, ymax = 5.6, fill = "magenta2", alpha = .2)+
  annotate("text", x = 1999, y = 5.5, label = "Luke Savoie")+
  annotate("rect", xmin = 2003.5, xmax = 2020, ymin = 5.4, ymax = 5.6, fill = "seagreen2", alpha = .2)+
  annotate("text", x = 2010, y = 5.5, label = "Ben Zisserson")+
  annotate("rect", xmin = 2020, xmax = 2022.5, ymin = 5.4, ymax = 5.6, fill = "steelblue1", alpha = .2)+
  annotate("text", x = 2023, y = 5.5, label = "Amy Glass\n & Brent Cameron")+
  
  annotate("rect", xmin = 1995, xmax = 2003.5, ymin = 5.1, ymax = 5.3, fill = "pink", alpha = .2)+
  annotate("text", x = 1999, y = 5.2, label = "???")+
  annotate("rect", xmin = 2003.5, xmax = 2019.5, ymin = 5.1, ymax = 5.3, fill = "darkolivegreen1", alpha = .2)+
  annotate("text", x = 2009, y = 5.2, label = "John Baker")+
  annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = 5.1, ymax = 5.3, fill = "lightblue2", alpha = .2)+
  annotate("text", x = 2023, y = 5.2, label = "Travis Lennon(3/4 2021)\n Coalie Deon (1/4 2021, 2022)") +
  
  theme_bw()

coeff <- 5
table1 <- set %>%
  group_by(yr) %>%
  summarise(mean.spread = mean(spread))
table1
table2 <- set %>%
  group_by(yr) %>%
  summarise(mean.vel = mean(vel))
table2
set = set[-which(set$vel >= 3),]

ggplot(set, aes(x = yr)) +
  geom_point(aes(y=spread), size = 1, stroke = .5, alpha = .1, col = "darkblue")+
  stat_summary(aes(y=spread), geom = "line",
    fun = "mean",
    col = "darkblue",
    size = .5)+
  geom_point(aes(x = yr+.2, y=vel*coeff), size = 1, stroke = .5, alpha = .1, col = "darkorange3")+
  stat_summary(aes(x = yr+.2, y=vel*coeff), geom = "line",
               fun = "mean",
               col = "darkorange3",
               size = .5)+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Spread",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Speed")
  )+
  xlab("Year") + 
  ggtitle("SPREAD(m) vs SPEED(kts) @ Station by yr")+
  annotate("rect", xmin = 1995, xmax = 2013.5, ymin = 4, ymax = 15, fill = "yellow", alpha = .2)+
  annotate("text", x = 2003, y = 14, label = "Netmind")+
  annotate("rect", xmin = 2013.5, xmax = 2019.5, ymin = 4, ymax = 15, fill = "green", alpha = .2)+
  annotate("text", x = 2016, y = 14, label = "E-sonar")+
  annotate("rect", xmin = 2019.5, xmax = 2023.5, ymin = 4, ymax = 15, fill = "blue", alpha = .2)+
  annotate("text", x = 2022, y = 14, label = "Marport")+
  theme_bw()+
  theme(
    axis.title.y.right = element_text(color = "darkorange3"),
    axis.title.y.left = element_text(color = "darkblue"),
    axis.ticks.y.right = element_line(color = "darkorange3"),
    axis.text.y.right = element_text(color = "darkorange3"), 
    axis.ticks.y.left = element_line(color = "darkblue"),
    axis.text.y.left = element_text(color = "darkblue")
    
    )
}

