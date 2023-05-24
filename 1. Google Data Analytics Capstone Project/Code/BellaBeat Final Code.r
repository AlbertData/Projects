install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('skimr')
install.packages('ggplot2')
install.packages('readr')
install.packages("ggrepel")
install.packages("RColorBrewer")
install.packages("data.table")

library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(ggplot2)
library(readr)
library(ggrepel)
library(RColorBrewer)
library(data.table)


daily_activity <- fread("https://raw.githubusercontent.com/AlbertData/Datasets/main/Raw%20Datasets/BellaBeat%20Fitness%20Dataset/dailyActivity_merged.csv")
daily_sleep <- fread("https://raw.githubusercontent.com/AlbertData/Datasets/main/Raw%20Datasets/BellaBeat%20Fitness%20Dataset/sleepDay_merged.csv")
daily_steps <- fread("https://raw.githubusercontent.com/AlbertData/Datasets/main/Raw%20Datasets/BellaBeat%20Fitness%20Dataset/dailySteps_merged.csv")
hourly_steps <- fread("https://raw.githubusercontent.com/AlbertData/Datasets/main/Raw%20Datasets/BellaBeat%20Fitness%20Dataset/hourlySteps_merged.csv")



daily_activity <- daily_activity %>% 
                  rename(date = ActivityDate) %>%
                  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>% 
               rename(date = SleepDay) %>%
               mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p"))

daily_steps <- daily_steps %>% 
               rename(date = ActivityDay) %>%
               mutate(date = as_date(date, format = "%m/%d/%Y"))


hourly_steps <- hourly_steps %>%
                rename(date_time = ActivityHour) %>%
                mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", 
                       tz = Sys.timezone()))

daily_sleep <- distinct(daily_sleep)
View(sum(duplicated(daily_sleep)))

daily_activity_sleep <- merge(daily_activity, daily_sleep, by = c('Id','date'))

daily_activity_sleep <- daily_activity_sleep %>%
   mutate(week_day = weekdays(date))





names(daily_activity) <- tolower(names(daily_activity))

names(daily_sleep) <- tolower(names(daily_sleep))

names(daily_steps) <- tolower(names(daily_steps))

names(hourly_steps) <- tolower(names(hourly_steps))

names(daily_activity_sleep) <- tolower(names(daily_activity_sleep))



View(daily_activity_sleep)


user_category <- daily_activity %>%
                    group_by(id) %>%
                    summarise(days_used = n())
      
usertype <- user_category %>% 
            mutate(usage = case_when(days_used >= 0 & days_used < 15 ~ "0 - 15 Days ~ Rarely", 
                                     days_used >= 15 & days_used < 25 ~ "15 - 25 Days ~ Often", 
                                     days_used >= 25 ~ "25+ Days ~ Regularly"))


category_counts <- table(usertype$usage)
total_entries <- sum(category_counts)
usage_percentages <- prop.table(category_counts) * 100


usage_table <- as.data.frame(usage_percentages)
usage_table <- usage_table %>%
          arrange(Freq) 

View(usage_table)


palette <-  brewer.pal(11, "Spectral")
selected_colors <- palette[9:11]


plot1 <- ggplot(usage_table, aes(x = "", y = Freq, fill = factor(Var1))) +
              geom_bar(width = 1, stat = "identity") +
              coord_polar("y", start = 1100) +
              theme_void() +
              theme(legend.position = "right",
                    legend.key.size = unit(2, 'cm'),
                    legend.title = element_text(size=23),
                    legend.text = element_text(size=24)) +
              labs(fill = "How often do users use their devices")



plot1A <- plot1 +  
                  geom_rect(aes(xmin = -0.1, xmax = -0.6, ymin = -0.0005, ymax = -0.001), fill = "white", color = NA)+
                  scale_fill_manual(values = selected_colors)+
                  annotate("text", x = 1, y = 1, label = "9.1%", 
                           vjust = -3.5, hjust = 1.65, 
                           color = "#000000", size = 8.2, angle = 2, fontface = 2) +
                  annotate("text", x = 1, y = 1, label = "3%", 
                            vjust = -2.8, hjust = 0.9, 
                            color = "#000000", size = 8.2, angle = 340, fontface = 2) +
                  annotate("text", x = 1, y = 1, label = "87.9%", 
                           vjust = -6, hjust = 2.25, 
                           color = "#000000", size = 8.2, angle = 30, fontface = 2)

print(plot1A)




time_to_sleep <- daily_sleep %>%
                 mutate(time_taken = (totaltimeinbed - totalminutesasleep)- 5)


total_minutes_asleep_in_bed <- time_to_sleep %>%
                               group_by(id) %>% 
                               summarize(total_time_in_bed = sum(totaltimeinbed),
                                      total_minutes_asleep = sum(totalminutesasleep),
                                 total_time_taken_to_sleep = (sum(time_taken)),
                                             count_entries = sum(n()))


avgs_minutes_taken <- total_minutes_asleep_in_bed %>%
                      group_by(id) %>%
                      summarize(avg_time_in_bed = total_time_in_bed / count_entries,
                             avg_minutes_asleep = total_minutes_asleep / count_entries,
                     avg_minutes_taken_to_sleep = total_time_taken_to_sleep / count_entries)


avgs_minutes_sleeping <- avgs_minutes_taken %>% 
                         filter(avg_minutes_taken_to_sleep >= 0) %>%
                         mutate(user_number = row_number())
 
 avgs_minutes_sleep <- avgs_minutes_sleeping %>%
                        filter(avg_minutes_taken_to_sleep >= 0)
                        
avgs_of_minutes_taken <- arrange(avgs_minutes_sleep, avg_minutes_taken_to_sleep)
avgs_of_minutes_taken$user_number <- factor(avgs_of_minutes_taken$user_number)
avgs_minutes_sleeping <- arrange(avgs_minutes_sleeping, avg_time_in_bed)


View(avgs_minutes_sleeping)
View(avgs_of_minutes_taken)


plot2A <- ggplot(avgs_minutes_sleeping, aes(x = reorder(user_number, avg_minutes_asleep)), y = avg_time_in_bed) +
                   geom_bar(aes(fill = "avg_minutes_in_bed", y = avg_time_in_bed), stat = "identity", color = "#1fc0ff") +
                   geom_bar(aes(fill = "avg_minutes_asleep", y = avg_minutes_asleep), stat = "identity", color = "#28ff77") +
                   scale_fill_manual(values = c("avg_minutes_in_bed" = "#3434ff", "avg_minutes_asleep" = "#29ff34"),
                   labels = c("Minutes Asleep", "Minutes In Bed")) +
                   coord_fixed(ratio = 0.015) +
                   theme_bw()+
                   scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 50))+
                   labs(fill = "",
                        title = "Time spent in bed vs Time spent asleep", 
                            x = "User", 
                            y = "Minutes")+
                  theme(axis.text.x = element_text(size = 12), 
                        axis.text.y = element_text(size = 12),
                        axis.title.y = element_text(size = 18),
                        axis.title.x = element_text(size = 18),
                        plot.title = element_text(size = 20, face = "bold"),
                        legend.key.size = unit(1, 'cm'),
                        legend.title = element_text(size=18),
                        legend.text = element_text(size=20))
                        
print(plot2A)



plot2B <- ggplot(avgs_of_minutes_taken, aes(x = reorder(user_number, avg_minutes_taken_to_sleep), y = avg_minutes_taken_to_sleep)) + 
                       geom_point(color = "purple", size = 5) +
                       geom_segment(aes(x = user_number, xend = user_number, y = 0, yend = (avg_minutes_taken_to_sleep - 0.5)), color = "#f9952b", size = 1.5)+
                       coord_fixed(ratio = 0.28) +
                       theme_light()+
                       scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5))+
                       labs(title = "Average Time Taken To Fall Asleep", 
                                x = "User", 
                                y = "Minutes") +
                       theme(axis.text.x = element_text(size = 15), 
                             axis.text.y = element_text(size = 15),
                             axis.title.y = element_text(size = 20),
                             axis.title.x = element_text(size = 20),
                             plot.title = element_text(size = 20, face = "bold")) 

print(plot2B)








average_steps_per_day <- daily_steps %>% 
                         group_by(id) %>% 
                         summarize(avg_steps_per_day = mean(steptotal),
                                         total_steps = sum(steptotal))

average_sleep_per_day <- daily_activity_sleep %>% 
                         group_by(id) %>% 
                         summarize(avg_sleep_per_day = mean(totalminutesasleep),
                                         total_sleep = sum(totalminutesasleep))

average_steps_sleep <- merge(average_steps_per_day, average_sleep_per_day, by = "id")


View(average_steps_sleep)



plot3 <- ggplot(average_steps_sleep, aes(x= avg_steps_per_day, y= avg_sleep_per_day))+
         geom_point(fill = "#f700ff", size = 5, color = "#9d00ff")+
         coord_fixed(ratio = 15) +
         theme_bw()+
         labs(title = "Average Steps Vs Average Sleep", 
                   x= "Average Steps Per Day", 
                   y= "Average Sleep Per Day (Minutes)")+
         scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 
                                       400, 450, 500, 550, 600, 650, 700))


plot3A <- plot3 +  
                  scale_x_continuous(breaks = seq(0, 15000, by = 1000))+
                  theme(axis.text.x = element_text(size = 12), 
                        axis.text.y = element_text(size = 12),
                        axis.title.y = element_text(size = 17),
                        axis.title.x = element_text(size = 17),
                        plot.title = element_text(size = 20, face = "bold"))+
                  annotate('rect', xmin=8000, xmax=12000, 
                                   ymin=350, ymax=500, 
                                   alpha=.35, fill='#84ff10')+
                  annotate('rect', xmin=820, xmax=5000, 
                                   ymin=50, ymax=658, 
                                   alpha=.35, fill='#ff3810e9')
 print(plot3A)






weekday_steps_sleep <- daily_activity_sleep 

weekday_steps_sleep$week_day <- ordered(weekday_steps_sleep$week_day, levels = c("Monday", 
                                                       "Tuesday", "Wednesday", "Thursday", 
                                                       "Friday", "Saturday", "Sunday"))

weekday_steps_sleep <- weekday_steps_sleep %>%
                       group_by(week_day) %>%
                       summarise(daily_steps = mean(totalsteps), 
                                    daily_sleep = mean(totalminutesasleep))

mean_steps <- round(mean(weekday_steps_sleep$daily_steps))
mean_sleep <- round(mean(weekday_steps_sleep$daily_sleep))

View(weekday_steps_sleep)

plot4A <-  ggplot(weekday_steps_sleep) +
           geom_col(mapping = aes(week_day, daily_steps), fill = "#00ff99db")+
           coord_fixed(ratio = 0.00055)+
           theme_minimal()+
           labs(title = "Average Steps Taken On Each Day Of The Week", x= "", y= "Steps")+
           geom_hline(yintercept = mean_steps, color = "#3d8df7", size = 0.8) +
           theme(axis.text.x = element_text(size = 17.5), 
                 axis.text.y = element_text(size = 15),
                 axis.title.y = element_text(size = 20), 
                 plot.title = element_text(size = 22.5, face = "bold"))+
           scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 1000))+
           annotate("text", x = 1.2, y = mean_steps, 
                     label = "Average = 8557 steps", 
                     vjust = -0.5, hjust = -1.5, 
                     color = "#070724", size = 5.25)

print(plot4A)


plot4B <-  ggplot(weekday_steps_sleep) +
           geom_col(mapping = aes(week_day, daily_sleep), fill = "#57cdf8")+
           coord_fixed(ratio = 0.011)+
           theme_minimal()+
           theme(axis.text.x = element_text(size = 17.5), 
                 axis.text.y = element_text(size = 15),
                 axis.title.y = element_text(size = 18), 
                 plot.title = element_text(size = 22.5, face = "bold"))+
           labs(title = "Sleep per day of the week", x= "", y= "Sleep (in minutes)")+
           geom_hline(yintercept = mean_sleep, color = "#00a53a", size = 1)+
           scale_y_continuous(limits = c(0, 460), breaks = seq(0, 460, by = 25))+
           annotate("text", x = 1.2, y = mean_sleep, 
                    label = "Average = 420 minutes", 
                    vjust = -0.5, hjust = -1.75,
                    color = "#070724", size = 5.25)
print(plot4B)



steps_per_hour <- hourly_steps %>%
                  separate(date_time, into = c("date", "time"), sep= " ") %>%
                  mutate(date = ymd(date), time = ifelse(is.na(time), "00:00", 
                         substr(time, 1, 5)))

  stepsperhour <- steps_per_hour %>%
                  group_by(time) %>%
                  summarise(avg_steps = mean(steptotal))

View(stepsperhour)



plot5 <- ggplot(stepsperhour, aes(x= time, y= avg_steps, fill = avg_steps))+
         geom_col()+
         coord_fixed(ratio = 0.025)+
         theme_minimal()+
         labs(title = "Steps walked Each Hour of the Day", x= 'Time of day', y= "Steps")+
         theme(axis.text.x = element_text(size = 12), 
               axis.text.y = element_text(size = 12),
               axis.title.y = element_text(size = 17, face = "bold"),
               axis.title.x = element_text(size = 17, face = "bold"),
               plot.title = element_text(size = 20, face = "bold"))+
         labs(fill = "Average Steps")+
         scale_y_continuous(expand = c(0, 20), breaks = seq(0, 600, by = 50))+
         scale_x_discrete(guide = guide_axis(angle = 90))

print(plot5)
