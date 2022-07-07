# LEW JUN LONG
# TP059638


## ===================================== Load Libraries ==================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(openair)
library(plotrix)
library(reshape2)
library(rgl)
library(corrplot)
library(hrbrthemes)


## ======================================= Import Data ===================================================
weather_data = read.csv( "C:\\Users\\MSI\\Desktop\\Assigment\\weather.csv",
                    header = TRUE, stringsAsFactors = TRUE)


## ==================================== Data Pre-processing ==============================================
options(max.print=10000)
# Create a new data frame (Date) and combine with Weather Data
Date = seq(as.Date("2020-01-01"), length = 366, by = "days")
weather_data = cbind(Date, weather_data)

# Mutate (add "Month" column)
weather_data = mutate(weather_data, Month = format(Date, "%m"))
weather_data = weather_data %>% relocate(Month, .after = Date)

# View the data in table format
View(weather_data)

# Check data type
str(weather_data)

# Correct data type
weather_data$Month <- factor(weather_data$Month)

# Summary for weather data
summary(weather_data) 


## ======================================== Question 1 ===================================================
# A: How to predict raining day?
# ========================================================================================================
# --------------------------------------- Data Exploration -----------------------------------------------
raining_day <- filter(weather_data,RainToday == "Yes")
nrow(raining_day)

raining_day [(raining_day$RainToday == "Yes") & (raining_day$RainTomorrow == "Yes"),1]

# ----------------------------------------- Analysis 1 ---------------------------------------------------
# Find the rain distribution by month.
Yes_No = data.frame(weather_data %>% group_by(Month) %>%  count(RainToday) %>% spread(RainToday,n))
Yes_No['No'] = NULL
names(Yes_No)[2] <- "Num_RainingDay"
Yes_No
Total_Yes = 0
for (i in Yes_No$Num_RainingDay){
  Total_Yes = Total_Yes + i
}
Yes_No=cbind(Yes_No, Percentage = c(round ((Yes_No$Num_RainingDay / Total_Yes) * 100, digits = 2)))

pie(Yes_No$Percentage, paste0(Yes_No$Percentage, "%"), radius = 1,
    main ="Rain Distribution(%)",
    col = c("#3e8f4f", "#649a4f", "#86a453", "#a6ad5b", "#c4b667", "#e1bf76",
            "#fcc889", "#fdb680", "#fca47b", "#f8917a", "#f37f7c", "#ea6e80"),
    clockwise = TRUE)
legend("right", legend = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
       fill = c("#3e8f4f", "#649a4f", "#86a453", "#a6ad5b", "#c4b667", "#e1bf76",
                "#fcc889", "#fdb680", "#fca47b", "#f8917a", "#f37f7c", "#ea6e80"))

# ----------------------------------------- Analysis 2 ---------------------------------------------------
# Find the total rainfall by month.
Total_rainfall = aggregate(Rainfall ~ Month, weather_data, sum)
Total_rainfall$Month <- as.integer(Total_rainfall$Month)

ggplot(Total_rainfall, aes(x = Month,y = Rainfall))+
  geom_line(color = "grey")+
  geom_point(shape = 21, color = "black",fill = "#69b3a2", size = 6)+
  ggtitle("Total Rainfall(mm) in 2020") +
  scale_x_continuous(breaks = seq(0,12,1),labels = function(x) month.abb[x])

# ----------------------------------------- Analysis 3 ---------------------------------------------------
# Compare the status of 9am cloud during raining day with non-raining day.
Cloud_Data <- data.frame(weather_data %>% group_by(Cloud9am) %>% count(RainToday))
head(Cloud_Data,10)
colnames(Cloud_Data)[3] <- "Frequency"

ggplot(Cloud_Data, aes(x=Cloud9am, y=Frequency, fill=RainToday)) +
  geom_bar(stat='identity', alpha = 0.5, position='dodge') +
  geom_text(aes(label = Frequency), 
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3) +
  xlab("Cloud3pm(oktas)") +
  scale_x_continuous(breaks = seq(0,8,1)) +
  scale_y_continuous(breaks = seq(0,100,20))

# ----------------------------------------- Analysis 4 ---------------------------------------------------
# Find the frequency of raining according to temperature at 3pm.
Rain_Temp <- filter(weather_data, RainToday == "Yes") 
nrow(Rain_Temp)
Rain_Temp
ggplot(Rain_Temp, aes(x = Temp3pm)) + 
  geom_histogram(color="orange", aes(fill = ..count..)) +
  ggtitle("Temperature(°C) at 3pm during Raining Day") +
  scale_fill_gradient("Count", low = "green", high = "red") +
  scale_x_continuous(breaks = seq(5,40,2)) 

# ----------------------------------------- Analysis 5 ---------------------------------------------------
# Find the relationship between humidity and RainToday.
# Reference: https://statisticsglobe.com/draw-multiple-boxplots-in-one-graph-in-r
# library(reshape2)
humidity_data <- subset(weather_data, select = c(Humidity9am, Humidity3pm, RainToday))
humidity_data = melt(humidity_data)

ggplot(humidity_data, aes(x = variable, y = value,color=RainToday)) +
  geom_boxplot(alpha=0.3) +
  facet_wrap(~RainToday) +
  xlab("Humidity") +
  ylab("Value(%)") + 
  scale_y_continuous(breaks = seq(0,100,20)) 
  
# ----------------------------------------- Analysis 6 ---------------------------------------------------
# Find the relationship between humidity at 9am and evaporation based on RainToday.
ggplot(weather_data, aes(Evaporation,Humidity9am)) +
  geom_point(aes(color=RainToday)) +
  ggtitle("HUMIDITY9AM(%) VS EVAPORATION(mm) based on RainToday") +
  stat_smooth(method=lm) #https://www.rdocumentation.org/packages/ggplot2/versions/0.9.0/topics/stat_smooth
          
# ----------------------------------------- Analysis 7 ---------------------------------------------------
# Find the relationship between sunshine and evaporation.
Sun_Eva <- subset(weather_data, select= c("Sunshine","Evaporation"))
summary(Sun_Eva,maxsum = 6)
Sun_Eva = na.omit(Sun_Eva)

ggplot(Sun_Eva, aes(x=Sunshine, y=Evaporation)) + 
  geom_hline(yintercept = median(Sun_Eva$Evaporation), size= 1.5, color=("red")) +
  geom_vline(xintercept = median(Sun_Eva$Sunshine), size= 1.5, color=("yellow")) +
  geom_point(aes(color=Sunshine)) +
  geom_smooth(formula = y ~ x, method = 'loess', color = "black", size = 1, se = FALSE) +
  xlab("Sunshine(hrs/day)") + ylab("Evaporation(mm)") +
  scale_x_continuous(breaks = seq(0,15,2)) +
  scale_y_continuous(breaks = seq(0,15,2))

# ----------------------------------------- Analysis 8 ---------------------------------------------------
# Find the relationship between sunshine and cloud at 3pm.
weather_data %>% group_by(Cloud3pm) %>% na.omit(Sunshine) %>%
  summarise(Total = n(), SunshineToCloud3pm =sum (Sunshine) / Total) %>%
  ggplot(aes(x = Cloud3pm, y = SunshineToCloud3pm)) + 
  geom_point(color="violetred") + 
  geom_line(color="blue") + xlab("Cloud3pm(oktas)") +
  geom_text(aes(label = round(SunshineToCloud3pm,2)),hjust=0, vjust=0)

# ----------------------------------------- Analysis 9 ---------------------------------------------------
# Status of the amount of rainfall for the whole year.
# Reference: https://rstudio-pubs-static.s3.amazonaws.com/116317_e6922e81e72e4e3f83995485ce686c14.html#/6
# Take the data from data exploration (raining_day)
# library(plotrix)
Up_Line <- mean(raining_day$Rainfall) * 1.1
Down_Line <- mean(raining_day$Rainfall) * 0.9
raining_day <- mutate(raining_day, RainStatus = ifelse(Rainfall > Up_Line, "High", 
                                                       ifelse(Rainfall < Down_Line,"Low","Moderate")))
names(raining_day)
value <- c()
value[1] <- nrow(filter(raining_day, RainStatus == "High"))
value[2] <- nrow(filter(raining_day, RainStatus == "Moderate"))
value[3] <- nrow(filter(raining_day, RainStatus == "Low"))
name <- c("High","Moderate","Low")
pie3D(value, radius = 0.9, labels = name, 
      explode=0.1, theta = 0.8, labelcex = 1.2,
      main = "Amount of rainfall in 2020",
      col=c("brown","#ddaa00","pink"))

# ----------------------------------------- Analysis 10 ---------------------------------------------------
# Mean of weather data group by months.
# Take data from analysis 1 and 2
# Conclusion of question 1
data_mean <- subset(weather_data, select=c(Month, Cloud9am, Cloud3pm, Temp3pm, Humidity9am, 
                                           Humidity3pm, Evaporation, Sunshine))
data_mean <- na.omit(data_mean)

data_mean %>% melt(id = c("Month")) %>%
  dcast(Month ~ variable, fun.aggregate = mean) %>%
  format(.,digits = 2) %>%
  cbind(., Rainfall = Total_rainfall$Rainfall, Num_RainingDay = Yes_No$Num_RainingDay)


## ======================================== Question 2 ====================================================
# B: When is the best time for outdoor activities?
# =========================================================================================================
# ----------------------------------------- Analysis 11 ---------------------------------------------------
# Find the daily temperature average using Minimum Temperature and Maximum Temperature.
average_temp = (weather_data$MinTemp + weather_data$MaxTemp) / 2

ggplot(weather_data, aes(x = Date, y = average_temp)) +
  geom_line(alpha = 0.3) +
  geom_hline(yintercept = c(max(average_temp),min(average_temp)), size= 1.5, color=("red")) +
  stat_smooth(formula=y ~ x, se=FALSE, color="black", method = 'loess') +
    ggtitle("Average Temperature(°C) Recorded in USA") +
  xlab("Month") + ylab("Average Temperature") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(breaks = seq(0,28,4)) + expand_limits(y = 0)

# ----------------------------------------- Analysis 12 --------------------------------------------------
# Compare the number of hot days and cool days based on Temperature at 3pm.
Hot_Cool = select(weather_data, Temp3pm) %>% 
  mutate(TempStatus = ifelse(Temp3pm > 25, "Hot", "Cool")) %>%
  count(TempStatus=="Hot")
colnames(Hot_Cool) = c("TempStatus","Frequency")
Hot_Cool[1,1] = "Cool"
Hot_Cool[2,1] = "Hot"

Hot_Cool$fraction <- Hot_Cool$Frequency / sum(Hot_Cool$Frequency)
Hot_Cool$ymax <- cumsum(Hot_Cool$fraction)
Hot_Cool$ymin <- c(0, head(Hot_Cool$ymax, n=-1))
Hot_Cool$labelPosition <- (Hot_Cool$ymax + Hot_Cool$ymin) / 2
Hot_Cool$label <- paste0(Hot_Cool$TempStatus, "\n value: ", Hot_Cool$Frequency)

ggplot(Hot_Cool, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=TempStatus)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=5) +
  ggtitle("Number of hot days and cool days") +
  scale_fill_brewer(palette=1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),legend.position = "none")

# ----------------------------------------- Analysis 13 --------------------------------------------------
# Find the median and mean of temperature at 9am according to RainToday categorized by cloud at 9am
ggplot(weather_data, aes(x=RainToday, y=Temp9am, fill=RainToday)) + 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  facet_wrap(~Cloud9am, scale="free") +
  scale_fill_brewer(palette="RdBu")

# ----------------------------------------- Analysis 14 --------------------------------------------------
# Find the relationship among Temp9am, Temp3pm, average_temp
# Reuse the average_temp from Analysis 11
# Reference:https://www.r-graph-gallery.com/3d_scatter_plot.html
# library(rgl)
Temp_Data <- subset(weather_data, select = c(Temp9am,Temp3pm,RainToday))
mycolors <- c('royalblue1', 'darkcyan')
Temp_Data$color <- mycolors[ as.numeric(Temp_Data$RainToday) ]

plot3d( 
  x=Temp_Data$Temp9am, y=Temp_Data$Temp3pm, z=average_temp, 
  col = Temp_Data$color, 
  type = 's', 
  radius = 1,
  xlab="Temp9am", ylab="Temp3pm", zlab="Avg Temp")

bgplot3d({
  plot.new()
  title(main = '3D Diagram for Temperature', line = 3)
})


## ======================================== Question 3 ====================================================
# C: How to avoid storm in the U.S.?
# =========================================================================================================
# ----------------------------------------- Analysis 15 --------------------------------------------------
# Find the status of wind for the whole year.
# library(openair)
wind_data <- subset(weather_data, select = c(WindGustDir, WindGustSpeed, WindDir9am,
                                             WindSpeed9am, WindDir3pm, WindSpeed3pm))
class(wind_data$WindGustDir)
dir <- setNames( seq(0, 337.5 , by = 22.5),
                 c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                   "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))
wind_data$WindGustDir = dir[as.character(wind_data$WindGustDir)]
wind_data$WindDir9am = dir[as.character(wind_data$WindDir9am)]
wind_data$WindDir3pm = dir[as.character(wind_data$WindDir3pm)]
class(wind_data$WindGustDir)

wind_analysis <- function(WindSpeed, WindDir){
  return(windRose(wind_data, WindSpeed,WindDir,
                  breaks = c(0,5,10,15,20,25,30,35,40,100),
                  angle = 22.5 ,
                  auto.text = FALSE,
                  paddle = FALSE,
                  annotate = FALSE,
                  grid.line = 5,
                  key = list(labels = c("0 - 5",
                                        "5 - 10",
                                        "10 - 15",
                                        "15 - 20",
                                        "20 - 25",
                                        "25 - 30",
                                        "30 - 35",
                                        "35 - 40",
                                        ">40")),
                  key.footer = "WSP (km/h)",
                  key.position = "right",
                  par.settings = list(axis.line = list(col = "lightgray")),
                  col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a"))) 
}
wind_analysis("WindGustSpeed", "WindGustDir")
wind_analysis("WindSpeed9am", "WindDir9am")
wind_analysis("WindSpeed3pm", "WindDir3pm")


# ----------------------------------------- Analysis 16 --------------------------------------------------
# Find the relationship of wind direction and wind speed at 3pm.
wind_data1 <- subset(weather_data, select = c(WindDir3pm, WindSpeed3pm))
summary(wind_data1)
wind_data1 <- na.omit(wind_data1) 

ggplot(wind_data1,aes(x=WindDir3pm,y= WindSpeed3pm)) +
  geom_violin(alpha=0.5, color="gray") +
  geom_jitter(alpha=0.5, aes(color=WindDir3pm), position = position_jitter(width = 0.1)) +
  coord_flip()

## ========================================== Other =======================================================
# ----------------------------------------- Analysis 17 --------------------------------------------------
# Correlation relationship among the weather data
# library(corrplot)
new_data <- select(weather_data,-c("Date","Month","WindGustDir","WindDir9am",
                                   "WindDir3pm","RainToday","RainTomorrow"))
summary(new_data)
new_data <- na.omit(new_data)

#calculate the nonsymmetric set difference of subsets of a probability space.
numeric_data <- setdiff(numeric_data, "RainTomorrow")
numeric_data_mat <- as.matrix(new_data[, numeric_data, drop=FALSE])
numeric_data_cor <- cor(numeric_data_mat)
corrplot(numeric_data_cor)

# ----------------------------------------- Analysis 18 --------------------------------------------------
# Find the density of each weather data by entering the choice
#library(hrbrthemes)
op1 <- ggplot(weather_data, aes(Sunshine, group=RainTomorrow, fill=RainTomorrow)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
op2 <- ggplot(weather_data, aes(Temp9am, group=RainTomorrow, fill=RainTomorrow)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
op3 <- ggplot(weather_data, aes(Cloud9am, group=RainTomorrow, fill=RainTomorrow)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
op4 <- ggplot(weather_data, aes(Pressure9am, group=RainTomorrow, fill=RainTomorrow)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
op5 <- ggplot(weather_data, aes(Humidity9am, group=RainTomorrow, fill=RainTomorrow)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

input <- function(){
  message("Density:\n(A)Sunshine\n(B)Temp9am\n(C)Cloud9am\n(D)Pressure9am\n(E)Humidity9am")
  option = (readline(prompt = "Option: "))
  result = switch(option,
                  A = op1,
                  B = op2,
                  C = op3,
                  D = op4,
                  E = op5,
                  "Error")
  if(result!="Error"){
    print(result)
  }else{
    print("Invalid Option")
  }
}
input()

# ----------------------------------------- Analysis 19 --------------------------------------------------
# Write new CSV files into new folder
dir.create("C:\\Users\\MSI\\Desktop\\Assigment\\Extra File")
# Data from Analysis 1
Total_rainfall$Month = NULL
Rain_Data <- cbind(Yes_No, Total_rainfall)
write.csv(Rain_Data,"C:\\Users\\MSI\\Desktop\\Assigment\\Extra File\\Raining Data by Month.csv")
# Data from Analysis 3
write.csv(Cloud_Data,"C:\\Users\\MSI\\Desktop\\Assigment\\Extra File\\Cloud Data(9am).csv")
# Data from Analysis 5
write.csv(humidity_data,"C:\\Users\\MSI\\Desktop\\Assigment\\Extra File\\Humidity Data.csv")
# Data from Analysis 10
write.csv(data_mean,"C:\\Users\\MSI\\Desktop\\Assigment\\Extra File\\Mean data.csv")
# Data from Analysis 12
write.csv(Hot_Cool,"C:\\Users\\MSI\\Desktop\\Assigment\\Extra File\\Temperature Data(3pm).csv")
# Data from Analysis 17
write.csv(numeric_data_cor,"C:\\Users\\MSI\\Desktop\\Assigment\\Extra File\\Correlation Relationship.csv")

## ====================================== Extra Feature ==============================================
# 1. Bar Diagram with Two Bars (3)
# 2. Melt Command (5,10)
# 3. Donut Chart (12)
# 4. 3D Chart with RGL (14)
# 5. Wind Rose (15)
# 6. Correlation among Weather Data (17)
# 7. Violin Chart (16)
# 8. Density graph(18)
# 9. geom_hline & geom_vline (7)
