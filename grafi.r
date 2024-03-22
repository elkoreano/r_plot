# libraries to be used in the script 
library(readr)  # if you've chosen to use readr for reading CSV files
library(tidyr)  # for data transformations
library(ggplot2)  # for plotting
library(plotly)  # for interactive plots
library(dplyr)  # for data manipulation
library(scales) # to add the thousand separator
#check

## this script works

df<-read.csv("C:/input.csv",sep=";",dec = ",",header=TRUE)

# type is the name of the "new column" that is the result from -X..CRECIMIENTO.ANUAL.DIC.YTD:X..CRECIMIENTO.ANUAL.YTY.BH.MES-
df2 <- gather(df, type, total, FROM:TO, -Fecha) #Create long format
df2 <- na.omit(df2)
#df2 <- df2[!is.na(df2$total) & df2$total != 0, ]
colours <- setNames(c("orange", "blue", "limegreen"),
                    c("COLUMN USED", "COLUMN USED", "COLUMN USED"))
df2$Fecha <- as.Date(df2$Fecha, format = "%d/%m/%Y")


#vertical lines
verticales <- df2$Fecha[c(1, 13, 28, 41)]
vertical2 <- df2$Fecha[c(15)]

p<-ggplot(df2, aes(x = Fecha)) +            # Draw ggplot2 plot
  geom_line(aes(x=Fecha,y = total, col=type, group = type)) +
  scale_y_continuous(labels = comma) + # it adds the thousand separator
  scale_x_date(date_labels = "%b-%y", date_breaks  ="1 month")+ # IT format the date in the x axis
  geom_text(aes(y=total,label = comma(total), hjust=1.5),vjust = -2, size=3)+ # it moves the and stylize the number on the plot
  geom_point(aes(y=total, shape =type,colour= factor(type)), size=1) + # the "size" argument controls the size of the data marker (point)
  geom_vline(xintercept = as.numeric(verticales) , linetype = "dotted", color = "lightcoral") + #vertical line, not working as expected  *_* - **now working as expected ^.^
  geom_vline(xintercept = as.numeric(vertical2) , linetype = "dotted", color = "blue") +
  # scale_fill_manual(values = colours)+
  scale_color_manual(values=colours)+
  theme(axis.text.x = element_text(angle = -45, hjust = 1, vjust = 1, size=10),
        plot.title = element_text(color="red", size=14, face="bold.italic", hjust = 0.5),
        axis.title.x=element_blank(), axis.title.y=element_blank()) +
        #plot.margin = margin(t = 1, r = 3, b = 1, l = 3, unit = "cm")) +
  ggtitle("Title of the plot");

#print(p)

#ggplotly(p)

# floor() to correct the error of 10.000 in the plot

p <- plotly::ggplotly(p)%>%
plotly::style(textposition = "top center")

#showing plot
p