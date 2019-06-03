library(plotly)

#Set a directory
setwd("C:/Users/T430/Dropbox/PhD2/MOB")


#Set the data
data <- read.csv(file = "Profiles MOB and environ all together.csv", header = TRUE, sep = ",", dec = ".")


#create font description for axes titles and tick fonts

f <- list(size = 14, color = "black")
f2 <- list(size = 12, color = "black")

#Subset lake Geai from data
geai <- subset(data, Lake == "Geai")

#create font description for axes titles and tick fonts
f <- list(size = 14, color = "black")
f2 <- list(size = 12, color = "black")

#Create the axes
ax <- list(title ="CH<sub>4</sub> concentration (&mu;M)",
           titlefont = f,
           tickfont = f2,
           rangemode = "tozero",
           zeroline = FALSE,
           showgrid = FALSE,
           autorange = FALSE,
           range = c(-1, 3.2),
           type = 'log',
           autotick = FALSE,
           tick0 = 0,
           dtick = 1,
           showline = TRUE,
           autotick = FALSE,
           ticks = "outside",
           ticklen = 5,
           tickwidth = 2)

ax2 <- list(overlaying = "x",
            anchor = "y",
            side = "top",
            title = "Dissolved Oxygen (&mu;M)",
            titlefont = f,
            showgrid = TRUE,
            zeroline = FALSE,
            showline = TRUE,
            range = c(0, 310),
            autotick = FALSE,
            ticks = "outside",
            tick0 = 0,
            dtick = 100,
            ticklen = 5,
            tickwidth = 2,
            tickfont = f2)

ax3 <- list(tickfont = f2,
            titlefont = f,
            overlaying = "x",
            anchor = "free",
            side = "top",
            position = 0.80,
            title = "Temperature (&deg;C)",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = TRUE,
            range = c(0, 30),
            autotick = FALSE,
            ticks = "outside",
            tick0 = 0,
            dtick = 5,
            ticklen = 5,
            tickwidth = 2)

ax4 <- list(tickfont = list(color = "black", size = 10),
            overlaying = "x",
            anchor = "free",
            side = "bottom",
            position = 0.20,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = TRUE,
            title = "MOB abundance (cells.mL<sup>-1</sup>)",
            titlefont = f,
            autorange = FALSE,
            range = c(2, 6),
            type = 'log',
            exponentformat = 'power',
            autotick = FALSE,
            tick0 = 0,
            dtick = 1,
            ticks = "outside",
            ticklen = 5,
            tickwidth = 2)

ay <- list(autorange = FALSE,
           range = c(7.3,-0.3),
           showline = TRUE,
           title = "Depth (m)",
           titlefont = f,
           tickfont = f,
           autotick = FALSE,
           ticks = "outside",
           tick0 = 0,
           dtick = 1,
           ticklen = 5,
           tickwidth = 2,
           domain = c(0.3, 0.7))

#Plot Geai data

stdevalpha = as.list(na.omit(geai[,15]))
stdevgamma = as.list(na.omit(geai[,17]))


geai_plot <- plot_ly(data = geai, height = 800, width = 400) %>%
  
  add_bars(x = ~Alpha.MOB.cells.mL.1.DAPISubset, 
           y = ~Depth,
           name = "Alpha-MOB",
           orientation = "h",
           color = I("dodgerblue"), alpha = 0.5,
           xaxis = "x4",
           error_x = list(type = data, array = stdevalpha, visible = TRUE, color = "black", thickness = 1, width = 2),
           width = 0.3)  %>% 
  
  add_bars(x = ~Gamma.MOB.cells.mL.1.DAPISubset, 
           y = ~Depth,
           name = "Gamma-MOB",
           orientation = "h",
           color = I("darkorange"), alpha = 0.5,
           xaxis = "x4",
           error_x = list(type = data, array = stdevgamma, visible = TRUE, color = "black", thickness = 1, width = 2),
           width = 0.3) %>%
  
  add_trace(x = ~finalCH4.umol.L.1.,
            y = ~Depth,
            name = "CH<sub>4</sub>",
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'black', width = 2),
            marker = list(color = 'black', size = 4),
            connectgaps = TRUE) %>%
  
  add_trace(x = ~O2.mg.L.1.*31.25, 
            y = ~Depth, name = "O<sub>2</sub>",
            type = 'scatter',
            mode = 'lines',
            line = list(color = 'black', width = 2, dash = 'dash'),
            xaxis = "x2")  %>%
  
  add_trace(x = ~Temp, 
            y = ~Depth, name = "Temperature",
            type = 'scatter',
            mode = 'lines',
            line = list(color = 'black', width = 2, dash = 'dot'),
            xaxis = "x3")  %>%
  
  layout(xaxis2 = ax2,
         xaxis3 = ax3,
         xaxis4 = ax4,
         xaxis  = ax,
         yaxis  = ay,
         barmode = 'group',
         bargap = 0.8,
         legend = list(x = 100, y = 0.5, font=list(color = 'black')))
   
#Print the plot
print(geai_plot)
