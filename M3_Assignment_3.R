r = getOption("repos")
r["CRAN"]="http://cran.us.r-project.org"
options(repos=r)

#1.Print your name at the top of the script.Import libraries including: FSA, FSAdata, magrittr, dplyr, tidyr plyr and tidyverse

Name <- ("Poonam Dighe")
Name

install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")

library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("tidyr")
library("plyr")
library("tidyverse")

#2. Import the inchBio.csv and name the table <bio>
bio <- read.csv("C:\\Users\\poonam\\Desktop\\inchBio.csv") 
View(bio)

#3. Display the head, tail and structure of <bio>
head(bio,5)
tail(bio,5)
headtail(bio,5)
str(bio)

#4.Create an object, <counts>, that counts and lists all the species records
counts <- bio$species
table(counts)

#5.Display just the 8 levels (names) of the species

unique(bio$species)

#6.Create a <tmp> object that displays the different species and the number of record of each species in the dataset. 
#Include this information in your report.

tmp <- table(bio$species)
tmp

#7.Create a subset, <tmp2>, of just the species variable and display the first five records.

tmp2 <- subset(bio,select = species)
head(tmp2,5)

#8.Create a table, <w>, of the species variable. Display the class of w.

w <- table(bio$species)
w

class(w) 


#9.Convert <w> to a data frame named <t> and display the results.

t<- as.data.frame(w)  
t

class(t)


#10.Extract and display the frequency values from the <t> data frame

t$Freq

#11.Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <bio>

cSpec <- table(bio$species) 
cSpec
class(cSpec) 

#12.Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class.
cSpecPct <- (table(bio$species)*100)/length(bio$species)
cSpecPct         #display species & their percentage
class(cSpecPct)

#13.Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame
u <- data.frame(cSpecPct )  #converting cSpectPct to a data frame
class(u)

#14.Create a barplot of <cSpec> with the following: titled Fish Count with the following specifications:
#Title: Fish Count
#Y axis is labeled "COUNTS"
#Color the bars Light Green
#Rotate Y axis to be horizontal
#Set the X axis font magnification to 60% of nominal

barplot(cSpec,
        main="Fish Count", 
        xlab="COUNTS",
        col="light green", 
        las = 1 , horiz = TRUE, 
        cex.axis=0.60, 
        cex.names = 0.45,
        cex.lab = 0.60, 
        cex.main = 0.60,
        xlim = c(0,250))

#15.Create a barplot of <cSpecPct>, with the following specifications:
#Y axis limits of 0 to 4
#Y axis label color of Light Blue
#Title of "Fish Relative Frequency"


barplot(cSpecPct/10, 
        ylim=c(0,4), 
        las=2, 
        col= "Light Blue", 
        main= "Fish Relative Frequency",
        cex.axis = 0.8,
        cex.names = 0.55)

#16.Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save the rearranged data frame as the object <d>

d <- u[order(u$Freq,decreasing=TRUE),]
d

#17.Rename the <d> columns Var 1 to Species, and Freq to RelFreq

colnames(d) <- c("Species","RelFreq")
d

#18.Add new variables to <d> and call them cumfreq, counts, and cumcounts.

t<- arrange(t,desc(t$Freq))
d<- mutate(d, cumFreq = cumsum(RelFreq), counts = t$Freq, cumcounts = cumsum(t$Freq))
d

#19.Create a parameter variable <def_par> to store parameter variables.

def_par <- as.data.frame(d)
def_par

#20. Create a barplot, <pc>, with the following specifications:
#d$counts of width 1, spacing of .15
#no boarder
#Axes: F
#Yaxis limit 0,3.05*max
#d$counts na.rm is true
#y label is Cummulative Counts
#scale x axis to 70%
#names.arg: d$Species
#Title of the barplot is "Species Pareto"
#las: 2)
#25.Display the finished Species Pareto Plot (without the star watermarks).Have your last name on the plot.
subtitle="DIGHE"
pc <- barplot(d$counts, 
              width = 1,
              space = 0.15, 
              border = NA, 
              axes = F,
              ylim = c(0,3.05*max(d$counts, na.rm = TRUE)), 
              ylab = "Cummulative Counts", 
              cex.axis = 0.7, 
              names.arg = d$Species,
              main = "Species Pareto", 
              las=2,
              cex.names = 0.55)
mtext(side=3, line=0, at=-0.17, adj= -4, cex=1, subtitle)
par(mar = c(0.1, 4, 4.5, 3))
#21.Add a cumulative counts line to the <pc> plot with the following:
#Spec line type is b
#Scale plotting text at 70%
#Data values are solid circles with color cyan4

lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")

#22.Place a grey box around the pareto plot.
box(col = "grey62") 

#23.Add a left side axis with the following specifications
#Horizontal values at tick marks at cumcounts on side 2
#Tickmark color of grey62
#Color of axis is grey62
#Axis scaled to 80% of normal

axis(side = 2, 
     at = c(0, d$cumcounts),
     las = 1,
     col.axis = "grey62", 
     col = "grey62", 
     cex.axis = 0.8)


#24.Add axis details on right side of box with the specifications:
#Spec: Side 4
#Tickmarks at cumcounts with labels from 0 to cumfreq with %,
#Axis color of cyan5 and label color of cyan4
#Axis font scaled to 80% of nominal

axis(side = 4, 
     at = c(0, d$cumcounts), 
     labels = paste(c(0, round(d$cumFreq,digit=1)),sep=" "),
     las = 1, 
     col.axis = "cyan4", 
     col = "cyan4", 
     cex.axis = 0.8)









