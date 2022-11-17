
######################################
# REGRESSION ANALYSIS
######################################

# instructions: 
# 1. make four queries: traps, camps, offenders, and km travelled per day
# 2. trap, camp, and offender queries 
# should have 3 columns: "Date", "Type" (trap, camp, offender), and "Quantity"
# 3. effort should have two columns: "Date", and "km"

# loaf libraries
suppressMessages(require(ggplot2))
suppressMessages(require(tidyverse))
suppressMessages(require(ggpmisc))
suppressMessages(require(gtools))


# READ IN ILLEGAL ACTIVITY AND ASSOCIATED EFFORT DATA
args = commandArgs(trailingOnly=TRUE)
myquery1 = args[1]
myquery2 = args[2]
myquery3 = args[3]
myquery4 = args[4]

#The SMART query is in CSV format, so parse the
#query into a data frame
dat_traps <- read.csv(myquery1)
dat_traps$ID <- "Traps" # add ID column
names(dat_traps)[3] <- "n" # change quantity column to n
dat_camps <- read.csv(myquery2)	
dat_camps$ID <- "Camps" # add ID column
names(dat_camps)[3] <- "n" # change quantity column to n
dat_offenders <- read.csv(myquery3)	
dat_offenders$ID <- "Offenders" # add ID column
names(dat_camps)[3] <- "n" # change quantity column to n
effort <- read.csv(myquery4)	
names(effort) <- c("Waypoint.Date", "effort_km") # set new column names

# combine all illegal activity data
dat <- rbind(dat_traps, dat_camps, dat_offenders)
# combine number of illegal activities by date
dat <- dat%>%
  group_by(Waypoint.Date, ID)%>%
  summarise(n= sum(n, na.rm = TRUE))

# add the effort column by date
dat <- left_join(dat, effort, by="Waypoint.Date")

# remove ouliers, as they are undoubtedly mistakes.
# Apply the finction to one column
# first create the function
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

dat$density <- dat$n/dat$effort_km

# remove outliers
# this will remove instances where patrols 
# didn't turn tracks off or record tracks
dat2 <- dat %>%
  group_by(ID) %>%
  mutate(density = remove_outliers(density)) 


# now plot the density of illegal stuff over time
p1 <- ggplot(dat2, aes (x=Waypoint.Date, y=density))+
  geom_point(color="black", alpha=0.3, size=3)+
  geom_smooth(method="loess", color="grey50",fill="grey50", size=1)+
  scale_y_log10()+
  theme_bw(base_size = 15)+
  labs(x="Date",y="log10 Density (quantity per km)")+
  stat_poly_eq(aes(label = paste(..eq.label..)),
               formula = "y~x", parse = TRUE, rr.digits = 2,
               size=3, 
               label.x.npc = 0.9, label.y.npc = 0.99)+
  stat_poly_eq(aes(label = paste(..adj.rr.label..)),
               formula = "y~x", parse = TRUE, rr.digits = 2,
               size=3, 
               label.x.npc = 0.9, label.y.npc = 0.9)+
  stat_poly_eq(aes(label = paste(..p.value.label..)),
               formula = "y~x", parse = TRUE, rr.digits = 2,
               size=3, 
               label.x.npc = 0.9, label.y.npc = 0.78)+
  geom_smooth(method="lm", color="#CD001A", se=F, 
              size=1.3, linetype="dashed")+
  theme(panel.border = element_rect(size=1, fill="transparent"),
        strip.background = element_rect(fill="grey80"),
        strip.text = element_text(face=2),
        axis.text.x = element_text(size=8))+
  facet_wrap(~ID, scales = "free_y")

x11()
p1
sys.sleep(10)

setwd("Enter/File/folder/Path/Here")
ggsave("plot1.png")


