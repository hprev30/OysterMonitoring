#####packages-------
install.packages('readxl')
install.packages('dplyr')
install.packages('ggtext')
library(readxl)
library(ggplot2)
library(dplyr)
library(ggtext)
library(readr)

#####reading in data sets-----
reef = read_xlsx("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Analysis/2022-2024/Reefs.xlsx")
counts = read_xlsx("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Analysis/2022-2024/Counts.xlsx")
mus = read_xlsx("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Analysis/2022-2024/Mussels.xlsx")
oys = read_xlsx("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Analysis/2022-2024/Oysters.xlsx")
clams = read_xlsx("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Analysis/2022-2024/Clams.xlsx")
percents = read.csv("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Analysis/2022-2024/percent_means.csv")

reefm = read_excel("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Masters/GTM Oyster Monitoring Data Masters (2014-2024).xlsx",  
                  sheet = "Reefs") 

countsm = read_excel("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Masters/GTM Oyster Monitoring Data Masters (2014-2024).xlsx",  
                   sheet = "Counts") 

oshellhm = read_excel("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Masters/GTM Oyster Monitoring Data Masters (2014-2024).xlsx",  
                     sheet = "Oysters") 

mshellhm = read_excel("Z:/Research/OYSTERS/Oyster Monitoring/Oyster Monitoring Data/Oyster Data Masters/GTM Oyster Monitoring Data Masters (2014-2024).xlsx",  
                      sheet = "Mussels") 
#####renaming-----
clams <- within(clams, Region[Region == 'Butler Beach' & Season == 'W23'] <- 'North Butler Beach')
clams <- within(clams, Region[Region == 'Butler Beach' & Season == 'W24'] <- 'South Butler Beach')
counts <- within(counts, Region[Region == 'Butler Beach' & Season == 'W23'] <- 'North Butler Beach')
counts <- within(counts, Region[Region == 'Butler Beach' & Season == 'W24'] <- 'South Butler Beach')
mus <- within(mus, Region[Region == 'Butler Beach' & Season == 'W23'] <- 'North Butler Beach')
mus <- within(mus, Region[Region == 'Butler Beach' & Season == 'W24'] <- 'South Butler Beach')
oys <- within(oys, Region[Region == 'Butler Beach' & Season == 'W23'] <- 'North Butler Beach')
oys <- within(oys, Region[Region == 'Butler Beach' & Season == 'W24'] <- 'South Butler Beach')
reef <- within(reef, Region[Region == 'Butler Beach' & Season == 'W23'] <- 'North Butler Beach')
reef <- within(reef, Region[Region == 'Butler Beach' & Season == 'W24'] <- 'South Butler Beach')

colnames(counts)[8]  <- "Oysters"
colnames(counts)[9] <- "Barnacles"
colnames(counts)[10] <- "Mussels"
colnames(counts)[11] <- "Clams"
colnames(reef)[18] <- "Clusters"

#####ordering, Averages, scaling up counts to meter squared------
counts$Region = as.factor(counts$Region)
clams$ShellLength_mm = na.omit(clams$ShellLength_mm)
clams$ShellLength_mm = as.numeric(clams$ShellLength_mm)
mus$ShellHeight_mm = na.omit(mus$ShellHeight_mm)
mus$ShellHeight_mm = as.numeric(mus$ShellHeight_mm)
reef$Clusters = as.numeric(reef$Clusters)

counts$Region = factor(counts$Region, levels = c('Tolomato River', 'Guana River', 'St Augustine', 'Salt Run',
                                                  'North Butler Beach', 'South Butler Beach', 'Fort Matanzas', 'Pellicer Flats'))
oys$Region = factor(oys$Region, levels = c('Tolomato River', 'Guana River', 'St Augustine', 'Salt Run',
                                                 'North Butler Beach', 'South Butler Beach', 'Fort Matanzas', 'Pellicer Flats'))
mus$Region = factor(mus$Region, levels = c('Tolomato River', 'Guana River', 'St Augustine', 'Salt Run',
                                           'North Butler Beach', 'South Butler Beach', 'Fort Matanzas', 'Pellicer Flats'))
reef$Region = factor(reef$Region, levels = c('Tolomato River', 'Guana River', 'St Augustine', 'Salt Run',
                                           'North Butler Beach', 'South Butler Beach', 'Fort Matanzas', 'Pellicer Flats'))
clams$Region = factor(clams$Region, levels = c('Tolomato River', 'Guana River', 'St Augustine', 'Salt Run',
                                           'North Butler Beach', 'South Butler Beach', 'Fort Matanzas', 'Pellicer Flats'))

Rmeans_oys = counts %>% group_by(Region) %>%
  summarize(mean = mean(tcounts_o, na.rm = T))
Rmeans_mus = counts %>% group_by(Region) %>%
  summarize(mean = mean(tcounts_m, na.rm = T))
Rmeans_slope = reef %>% group_by(Region) %>%
  summarize(mean = mean(ReefSlope, na.rm = T))
Rmeans_height = reef %>% group_by(Region) %>%
  summarize(mean = mean(ReefHeight_m, na.rm = T))
Percent_means = reef %>% group_by(Region) %>% summarise(across(everything(), list(mean)))
#write.csv(Percent_means, "percent_means.csv")
Percent_means = Percent_means[,c(1, 21:25)]
Rmeans_clusters = reef %>% group_by(Region) %>%
  summarize(mean = mean(Clusters, na.rm = T))

counts$tcounts_o = counts$Oysters*16
counts$tcounts_c = counts$Clams*16
counts$tcounts_b = counts$Barnacles*16
counts$tcounts_m = counts$Mussels*16


#####plot colors-----
sitecolours <- c(
  `Tolomato River` = "#009E73",
  `Guana River` = "#CC79A7",
  `Salt Run` = "#F0E442",
  `St Augustine` = "#D55E00",
  `Fort Matanzas` = "#0072B2",
  `Pellicer Flats` = "#E69F00",
  `North Butler Beach` = "#56B4E9",
  `South Butler Beach` = "#999999"
)

cover_colors = c(
  "Box" = "green4",
  "Live" = "gold3",
  "Other" = "black",
  "Shell" = "slateblue2",
  "Substrate" = "red3"
)

#####counts jitter plots by region ------
oys_jit = counts %>% 
  ggplot(aes(x = Region, y = tcounts_o , color = Region)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = Rmeans_oys, aes(x = Region, y = mean, size = 4), color = 'black') +
  scale_color_manual(values = sitecolours) +
  ggrepel::geom_label_repel(data = Rmeans_oys, aes(x = Region, y = mean, 
                                                  label = round(mean, digits = 0),
                                                  family = "times new roman"), 
                                                  size = 4.5,
                                                  nudge_y = 2.5, nudge_x = 0.2,
                                                  color = "black") +
  theme(axis.text = element_text(color = "black"),axis.line = element_line(),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5), text = element_text(size = 16)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x = 'Region', y = bquote(Live~oyster~density~(oysters/m^2))) + 
  theme(panel.spacing = unit(0, 'lines')) +  ggtitle("Oyster Density by Region")


oys_jit

Mus_jit = counts %>% 
  ggplot(aes(x = Region, y = tcounts_m, color = Region)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = Rmeans_mus, aes(x = Region, y = mean, size = 4)) +
  scale_color_manual(values = sitecolours) +
  ggrepel::geom_label_repel(data = Rmeans_mus, aes(x = Region, y = mean, 
                                                   label = round(mean, digits = 2),
                                                   family = "times new roman"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme(axis.text = element_text(color = "black"),axis.line = element_line(),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x = 'Region', y = bquote(Mussel~Density~(per~m^2))) + 
  theme(panel.spacing = unit(0, 'lines')) +  ggtitle("Mussel Density by Region")
  
  
Mus_jit

clus_jit = reef %>% 
  ggplot(aes(x = Region, y = Clusters, color = Region)) + geom_violin(trim = F) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = Rmeans_clusters, aes(x = Region, y = mean, size = 4)) +
  geom_point(data = Rmeans_clusters, aes(x= Region, y = mean, size = 4, 
                                         shape = 1, color = 'black')) +
  scale_color_manual(values = sitecolours) +
  ggrepel::geom_label_repel(data = Rmeans_clusters, aes(x = Region, y = mean, 
                                                   label = round(mean, digits = 2),
                                                   family = "times new roman"),
                                                  size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme(axis.text = element_text(color = "black"),axis.line = element_line(),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x = 'Region', y = bquote(Cluster~Counts~(per~0.0625~m^2))) + 
  theme(panel.spacing = unit(0, 'lines')) +  ggtitle("Cluster Density by Region")

clus_jit

#####height histograms by region-------
hist_sh <- oys %>%
  ggplot(aes(x=ShellHeight_mm, fill = Region, color = Region)) +
  geom_histogram(alpha=0.5, position = 'identity') + 
  scale_fill_manual(values = sitecolours) + 
  scale_color_manual(values = sitecolours) +
  labs(x = "Shell Height (mm)", y = "Frequency") + facet_wrap(~Region) +
  theme(panel.background = element_blank(), axis.line=element_line(size=1), 
        plot.title = element_text(hjust = 0.5), legend.position = "none")+
  geom_vline(xintercept = 75, linetype = "dashed") + 
  ggtitle("Oyster Height Frequency by Region")

hist_sh

hist_clamH <- clams %>%
  ggplot(aes(x=ShellLength_mm, fill = Region, color = Region)) +
  geom_histogram(alpha=0.5, position = 'identity') + 
  scale_fill_manual(values = sitecolours) + 
  scale_color_manual(values = sitecolours) +
  labs(x = "Shell Length (mm)", y = "Frequency") + facet_wrap(~Region) +
  theme(panel.background = element_blank(), axis.line=element_line(size=1), 
        plot.title = element_text(hjust = 0.5), legend.position = "none")+
  #geom_vline(xintercept = 75, linetype = "dashed") + 
  ggtitle("Clam Length Frequency by Region")

hist_clamH

hist_MusH <- mus %>%
  ggplot(aes(x=ShellHeight_mm, fill = Region, color = Region)) +
  geom_histogram(alpha=0.5, position = 'identity') + 
  scale_fill_manual(values = sitecolours) + 
  scale_color_manual(values = sitecolours) +
  labs(x = "Shell Height (mm)", y = "Frequency") + facet_wrap(~Region) +
  theme(panel.background = element_blank(), axis.line=element_line(size=1), 
        plot.title = element_text(hjust = 0.5), legend.position = "none")+
  #geom_vline(xintercept = 75, linetype = "dashed") + 
  ggtitle("Mussel Shell Height Frequency by Region")

hist_MusH

#####stacked bar graph for percent cover by region------
Percent_bar = ggplot(percents, aes(fill=CoverType, y=Percent, x=Region)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text = element_text(color = "black"),
        axis.line = element_line(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = 'Region', y = 'Average Percent Cover', fill = "Cover Type") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +  
  scale_fill_manual(values = cover_colors)

Percent_bar

#####box and whiskers-----
region_counts_bw = ggplot(counts, aes(x=Region, y = Oysters, fill = Region)) + 
  geom_boxplot() +
  ylab(bquote(Oyster~Counts~(per~0.00625~m^2))) + ggtitle("Regional Oyster Counts") + 
  theme(panel.background = element_blank(),
        axis.line.x = element_line(color="black", linewidth =1),
        axis.line.y = element_line(color="black", linewidth =1),
        plot.title = element_text(face = 'bold')) + 
  scale_fill_manual(values = sitecolours) +  
  theme(axis.text = element_text(color = "black"),axis.line = element_line(),
                            legend.position = "none", panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5)) + scale_x_discrete(guide = guide_axis(n.dodge = 2))

region_counts_bw

region_MusCounts_bw = ggplot(counts, aes(x=Region, y = Mussels, fill = Region)) + 
  geom_boxplot() +
  ylab(bquote(Mussel~Counts~(per~0.00625~m^2))) + ggtitle("Regional Mussel Counts") + 
  theme(panel.background = element_blank(),
        axis.line.x = element_line(color="black", linewidth =1),
        axis.line.y = element_line(color="black", linewidth =1),
        plot.title = element_text(face = 'bold')) + 
  scale_fill_manual(values = sitecolours) +  
  theme(axis.text = element_text(color = "black"),axis.line = element_line(),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + scale_x_discrete(guide = guide_axis(n.dodge = 2))

region_MusCounts_bw

region_ClamCounts_bw = ggplot(counts, aes(x=Region, y = Clams, fill = Region)) + 
  geom_boxplot() +
  ylab(bquote(Clam~Counts~(per~0.00625~m^2))) + ggtitle("Regional Clam Counts") + 
  theme(panel.background = element_blank(),
        axis.line.x = element_line(color="black", linewidth =1),
        axis.line.y = element_line(color="black", linewidth =1),
        plot.title = element_text(face = 'bold')) + 
  scale_fill_manual(values = sitecolours) +  
  theme(axis.text = element_text(color = "black"),axis.line = element_line(),
        legend.position = "none", panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + scale_x_discrete(guide = guide_axis(n.dodge = 2))

region_ClamCounts_bw

#####Clusters-----

