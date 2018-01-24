# Libraries
library(ggplot2)
library(extrafont)
require(scales)
library(reshape2)
library(rts)
library(npphen)
#font_import() # This import all fonts from the local Windows font folder C:/...
#loadfonts()

#----------------------------------------------------------------------------------------

# Data extraction for a given pixel
table.path <- "~/PROJECTS/16b_trapananda_TS/table"
out.path <- "~/PROJECTS/16b_trapananda_TS/pdf_tif_out"
vi.path <- "~/PROJECTS/16b_trapananda_TS/03_MOD13Q1_EVI_clean"
vifl <- list.files(path=vi.path, pattern=glob2rx("*MOD*.tif"), full.names=T)

dates.table <- read.csv("~/PROJECTS/16b_trapananda_TS/table/MOD13Q1_400_dates.csv", sep = ",", header=TRUE)
vi.st <- stack(vifl)  # ERROR: arguments imply differing number of rows: 400, 0 // es porque no he cargado el paquete Raster
dates <- as.Date(dates.table$date, format='%d/%m/%Y')
vi.rts<-rts(vi.st, dates)

px<-cellFromXY(vi.rts,c(283124,4974102)) 

vi.ts<-extract(vi.rts,px)
setwd(out.path)
data<-as.data.frame(vi.ts)
data$dmy <- as.Date(dates, format='%d-%m-%Y')
data$evi <- as.numeric(data$V1/10000)

#----------------------------------------------------------------------------------------

# Plot

xlab=expression(Date)
ylab=expression(EVI)
theme_set(theme_bw(base_size=10))
theme_update(plot.title = element_text(hjust = 0.5))

TS_plot <- ggplot(data=data, aes(x=dmy, y=evi)) +
  geom_point(shape=16, size=3) +
  geom_line(size=0.4, linetype='solid') +
  xlab(NULL) +
  scale_x_date(labels=date_format("%Y"), breaks=date_breaks(width="1 year"), limits=c(as.Date('2001-1-1'), as.Date('2017-1-1'))) +
  ylim(0,0.6) +
  ylab(ylab) +
  #labs(title = "Serie Temporal Predio Guindo")+
  theme(legend.key = element_rect(colour = "white")) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(text=element_text(family="Arial", size=18)) +
  theme(axis.title = element_text(family="Arial", size=20, face="bold"))
TS_plot
#theme(legend.justification=c(0.96,0.91), legend.position=c(1,1))
