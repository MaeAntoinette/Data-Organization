setwd("F:/USGSStreamGauge/ImperialValley")
getwd()
load("F:/RStudio/Scripts/Observing_And_Comparing_USGS_StreamGage_Data_workenv.RData")


#Background: EEFlux ET is generated using remotely sensed data at moderate to low resolution. The combination of global 
#climate grids and Landsat 30m images are downscaled and resampled to generate 30m daily ETa. I use EEFlux ET data for 
#to quantify water consumption (ET) across Imperial Valley croplands, particularly to quantify and locate
#high water-use croplands that could be taken out of production for potential mitigation flows into the Salton Sea. 
#However, the reliability and validity of EEFlux ET has yet been assessed at the regional scale. 

#I've predownloaded and saved USGS stream gage data. However, you may download gage data directly in R using the  
#"dataRetrival" package. 

#Objective: I use a surface-water balance of annual canal flows through the Imperial Valley solving for a residual ET
#that will be compared to annual EEFlux volumetric ET. ET is solved residually as surface inflow less surface outflow 
#following methods proposed in Allen et al. (2005), Clemmens (2008), and Burt (1999) which all describe the Imperial Valley 
#as having a simplistic hydrologic structure, with tile drains intercepting most subsurface flows 
#and negligible groundwater flows. EEFlux ET data will be calibrated according to the observed, canal data. 

#Results: The following script will create figures and tables of observed canal data, estimated ET from EEFlux, and stats. 
#MAE, SD, CV, and RSME values are provided (labeled as: "ET.[whatever the stat is]"). 


#---------- Importing and Converting Data --------
#Identify your inputs.
Input1 = read.csv("Discharge_AAC_BlwPK_Inflow_Julian.csv", header = TRUE, sep = ",")
Input2 = read.csv("Discharge_AAC_Coachella_Inflow_Julian.csv", header = TRUE, sep = ",")
Input3 = read.csv("Discharge_NR_Calexico_Inflow_Julian.csv", header = TRUE, sep = ",")
#Identify your outputs.
Output1 = read.csv("Discharge_AlamoR_Outflow_Julian.csv", header = TRUE, sep = ",")
Output2 = read.csv("Discharge_NR_Outflow_Julian.csv", header = TRUE, sep = ",")

#Change to date format.
Input1$Date <- as.Date(Input1$Date, "%m/%d/%Y")
Input2$Date <- as.Date(Input2$Date, "%m/%d/%Y")
Input3$Date <- as.Date(Input3$Date, "%m/%d/%Y")
Output1$Date <- as.Date(Output1$Date, "%m/%d/%Y")
Output2$Date <- as.Date(Output2$Date, "%m/%d/%Y")

#Adjust your Julian date. 
Input1$JulianDay <- format(Input1$Date, "%j")
Input2$JulianDay <- format(Input2$Date, "%j")
Input3$JulianDay <- format(Input3$Date, "%j")
Output1$JulianDay <- format(Output1$Date, "%j")
Output2$JulianDay <- format(Output2$Date, "%j")

#IDENTIFY YOUR YEARS, MONTHS, and DAYS FOR YEARLY/MONTHLY ESTIMATION
#Input1 PK
yearPK = as.numeric(format(strptime(Input1[,3],format="%Y-%m-%d"),"%Y"))
monthPK = as.numeric(format(strptime(Input1[,3],format="%Y-%m-%d"),"%m"))
dayPK = as.numeric(format(strptime(Input1[,3],format="%Y-%m-%d"),"%d"))
#Input2 CC
yearCC = as.numeric(format(strptime(Input2[,3],format="%Y-%m-%d"),"%Y"))
monthCC = as.numeric(format(strptime(Input2[,3],format="%Y-%m-%d"),"%m"))
dayCC = as.numeric(format(strptime(Input2[,3],format="%Y-%m-%d"),"%d"))
#Input3 NR
yearNR = as.numeric(format(strptime(Input3[,3],format="%Y-%m-%d"),"%Y"))
monthNR = as.numeric(format(strptime(Input3[,3],format="%Y-%m-%d"),"%m"))
dayNR = as.numeric(format(strptime(Input3[,3],format="%Y-%m-%d"),"%d"))
#Outflow1 AR
yearARout = as.numeric(format(strptime(Output1[,3],format="%Y-%m-%d"),"%Y"))
monthARout = as.numeric(format(strptime(Output1[,3],format="%Y-%m-%d"),"%m"))
dayARout = as.numeric(format(strptime(Output1[,3],format="%Y-%m-%d"),"%d"))
#Outflow2 NR
yearNRout = as.numeric(format(strptime(Output2[,3],format="%Y-%m-%d"),"%Y"))
monthNRout = as.numeric(format(strptime(Output2[,3],format="%Y-%m-%d"),"%m"))
dayNRout = as.numeric(format(strptime(Output2[,3],format="%Y-%m-%d"),"%d"))

#Rename your Q_cfs by adding site name in front 
colnames(Input1)[colnames(Input1)=="Q_cfs"] <- "PK.Q_cfs"
colnames(Input2)[colnames(Input2)=="Q_cfs"] <- "CC.Q_cfs"
colnames(Input3)[colnames(Input3)=="Q_cfs"] <- "NR.Q_cfs"
colnames(Output1)[colnames(Output1)=="Q_cfs"] <- "ARout.Q_cfs"
colnames(Output2)[colnames(Output2)=="Q_cfs"] <- "NRout.Q_cfs"

#Coachella Canal has missing data. Clemmens (2008) observed the gage at PK is about 15% more than what reaches the CC. 
#Use your data to verify this average change. 

PK.CC.temp.change = merge(Input1,Input2,by="Date")
PK.CC.temp.change$PercentChange <- ((PK.CC.temp.change$PK.Q_cfs - PK.CC.temp.change$CC.Q_cfs)/PK.CC.temp.change$PK.Q_cfs)*100
PK.CC.temp.change$PercentReceived <- 100 - PK.CC.temp.change$PercentChange

drop.temp <- c("ID.x", "Control.x", "PK.Q_cfd", "PK.Q_cms", "PK.Q_cmd", "Type.y", "ID.y", "JulianDay.y", "Control.y", "CC.Q_cfd", "CC.Q_cms", "CC.Q_cmd")

PK.CC.Change = PK.CC.temp.change[,!(names(PK.CC.temp.change) %in% drop.temp)]
PK.CC.Change[,6] <-round(PK.CC.Change[,6],2)
PK.CC.Change[,7] <-round(PK.CC.Change[,7],2)
PK.CC.Change$Date <- as.Date(PK.CC.Change$Date, "%m/%d/%Y") 
 
mean(PK.CC.Change$PercentReceived, na.rm=TRUE)
median(PK.CC.Change$PercentReceived, na.rm=TRUE)
sd(PK.CC.Change$PercentReceived,na.rm=TRUE)
sd(PK.CC.Change$PercentReceived,na.rm=TRUE)/(mean(PK.CC.Change$PercentReceived, na.rm=TRUE))
#About 11%

#Fill the NAs in your Coachella data 
Input2 <- transform(Input2, CC.Q_cfs = ifelse(is.na(Input2$CC.Q_cfs), PK.CC.Change$PK.Q_cfs*0.1099, CC.Q_cfs))

#Convert to cubic feet day (....why did I do this?)
Input1$PK.Q_cfd <- Input1$PK.Q_cfs*86400
Input2$CC.Q_cfd <- Input2$CC.Q_cfs*86400
Input3$NR.Q_cfd <- Input3$NR.Q_cfs*86400
Output1$ARout.Q_cfd <- Output1$ARout.Q_cfs*86400
Output2$NRout.Q_cfd <- Output2$NRout.Q_cfs*86400

#Convert to CFS to CMS
Input1$PK.Q_cms <- Input1$PK.Q_cfs*0.0283168
Input2$CC.Q_cms <- Input2$CC.Q_cfs*0.0283168
Input3$NR.Q_cms <- Input3$NR.Q_cfs*0.0283168
Output1$ARout.Q_cms <- Output1$ARout.Q_cfs*0.0283168
Output2$NRout.Q_cms <- Output2$NRout.Q_cfs*0.0283168

#Convert to CMD
Input1$PK.Q_cmd <- Input1$PK.Q_cms*86400
Input2$CC.Q_cmd <- Input2$CC.Q_cms*86400
Input3$NR.Q_cmd <- Input3$NR.Q_cms*86400
Output1$ARout.Q_cmd <- Output1$ARout.Q_cms*86400
Output2$NRout.Q_cmd <- Output2$NRout.Q_cms*86400

#Convert to MCM per day.
Input1$PK.Q_mcmd <- Input1$PK.Q_cmd/1000000
Input2$CC.Q_mcmd <- Input2$CC.Q_cmd/1000000
Input3$NR.Q_mcmd <- Input3$NR.Q_cmd/1000000
Output1$ARout.Q_mcmd <- Output1$ARout.Q_cmd/1000000
Output2$NRout.Q_mcmd <- Output2$NRout.Q_cmd/1000000


#Save these working files. Set apart from your original stream gauge data
write.csv(Input1, "Discharge_AAC_PK_Inflow_Julian_Converts.csv")
write.csv(Input2, "Discharge_AAC_Coachella_Inflow_Julian_Converts.csv")
write.csv(Input3, "Discharge_NR_Calexico_Inflow_Julian_Converts.csv")
write.csv(Output1, "Discharge_AlamoR_Outflow_Julian_Converts.csv")
write.csv(Output2, "Discharge_NR_Outflow_Julian_Converts.csv")




#--------------- Aggregating Data ----------------
#SUMMING YOUR DAILY AVERAGE DISCHARGE to YEARLY Discharge
PK.ann.Q <- aggregate(as.numeric(Input1$PK.Q_mcmd),by=list(yearPK),FUN="sum") 
CC.ann.Q <- aggregate(as.numeric(Input2$CC.Q_mcmd),by=list(yearCC),FUN="sum")
NR.ann.Q <- aggregate(as.numeric(Input3$NR.Q_mcmd),by=list(yearNR),FUN="sum")
ARout.ann.Q <- aggregate(as.numeric(Output1$ARout.Q_mcmd),by=list(yearARout),FUN="sum")
NRout.ann.Q <- aggregate(as.numeric(Output2$NRout.Q_mcmd),by=list(yearNRout),FUN="sum")

#Rename your first column "Group.1" to Year
colnames(PK.ann.Q)[colnames(PK.ann.Q)=="Group.1"] <- "Year"
colnames(CC.ann.Q)[colnames(CC.ann.Q)=="Group.1"] <- "Year"
colnames(NR.ann.Q)[colnames(NR.ann.Q)=="Group.1"] <- "Year"
colnames(ARout.ann.Q)[colnames(ARout.ann.Q)=="Group.1"] <- "Year"
colnames(NRout.ann.Q)[colnames(NRout.ann.Q)=="Group.1"] <- "Year"

#Rename your annual Q estimates 
colnames(PK.ann.Q)[colnames(PK.ann.Q)=="x"] <- "PK.Q_Sum_mcmy"
colnames(CC.ann.Q)[colnames(CC.ann.Q)=="x"] <- "CC.Q_Sum_mcmy"
colnames(NR.ann.Q)[colnames(NR.ann.Q)=="x"] <- "NR.Q_Sum_mcmy"
colnames(ARout.ann.Q)[colnames(ARout.ann.Q)=="x"] <- "ARout.Q_Sum_mcmy"
colnames(NRout.ann.Q)[colnames(NRout.ann.Q)=="x"] <- "NRout.Q_Sum_mcmy"

#Merge all yearly discharge data
all.temp = merge(PK.ann.Q,CC.ann.Q,by="Year")
all.temp = merge(all.temp, NR.ann.Q,by="Year")
all.temp = merge(all.temp, ARout.ann.Q,by="Year")
all.temp = merge(all.temp, NRout.ann.Q,by="Year")

#Rename your working temp file
IV.ann.Q = all.temp
IV.ann.Q$Total.Inflow_mcmy <- ((IV.ann.Q$PK.Q_Sum_mcmy-IV.ann.Q$CC.Q_Sum_mcmy) + IV.ann.Q$NR.Q_Sum_mcmy)
IV.ann.Q$Total.Outflow_mcmy <- IV.ann.Q$ARout.Q_Sum_mcmy + IV.ann.Q$NRout.Q_Sum_mcmy
IV.ann.Q$ET.Q_Sum_mcmy <- IV.ann.Q$Total.Inflow_mcmy - IV.ann.Q$Total.Outflow_mcmy



# ------------ Validating your Data --------------
#Bring in your EEFlux water balance volum. ET maps. 
EEFlux.fold <- "F:/EEFlux/P39R37/"
EEFlux.WB.fold <- "/ET/WaterBalance/"

WB.dsn <- "F:\\GIS\\Study Area"
WB.fn <- "WaterBalanceBoundary"
WB.shp <- readOGR(WB.dsn,WB.fn)

EE.WB.2010 <- raster(list.files(paste0(EEFlux.fold,"2010",EEFlux.WB.fold),patt = ".img$",full.names = TRUE))
EE.WB.2011 <- raster(list.files(paste0(EEFlux.fold,"2011",EEFlux.WB.fold),patt = ".img$",full.names = TRUE))
EE.WB.2012 <- raster(list.files(paste0(EEFlux.fold,"2012",EEFlux.WB.fold),patt = ".img$",full.names = TRUE))
EE.WB.2013 <- raster(list.files(paste0(EEFlux.fold,"2013",EEFlux.WB.fold),patt = ".img$",full.names = TRUE))
EE.WB.2014 <- raster(list.files(paste0(EEFlux.fold,"2014",EEFlux.WB.fold),patt = ".img$",full.names = TRUE))
EE.WB.2015 <- raster(list.files(paste0(EEFlux.fold,"2015",EEFlux.WB.fold),patt = ".img$",full.names = TRUE))

EE.WB.stack <- stack(EE.WB.2010,EE.WB.2011,EE.WB.2012,EE.WB.2013,EE.WB.2014,EE.WB.2015)
plot(EE.WB.stack)

EE.WB.sums <- extract(EE.WB.stack,WB.shp,method = "simple",fun=sum,na.rm = TRUE)
EE.wb.sums.df <- setNames(data.frame(seq(2010,2015,by=1),t(EE.WB.sums)),c("Year","EEFlux_WB_ET_MCMY"))
rownames(EE.wb.sums.df) <- 1:nrow(EE.wb.sums.df)
EE.wb.sums.df$StreamGage_MCMY <- IV.ann.Q$ET.Q_Sum_mcmy[IV.ann.Q$Year == c("2010","2011","2012","2013","2014","2015")]

#Calculating Statistics
ET.MAE <- round(mean(abs(EE.wb.sums.df$StreamGage_MCMY - EE.wb.sums.df$EEFlux_WB_ET_MCMY)),digits = 0)
ET.MAPE <- paste0(round(mean((abs(EE.wb.sums.df$StreamGage_MCMY - EE.wb.sums.df$EEFlux_WB_ET_MCMY)))/mean(EE.wb.sums.df$StreamGage_MCMY) * 100,digits = 0),"%")
ET.SD <- round(sd(EE.wb.sums.df$StreamGage_MCMY - EE.wb.sums.df$EEFlux_WB_ET_MCMY),digits = 0)
ET.CV <- round(ET.SD/ET.MAE,digits = 2)
ET.RSME <- round(sqrt(mean((EE.wb.sums.df$StreamGage_MCMY - EE.wb.sums.df$EEFlux_WB_ET_MCMY)^2)),digits = 0)
ET.stats <- lm(EEFlux_WB_ET_MCMY ~ StreamGage_MCMY,EE.wb.sums.df)
ET.R2 <- round(summary(ET.stats)$r.squared,digits = 2)

#Create a table for your observed data 
ET.mattab.header <- c("Year","Stream Gage ET (MCM)","EEFlux ET (MCM)"," ", " ")
ET.mattab.years <- matrix(seq(2010,2015,by = 1))
ET.mattab.SGvol <- matrix(round(EE.wb.sums.df$StreamGage_MCMY,digits = 0))
ET.mattab.EEvol <- matrix(round(EE.wb.sums.df$EEFlux_WB_ET_MCMY,digits = 0))
ET.mattab.stats <- matrix(c("MAD","MAPD","RSMD","SD","CV","<i>R</i><sup><font size=1>2</font></sup>",ET.MAE,ET.MAPE,ET.RSME,ET.SD,ET.CV,ET.R2),nrow = 2,ncol = 6)

ET.mattab <- append(ET.mattab.SGvol,ET.mattab.EEvol)
ET.mattab <- append(ET.mattab, ET.mattab.stats)
ET.mattab <- matrix(ET.mattab, ncol = 6, byrow = TRUE, dimnames = NULL)

htmlTable(ET.mattab,
          align="c|c|c|c|c|c|",
          css.cell = "padding-top: .2em; padding-left: .5em; padding-bottom: .2em; padding-right: .2em;",
          header = paste(seq(2010,2015,by = 1)),
          rnames = c("Stream Gage ET (MCM)", "EEFlux ET (MCM)", " ", " "), #lists all the rows 
          rgroup = c("Validation", "Accuracy"),  #defines row sections
          n.rgroup = c(2,2),  #defines how many rows are within each define row section
          caption = "Annual EEFlux ET totals compared to annual stream gage ET")




#---------------- Calibrating EEFlux Data ---------
EE.wb.sums.df$CalibratedEEFlux <- EE.wb.sums.df$EEFlux_WB_ET_MCMY*0.75 #calibrating to 75% of the annual EEflux ET


#---------------- Plotting your data ---------------
#Plot inflow and ET with your EEFlux annual avg volumetric ET (clipped to the water balance boundary)
windows(width = 8, height = 10)

plot(IV.ann.Q$Year,IV.ann.Q$Total.Inflow_mcmy,type="l",col = "black",
     xlab = "Year", ylab = "Canal Discharge (MCM)", ylim = c(0,max(IV.ann.Q$Total.Inflow_mcmy)),
     xaxt = "n",yaxt = "n")
axis(side = 1, at = seq(2004,2015, by = 1),cex.axis = 0.75)
axis(side = 2, at = seq(0,4000,by = 500),cex.axis = 0.75)
lines(IV.ann.Q$Year,IV.ann.Q$ET.Q_Sum_mcmy,col = "grey40", lty = 2)
points(EE.wb.sums.df$Year,EE.wb.sums.df$EEFlux_WB_ET_MCMY,pch = 8,col = "black")
points(EE.wb.sums.df$Year,EE.wb.sums.df$CalibratedEEFlux,pch = 8, col = "grey50")

legend("bottomright",c("Canal Inflow","ET","EEFlux",expression('EEFlux'[adj])),
       lty = c(1,1,NA,NA), pch = c(NA,NA,8,8), col = c("black","grey40","black","grey50"))

#Create a multipanel plot of compared data. 
#Add a regression plot.



#FIND YOUR MMs 
#Find mm/year (area in m2 is 2001150000 according to digitized layer in Arc)
#area <- 2001150000
