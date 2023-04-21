
# run_richards_analysis.R
#
#  Description:
#   This R script will plot DayCent results from the Richard's farm simulations
#   against measured data.  DayCent results are extracted from .lis files 
#   and harvest.csv files.  The observed data is stored in the Data directory,
#   which is a subdirectory of the working directory.  
#
#   If the order of variables in the .lis file is changed (e.g. by updating 
#   outvars.txt), then you will need to edit the columns specified in function 
#   ReadDataLis.
#
#   If you change the crop types for corn or soybeans in the schedule file,
#   you will need to update the crpval specified in many of the plotting
#   functions.  Currently the script searches for 'C620' and 'SYBN100' in 
#   the crpval columns of the .lis and harvest.csv files.
#
#   Measured data includes total soil organic C, corn and soybean grain yields,
#   above-ground live biomass, root biomass, and crop residue left in the field.
#
###############################################################################

# Uninitialize the variables in memory before rerunning the R script
rm(list=ls())

siteName <<- "richards"
# Path to files
modelPath = getwd()

#-------------------------------------------------------------------------------------------------
# Function ReadDataLis
# * Read the crpval, aglivc, bglivcj, bglivcm, somtc, somsc, som1c(2), som2c(2), and som3c 
#   columns from a set of .lis files and store into crpvalLisData, aglivcData, bglivcData, 
#   somtcData, somscData, som1c2Data, som2c2Data, and somc3Data.
# * This function assumes that the .lis files for all mgmtOptions have the 
#   same number of lines. 
# * In the read.table call, set the number of lines to skip so the function reads   
#   past the values generated from the equilibrium simulation.
# * If the outvars.txt file is changed, the column numbers below might change also!

ReadDataLis = function ()
{
  
    # Columns to extract from .lis file. To read in a column there must be an array declared below. 
    # Not all columns listed below may actually be read. 

    timeCol <- 1      # Column in .lis file that contains simulation time 
    crpvalCol <- 2    # Column in .lis file that contains crpval    (crop value abbreviation)

    aglivcCol <- 3    # Column in .lis file that contains aglivc    (live above-ground shoots for crops, gC/m2)
    bglivcjCol <- 4   # Column in .lis file that contains bglivcj   (live juvenile fine roots for crops, gC/m2)
    bglivcmCol <- 5   # Column in .lis file that contains bglivcm   (live mature fine roots for crops, gC/m2)
    stdedcCol <- 6    # Column in .lis file that contains stdedc    (standing dead C, gC/m2)

    tnetmnCol <- 22   # Column in .lis file that contains tnetmn(1) (accumulator for net N mineralization, gN/m2/yr)
    tminrlCol <- 23   # Column in .lis file that contains tminrl(1) (total mineral N, gN/m2)

    metabc1Col <- 24  # Column in .lis file that contains metabc(1) (surface metabolic litter C, gC/m2)
    metabc2Col <- 25  # Column in .lis file that contains metabc(2) (soil metabolic litter C, gC/m2)
    strucc1Col <- 26  # Column in .lis file that contains strucc(1) (surface structural litter C, gC/m2)
    strucc2Col <- 27  # Column in .lis file that contains strucc(2) (soil structural litter C, gC/m2)

    som1c2Col <- 29   # Column in .lis file that contains som1c(2)  (active soil organic matter C, gC/m2)
    som2c2Col <- 31   # Column in .lis file that contains som2c(2)  (slow soil organic matter C, gC/m2)
    som3cCol <- 32    # Column in .lis file that contains som3c)    (passive soil organic matter C, gC/m2)
    somscCol <- 33    # Column in .lis file that contains somsc     (soil organic matter C, gC/m2)
    somtcCol <- 34    # Column in .lis file that contains somtc     (soil organic matter C + below-ground litter C, gC/m2)

    somsnCol <- 45    # Column in .lis file that contains somse(1)  (soil organic N, gN/m2)
    somtnCol <- 46    # Column in .lis file that contains somte(1)  (soil organic N + below-ground litter N, gN/m2)

    strm1Col <- 48    # Column in .lis file that contains strmac(1) (accumulator for streamflow, cm/yr)
    strm2Col <- 49    # Column in .lis file that contains strmac(2) (accumulator for NO3 leaching, gN/m2/yr)

    annetCol <- 54    # Column in .lis file that contains annet     (accumulator for actual evapotranspiration, cm/yr)
    petanCol <- 55    # Column in .lis file that contains petann    (accumulator for potential evapotranspiration, cm/yr)


    doAlloc <- 1  # Set to 0 after array somscData has been allocated
    colCnt <- 1   # Column in somscData to store somsc values

    for (mgmt in mgmtOptions)
    {
        sch = paste(siteName, mgmt, sep="_")
        schext = paste(sch, ".sch", sep="")
    
        somName <- paste(siteName, mgmt, sep="_")
        somName <- paste(somName, ".lis", sep="")
        lisFileName <<- paste(modelPath,somName,sep="/")

        colCnt <- colCnt + 1
        lisFileName
        if (file.exists(lisFileName))
        {        
            # sep="" refers to any length of white space as being the delimiter.  Don't use " ".
            dataLis <<- read.table(file=lisFileName,skip=46,header=FALSE,sep="",dec=".",fill=TRUE)

            if (doAlloc == 1)
            {
                nTimes <<- length(dataLis[,timeCol])
                nCols <<- length(mgmtOptions) + 1
                crpvalLisData <<- array(data = "", dim = c(nTimes,nCols)) #crpval is a string

                aglivcData <<- array(data = 0.0, dim = c(nTimes,nCols))
                bglivcData <<- array(data = 0.0, dim = c(nTimes,nCols))

                som1c2Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                som2c2Data <<- array(data = 0.0, dim = c(nTimes,nCols))
                som3cData <<- array(data = 0.0, dim = c(nTimes,nCols))
                somscData <<- array(data = 0.0, dim = c(nTimes,nCols))

                # Set the first column in each array to the time variable

                crpvalLisData[,1] <<- dataLis[,timeCol]
                aglivcData[,1] <<- dataLis[,timeCol]
                bglivcData[,1] <<- dataLis[,timeCol]

                som1c2Data[,1] <<- dataLis[,timeCol]
                som2c2Data[,1] <<- dataLis[,timeCol]
                som3cData[,1] <<- dataLis[,timeCol]
                somscData[,1] <<- dataLis[,timeCol]

                doAlloc <- 0
            }

            aglivcData[,colCnt] <<- dataLis[,aglivcCol] 
            bglivcData[,colCnt] <<- dataLis[,bglivcjCol] + dataLis[,bglivcmCol]

            som1c2Data[,colCnt] <<- dataLis[,som1c2Col]
            som2c2Data[,colCnt] <<- dataLis[,som2c2Col]
            som3cData[,colCnt] <<- dataLis[,som3cCol]
            somscData[,colCnt] <<- dataLis[,somscCol]

            for (i in 1:length(dataLis[,crpvalCol]))
            {
                crpvalLisData[i,colCnt] <<- toString(dataLis[i,crpvalCol])    # crpval is a string
            }
        }
        else
        {
           msg <<- paste(lisFileName, " does not exist.")
           print(msg)
        }
    }

} #End function ReadDataLis()

#-------------------------------------------------------------------------------------------------
# Function ReadDataHarvest
# * Read the cgrain, egrain(N), crmvst, ermvst(N), and addsd columns from a set of  
#   harvest.csv files and store into cgrainData, ngrainData, crmvstData, nrmvstData, 
#   and cStoverResidueData.
# * This function assumes that the harvest.csv files for all mgmtOptions have the 
#   same number of lines.

ReadDataHarvest = function ()
{
  
    # Columns to extract from harvest.csv file
    timeCol <- 1
    crpvalCol <- 3
    cgrainCol <- 7
    ngrainCol <- 8
    crmvstCol <- 11
    nrmvstCol <- 12
    addsdCol <- 23  # Crop residue that remains as standing dead after harvest
    residCol <- 27  # Crop residue that falls to the ground after harvest

    doAlloc <- 1  # Set to 0 after array somscData has been allocated
    colCnt <- 1   # Column in somscData to store somsc values

    for (mgmt in mgmtOptions)
    {
        harvFileName <<- paste("harvest", mgmt, sep="_")
        harvFileName <<- paste(harvFileName, ".csv", sep="")
        harvFileName <<- paste(modelPath,harvFileName,sep="/")

        colCnt <- colCnt + 1
        harvFileName
        if (file.exists(harvFileName))
        {        
            dataHarv <<- read.csv(file=harvFileName,skip=1,header=FALSE,dec=".")

            if (doAlloc == 1)
            {
                nTimes <<- length(dataHarv[,timeCol])  # number of harvest events in harvest.csv
                nCols <<- length(mgmtOptions) + 1      # number of columns in new datasets
        
                # Save crpval, cgrain, egrain(N), crmvst, ermvst(N) values from the harvest.csv file

                crpvalData <<- array(data = "",  dim = c(nTimes,nCols)) # crpval is a string
                cgrainData <<- array(data = 0.0, dim = c(nTimes,nCols))
                ngrainData <<- array(data = 0.0, dim = c(nTimes,nCols))
                cStoverResidueData <<- array(data = 0.0, dim = c(nTimes,nCols))
                crmvstData <<- array(data = 0.0, dim = c(nTimes,nCols))
                nrmvstData <<- array(data = 0.0, dim = c(nTimes,nCols))

                crpvalData[,1] <<- dataHarv[,timeCol]
                cgrainData[,1] <<- dataHarv[,timeCol]
                ngrainData[,1] <<- dataHarv[,timeCol]
                cStoverResidueData[,1] <<- dataHarv[,timeCol]
                crmvstData[,1] <<- dataHarv[,timeCol]
                nrmvstData[,1] <<- dataHarv[,timeCol]
                doAlloc <- 0
            }
            for (i in 1:length(dataHarv[,crpvalCol]))
            {
                crpvalData[i,colCnt] <<- toString(dataHarv[i,crpvalCol])    # crpval is a string
            }
            cgrainData[,colCnt] <<- dataHarv[,cgrainCol]
            ngrainData[,colCnt] <<- dataHarv[,ngrainCol]
            crmvstData[,colCnt] <<- dataHarv[,crmvstCol]
            nrmvstData[,colCnt] <<- dataHarv[,nrmvstCol]
            # Assume above-ground stover inputs = total above-ground crop residue left on the field
            cStoverResidueData[,colCnt] <<- dataHarv[,addsdCol] + dataHarv[,residCol]
        }
        else
        {
           msg <<- paste(harvFileName, " does not exist.")
           print(msg)
        }
    }

} #End function ReadDataHarvest()

#-------------------------------------------------------------------------------------------------
# Function PlotDataSOMSC 
# * Plot graphs of simulated vs. observed total soil organic C for all mgmtOptions 
# * Set yrange if necessary to insure that all values show up in the graphs.

PlotDataSOMSC = function ()
{

    #startYr <- 1962
    startYr <- 1980
    #startYr <- 1900
    endYr <- 2022
    colvec <<- c("blue")
    xrange <- c(startYr,endYr)
    yrange <- c(1000,4000)
    obsYrCol <- 1
    obsSOMSCcol <- 4

    plotTitle = "SOMSC"
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in somscData with somsc values

    for (mgmt in mgmtOptions)
    {
        obsFile <- paste(modelPath,"/Data/",sep="") 
        obsFile <- paste(obsFile,"obs",sep="")
        obsFile <- paste(obsFile,siteName,sep="_")
        obsFile <- paste(obsFile,"som",sep="_")
        obsFile <- paste(obsFile ,mgmt,sep="_")
        obsFile <- paste(obsFile ,".csv",sep="")     
    
        soilVals <<- read.csv(obsFile,header=TRUE)  
     
        colCnt <- colCnt + 1
        matplot(somscData[,1],somscData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=mgmt,xlab="",ylab="",xlim=xrange,ylim=yrange)
        points(soilVals[,obsYrCol],soilVals[,obsSOMSCcol],pch=19,col="red")
    }
    PlotLabels(plotTitle, units)

} # End function PlotDataSOMSC()

#-------------------------------------------------------------------------------------------------
# Function PlotDataSOMAll 
# * Plot graphs of simulated vs. observed total soil C and include som1c(2), som2c(2), 
#   and som3c(2) for all mgmtOptions.
# * Set yrange if necessary to insure that all values show up in the graphs.

PlotDataSOMAll = function ()
{

    #startYr <- 1962
    startYr <- 1980
    #startYr <- 1900
    endYr <- 2022
    colvec <<- c("blue","darkorange","darkgreen","cyan")
    xrange <- c(startYr,endYr)
    yrange <- c(0,4000)
    obsYrCol <- 1
    obsSOMSCcol <- 4

    plotTitle = "SOMSC and Individual Pools"
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in somscData with somsc values

    for (mgmt in mgmtOptions)
    {
        obsFile <- paste(modelPath,"/Data/",sep="") 
        obsFile <- paste(obsFile,"obs",sep="")
        obsFile <- paste(obsFile,siteName,sep="_")
        obsFile <- paste(obsFile,"som",sep="_")
        obsFile <- paste(obsFile ,mgmt,sep="_")
        obsFile <- paste(obsFile ,".csv",sep="")     
    
        soilVals <<- read.csv(obsFile,header=TRUE)  
     
        colCnt <- colCnt + 1
        matplot(somscData[,1],somscData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=mgmt,xlab="",ylab="",xlim=xrange,ylim=yrange)
        matlines(som1c2Data[,1],som1c2Data[,colCnt],type="l",lwd=2,col=colvec[2])
        matlines(som2c2Data[,1],som2c2Data[,colCnt],type="l",lwd=2,col=colvec[3])
        matlines(som3cData[,1],som3cData[,colCnt],type="l",lwd=2,col=colvec[4])
        points(soilVals[,obsYrCol],soilVals[,obsSOMSCcol],pch=19,col="blue")
    }
    legvals <<- c("somsc","som1c(2)", "som2c(2)","som3c")
    legend(x="top",y="bottom",lwd=2,col=colvec,lty=1,legend=legvals,ncol=1)

    PlotLabels(plotTitle, units)

} # End function PlotDataSOMAll()

#-------------------------------------------------------------------------------------------------
# Function PlotDataCgrain 
# * Plot graphs of simulated vs. observed grain C for the specified crop for all mgmtOptions
# * Set ymin, ymax if necessary to insure that all values show up in the graphs.
# * Make sure "crpval" below correspond to those in your harvest.csv files

PlotDataCgrain = function (crop)
{

    if (crop == "corn")
    {
        crpval <<- "'C6'"
        ymin <- 1
        ymax <- 500
    }
    else if (crop == "sybn")
    {
        crpval <<- "'SYBN'"
        ymin <- 1
        ymax <- 300
    }
    else
    {
        print ("Unknown crop in PlotDataCgrain:", crop)
        STOP
    }
    startYr <- 1980
    endYr <- 2001
    colvec <<- c("blue")
    xrange <- c(startYr,endYr)
    yrange <- c(ymin,ymax)
    obsYrCol <- 4
    obsCgrainCol <- 14

    plotTitle <- paste(crop, "grain C", sep=" ")
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in cgrainData with cgrain values for the mgmt option

    # Observed values are in the Data folder.
    # Naming convention: obs_wooster_crop_rot_tt.csv, where
    #   crop = "corn" or "sybn"
    #   rot (rotation) = "ccc" or "cb"
    #   tt (tillage) = "ct", "rt", or "nt"
    for (mgmt in mgmtOptions)
    {
        obsFile <- paste(modelPath,"/Data/",sep="") 
        obsFile <- paste(obsFile,"obs",sep="")
        obsFile <- paste(obsFile,siteName,sep="_")
        obsFile <- paste(obsFile,crop,sep="_")
        obsFile <- paste(obsFile,mgmt,sep="_")
        obsFile <- paste(obsFile,".csv",sep="")     
    
        harvVals <- read.csv(obsFile,skip=3,header=FALSE)  
     
        colCnt <- colCnt + 1
        cgrainCropData <<- cgrainData[crpvalData[,colCnt]==crpval,]  #Filter out values for the crop. This assumes crpval for each crop type.
        matplot(cgrainCropData[,1],cgrainCropData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=mgmt,xlab="",ylab="",xlim=xrange,ylim=yrange)
        points(harvVals[,obsYrCol],harvVals[,obsCgrainCol],pch=19,col="red")
    }
    PlotLabels(plotTitle, units)

} # End function PlotDataCgrain() 

#-------------------------------------------------------------------------------------------------

# Function PlotDataCstover
# * Plot graphs of simulated vs. observed above-ground (stover) residue C for the specified 
#   crop for all mgmtOptions.
# * Set ymin, ymax if necessary to insure that all values show up in the graphs.
# * Make sure "crpval" below correspond to those in your harvest.csv files

PlotDataCstover = function (crop)
{

    if (crop == "corn")
    {
        crpval <<- "'C620'"
        ymin <- 1
        ymax <- 1200
    }
    else if (crop == "sybn")
    {
        crpval <<- "'SYBN100'"
        ymin <- 1
        ymax <- 300
    }
    else
    {
        print ("Unknown crop in PlotDataCstover:", crop)
        STOP
    }
    startYr <- 1962
    endYr <- 2013
    colvec <<- c("blue")
    xrange <- c(startYr,endYr)
    yrange <- c(ymin,ymax)
    obsYrCol <- 4
    obsCstoverCol <- 15

    plotTitle <- paste(crop, "Stover Residue C", sep=" ")
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in cStoverResidueData with above-ground residue values for the mgmt option

    # Observed values are in the Data folder.
    # Naming convention: obs_wooster_crop_rot_tt.csv, where
    #   crop = "corn" or "sybn"
    #   rot (rotation) = "ccc" or "cb"
    #   tt (tillage) = "ct", "rt", or "nt"
    for (mgmt in mgmtOptions)
    {
        obsFile <- paste(modelPath,"/Data/",sep="") 
        obsFile <- paste(obsFile,"obs",sep="")
        obsFile <- paste(obsFile,siteName,sep="_")
        obsFile <- paste(obsFile,crop,sep="_")
        obsFile <- paste(obsFile,mgmt,sep="_")
        obsFile <- paste(obsFile,".csv",sep="")     
    
        stoverVals <- read.csv(obsFile,skip=3,header=FALSE)  
     
        colCnt <- colCnt + 1
        cStoverResidueCropData <<- cStoverResidueData[crpvalData[,colCnt]==crpval,]  #Filter out values for the crop. This assumes crpval for each crop type.
        matplot(cStoverResidueCropData[,1],cStoverResidueCropData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=mgmt,xlab="",ylab="",xlim=xrange,ylim=yrange)
        points(stoverVals[,obsYrCol],stoverVals[,obsCstoverCol],pch=19,col="red")
    }
    PlotLabels(plotTitle, units)

} # End function PlotDataCstover() 

#-------------------------------------------------------------------------------------------------

# Function PlotDataShootC 
# * Plot graphs of simulated vs. observed shoot C for the specified crop for all mgmtOptions
# * Set ymin, ymax if necessary to insure that all values show up in the graphs.
# * Make sure "crpval" below correspond to those in your harvest.csv files

PlotDataShootC = function (crop)
{

    if (crop == "corn")
    {
        crpval <<- "C620"
        ymin <- 1
        ymax <- 1500
    }
    else if (crop == "sybn")
    {
        crpval <<- "SYBN100"
        ymin <- 1
        ymax <- 500
    }
    else
    {
        print ("Unknown crop in PlotDataCrmvst:", crop)
        STOP
    }
    startYr <- 1962
    #endYr <- 2013
    endYr <- 1995   # Observations cease in 1993
    colvec <<- c("blue")
    xrange <- c(startYr,endYr)
    yrange <- c(ymin,ymax)
    obsYrCol <- 4
    obsShootCol <- 10

    plotTitle <- paste(crop, "Shoot C", sep=" ")
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in aglivcData with aglivc values for the mgmt option

    # Observed values are in the Data folder.
    # Naming convention: obs_wooster_crop_rot_tt.csv, where
    #   crop = "corn" or "sybn"
    #   rot (rotation) = "ccc" or "cb"
    #   tt (tillage) = "ct", "rt", or "nt"
    for (mgmt in mgmtOptions)
    {
        obsFile <- paste(modelPath,"/Data/",sep="") 
        obsFile <- paste(obsFile,"obs",sep="")
        obsFile <- paste(obsFile,siteName,sep="_")
        obsFile <- paste(obsFile,crop,sep="_")
        obsFile <- paste(obsFile,mgmt,sep="_")
        obsFile <- paste(obsFile,".csv",sep="")     
    
        harvVals <<- read.csv(obsFile,skip=3,header=FALSE) 
        harvVals[,obsShootCol] <<- 0.40*(harvVals[,obsShootCol])/10.0  # convert kg/ha dry matter to gC/m2
     
        colCnt <- colCnt + 1
        aglivcCropData <<- aglivcData[crpvalLisData[,colCnt]==crpval,]  #Filter out values for the crop. This assumes crpval for each crop type.
        matplot(aglivcCropData[,1],aglivcCropData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=mgmt,xlab="",ylab="",xlim=xrange,ylim=yrange)
        points(harvVals[,obsYrCol],harvVals[,obsShootCol],pch=19,col="red")
    }
    PlotLabels(plotTitle, units)

} # End function PlotDataShootC("corn") 

#-------------------------------------------------------------------------------------------------

# Function PlotDataRootC 
# * Plot graphs of simulated vs. observed root C for the specified crop for all mgmtOptions
# * Set ymin, ymax if necessary to insure that all values show up in the graphs.
# * Make sure "crpval" below correspond to those in your harvest.csv files

PlotDataRootC = function (crop)
{

    if (crop == "corn")
    {
        crpval <<- "C620"
        ymin <- 1
        ymax <- 500
    }
    else if (crop == "sybn")
    {
        crpval <<- "SYBN100"
        ymin <- 1
        ymax <- 200
    }
    else
    {
        print ("Unknown crop in PlotDataCrmvst:", crop)
        STOP
    }
    startYr <- 1962
    endYr <- 2013
    colvec <<- c("blue")
    xrange <<- c(startYr,endYr)
    yrange <<- c(ymin,ymax)
    obsYrCol <- 4
    obsRootCol <- 12

    plotTitle <- paste(crop, "Root C (sim=total, obs=20cm)", sep=" ")
    units = "gC/m2"
    PlotLayout()
    
    colCnt <- 1   # Column in bglivcData with bglivc values for the mgmt option

    # Observed values are in the Data folder.
    # Naming convention: obs_wooster_crop_rot_tt.csv, where
    #   crop = "corn" or "sybn"
    #   rot (rotation) = "ccc" or "cb"
    #   tt (tillage) = "ct", "rt", or "nt"
    for (mgmt in mgmtOptions)
    {
        obsFile <- paste(modelPath,"/Data/",sep="") 
        obsFile <- paste(obsFile,"obs",sep="")
        obsFile <- paste(obsFile,siteName,sep="_")
        obsFile <- paste(obsFile,crop,sep="_")
        obsFile <- paste(obsFile,mgmt,sep="_")
        obsFile <- paste(obsFile,".csv",sep="")     
    
        harvVals <- read.csv(obsFile,skip=3,header=FALSE)  
     
        colCnt <- colCnt + 1
        bglivcCropData <<- bglivcData[crpvalLisData[,colCnt]==crpval,]  #Filter out values for the crop. This assumes crpval for each crop type.
        matplot(bglivcCropData[,1],bglivcCropData[,colCnt],type="l",lwd=2,col=colvec,lty=1,main=mgmt,xlab="",ylab="",xlim=xrange,ylim=yrange)
        points(harvVals[,obsYrCol],harvVals[,obsRootCol],pch=19,col="red")
    }

    PlotLabels(plotTitle, units)

} # End function PlotDataRootC() 

#-------------------------------------------------------------------------------------------------

PlotLayout = function()
{
    #library(gplots)


    # 2x2 matrix with title 
    l <<- layout(matrix(c(seq(1,4)), 2,2, byrow = T), widths=c(3,3), heights=c(3,3))

    # Margin sizes for bottom, left, top, right
    par(mar = c(1, 4, 4, 1),  oma = c(0, 2, 2, 0))

} # End function PlotLayout()

#-------------------------------------------------------------------------------------------------

PlotLabels = function(plotTitle, units)
{
    mtext(plotTitle, side = 3, outer=TRUE, cex = 1.5)
    mtext(units, side = 2, outer=TRUE, cex = 1.5)

} #End function PlotLabels()

#-------------------------------------------------------------------------------------------------

# Read simulated soil C (somsc), live shoot biomass (aglivc), and live root biomass (bglivc) from .lis files
ReadDataLis()

# Read simulated grain harvest and crop residue from harvest.csv files
ReadDataHarvest()

PlotDataShootC("corn")
PlotDataRootC("corn")
PlotDataCgrain("corn")
#PlotDataCstover("corn")

PlotDataShootC("sybn")
PlotDataRootC("sybn")
PlotDataCgrain("sybn")
#PlotDataCstover("sybn")

#PlotDataSOMAll() 
PlotDataSOMSC()  



