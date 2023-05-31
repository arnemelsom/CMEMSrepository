
# Software to create monthly climatology, for
#  CMEMS product      ARCmyWAV
#  stored under MYpath/<year>/<month>/
#   as          <yyyy><mm><dd>_MyWam3km_hindcast-cmems.nc
#  for variables in varList, written to
#               outPath/clim<mm>_MyWam3km_hindcast-cmems.nc

ARCmyWAV <- "ARCTIC_MULTIYEAR_WAV_002_013"
varList  <- c("VHM0","VTM02")
nVar     <- length(varList)


# Dependencies:
#  library: RNetCDF (https://cran.r-project.org/web/packages/RNetCDF/index.html)
#  functions in accompanying script  copyVarAtts.R


# Read input, output directories from the command line:
cargs      <- Sys.getenv(c('inPath', 'outPath', 'Rpath'))
print(cargs)
MYpath  <- cargs[1]
outPath <- cargs[2]
Rpath   <- cargs[3] # ...path for local R functions script
if (Rpath == '') {
    print("Syntax e.g.:")
    print(" inPath='/disk1/CMEMS/MY'; outPath='/disk1/CMEMS/climatology'; Rpath='/home/usr/Rsrc'")
    print(" export inPath outPath Rpath")
    print(" R CMD BATCH makeWaveClimatology.R")
    quit("no")
}

# Year range:
iniYear  <- 1991
endYear  <- 2020
cntrYear <- (iniYear + endYear)%/%2  # ...see https://github.com/cf-convention/discuss/issues/236

# List of  months:
monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")
monthDays <- c( 31 ,  0 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 ) # February set below

# Names of x,y dimensions (input, output):
xName <- "rlon"
yName <- "rlat"
# Name of time dimension (input):
tName <- "time"
# Name of climatology (time) dimension (output):
climName <- tName
# List of attributes to copy from tName to climName:
attClimList <- c("units","axis","standard_name","calendar")
# Name of new bounds variable (auxillary, ref.: climName), attributes to copy:
climBndName    <- "climatology_bounds"
attClimBndList <- c("units","calendar")

# Lists of auxillary variables to copy (e.g. depth):
auxVarList0D <- "projection_stere"
auxVarList2D <- c("model_depth","lon","lat")

# List of global attributes to copy:
attGlobList <- c("institution","source","comment","Conventions")

# Additional variable, for concentration (input),
#                          frequency of ice cover (output):
iceConcName  <- "SIC"
iceFrqcyName <- "ice_cover_frequency"
iceAttList   <- c("_FillValue","coordinates","grid_mapping")


# Load required library, functions:
library(RNetCDF)
source(paste(Rpath,"copyVarAtts.R",   sep="/"))


# Dimension name lists:
dimList2D <- c(xName,yName)
dimList3D <- c(xName,yName,climName)


monthNo <- 0
for (month in monthList) {
    monthNo <- monthNo + 1
    print(paste("Handling MONTH",month))

    # Set name of output file:
    outName <- paste("clim",month,"_MyWam3km_hindcast-cmems.nc", sep="")
    outFile <- paste(outPath,outName, sep="/")

    fileCreated <- FALSE
    gotLandMask <- FALSE

    for (year in iniYear:endYear) {
        print(paste("Handling YEAR ",year))

        # Days in February:
        im <- which(monthList == "02")
        if (length(im == 1))
            if (year%%4 == 0) monthDays[im] <- 29 else
                              monthDays[im] <- 28       # year 2100 BUG!

        for (day in 1:monthDays[monthNo]) {
            print(paste("Handling day   ",day))
            if (day > 9) cLead <- "" else cLead <- "0"
            cDay <- paste(cLead,day, sep="")            # two-charater day

            # Open input file:
            inName <- paste(year,month,cDay,"_MyWam3km_hindcast-cmems.nc", sep="")
            inFile <- paste(MYpath,year,month,inName, sep="/")
            inID   <- open.nc(inFile)


            # Create output file?
            if (!fileCreated) {


                #########################
                # ...CREATE OUTPUT FILE #
                #      S  T  A  R  T    #

                print(paste(" ...creating output file",outFile))
                outID   <- create.nc(outFile, clobber=FALSE)

                # ...define dimensions:
                def2Dclim(inID,outID,xName,yName,climName,1)   #  from copyVarAtts.R
                dim.def.nc(outID,"nbnd",2)

                # ...dimension variables:
                copyVar(inID,outID,xName,xName)                #  from copyVarAtts.R
                copyVar(inID,outID,yName,yName)                #  from copyVarAtts.R
                var.def.nc(outID,climName,"NC_DOUBLE",climName)
                for (attName in attClimList)
                    att.copy.nc(inID,tName,attName,outID,climName)
                att.put.nc(outID,climName,"cell_methods","NC_CHAR","time: mean within years time: mean over years")
                att.put.nc(outID,climName,"climatology", "NC_CHAR",climBndName)
                tUnitsClim <- att.get.nc(outID,climName,"units")

                # ...auxillary variables:
                for (varName in auxVarList0D)
                    copyVar(inID,outID,NA,varName)             #  from copyVarAtts.R
                for (varName in auxVarList2D)
                    copyVar(inID,outID,dimList2D,varName)      #  from copyVarAtts.R
                var.def.nc(outID,climBndName,"NC_DOUBLE",c("nbnd",climName))
                att.put.nc(outID,climBndName,"long_name","NC_CHAR","time interval endpoints")
                for (attName in attClimBndList)
                    att.copy.nc(outID,climName,attName,outID,climBndName)

                # ...define main output variables:
                for (varName in varList)
                    copyVarDef(inID,outID,dimList3D,varName)   #  from copyVarAtts.R

                # ...define variable for ice cover frequency
                #    (has no appropriate standard_name attribute):
                var.def.nc(outID,iceFrqcyName,"NC_FLOAT",dimList3D)
                att.put.nc(outID,iceFrqcyName,"units",     "NC_CHAR","1")
                att.put.nc(outID,iceFrqcyName,"long_name", "NC_CHAR","frequency_of_ice_cover")
                att.put.nc(outID,iceFrqcyName,"definition","NC_CHAR",
                                 "fraction of time covered by sea ice; e.g. =0: never ice, =1: always ice")
                for (attName in iceAttList)
                    att.copy.nc(inID,varList[1],attName,outID,iceFrqcyName)

                # ...set global attributes:
                for (attName in attGlobList)
                    att.copy.nc(inID,"NC_GLOBAL",attName,outID,"NC_GLOBAL")
                fieldDay  <- paste(cntrYear,month,15, sep="-")
                fieldDate <- paste(fieldDay,"00:00:00", sep="T")
                att.put.nc(outID,"NC_GLOBAL","field_date",
                                 "NC_CHAR",fieldDate)
                att.put.nc(outID,"NC_GLOBAL","Copernicus_Marine_product",
                           "NC_CHAR","ARCTIC_MULTIYEAR_WAV_002_013")
                att.put.nc(outID,"NC_GLOBAL","Copernicus_Marine_dataset",
                           "NC_CHAR","cmems_mod_arc_wav_my_3km-climatology_PT1M-m")
                att.put.nc(outID,"NC_GLOBAL","references",
                           "NC_CHAR","http://marine.copernicus.eu")
                att.put.nc(outID,"NC_GLOBAL","title",
                                 "NC_CHAR","Arctic Ocean Wave Analysis, 3km monthly climatology")
                sysDate <- system("date --rfc-3339=s", intern=TRUE)
                history <- paste(sysDate,"Created by R script makeWaveClimatology.R", sep=":")
                att.put.nc(outID,"NC_GLOBAL","history","NC_CHAR",history)
                att.put.nc(outID,"NC_GLOBAL","GitHub_doi","NC_CHAR",
                                 "10.5281/zenodo.7991137")


                # Reset flag:
                fileCreated <- TRUE
                print(" ...output file successfully initialized")

                # ...PREPARATIONS/INITIALIZATIONS:
                nx        <- dim.inq.nc(inID,xName)$length
                ny        <- dim.inq.nc(inID,yName)$length
                iniTime   <- var.get.nc(inID,tName,start=1,count=1)
                nCount    <- 0
                varSum    <- array(0,dim=c(nx,ny,nVar))
                oceanMask <- array(  dim=c(nx,ny))
                nIce      <- array(0,dim=c(nx,ny))

                # For subsetting:
                start <- c( 1, 1,1)
                count <- c(nx,ny,1)


                #         E  N  D       #
                # ...CREATE OUTPUT FILE #
                #########################


            }    #  <-  if (!fileCreated)


            nt      <- dim.inq.nc(inID,tName)$length
            endTime <- var.get.nc(inID,tName,start=nt,count=1)   # ...updated on the fly
            for (t in 1:nt) {
                nCount   <- nCount + 1
                start[3] <- t
                # Update ice frequency count, set mask:
                iceVar         <- var.get.nc(inID,iceConcName,start=start,count=count)
                iceGrids       <- which(iceVar > 0)
                nIce[iceGrids] <- nIce[iceGrids] + 1
                oceanMask[]    <- 1
                oceanMask[iceGrids] <- 0
                # Update sum of variable values:
                varNo    <- 0
                for (varName in varList) {
                    varNo <- varNo + 1
                    var   <- var.get.nc(inID,varName,start=start,count=count)
                    varSum[,,varNo] <- varSum[,,varNo] + oceanMask*var
                }
            }    #  <-  for (t in nt)

            close.nc(inID)

        }        #  <-  for (day in 1:monthDays[monthNo])

    }            #  <-  for (year in iniYear:endYear)

    # Handle time information:
    cntrTimeVal <- utinvcal.nc(tUnitsClim, c(cntrYear,as.numeric(month),15,0,0,0))
    var.put.nc(outID,climName,   cntrTimeVal)
    var.put.nc(outID,climBndName,c(iniTime,endTime),start=c(1,1),count=c(2,1))

    # Compute, write to output file:
    start[3]           <- 1
    iceFrqcy           <- nIce/nCount
    landMask           <- which(is.na(var))     # land masked grids
    iceFrqcy[landMask] <- NA                    # impose land mask
    var.put.nc(outID,iceFrqcyName,iceFrqcy,start=start,count=count)
    iceMask            <- which(iceFrqcy == 1)  # ice mask, to be imposed on output variables
    varNo              <- 0
    outFrqcy           <- 1 - iceFrqcy
    for (varName in varList) {
        varNo           <- varNo + 1
        outVar          <- varSum[,,varNo]/(outFrqcy*nCount)
        outVar[iceMask] <- NA                   # masking for grids always ice infested
        var.put.nc(outID,varName,outVar,start=start,count=count)
    }

    close.nc(outID)
    print(" ...output file complete")

}                #  <-  for (month in monthList)

warnings()
quit("no")
