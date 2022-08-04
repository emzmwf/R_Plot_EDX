#
#
#MSA plotter - for files saved in msa format (either .msa or .txt)
# This code uses the ggplot library
library(ggplot2)

###############################################
###DEFINE FILE NAME HERE                    ###
###NOTE, had to be forward slashes, not back###
###############################################

#Example EDX plot
fname = 'EDX example.msa'


#Check this file is a suitable type - read in lines
rl <- readLines(fname)
#use grep to loof for the SIGNALTYPE
r <- unlist(strsplit(rl[grep('SIGNALTYPE', rl)], ":"))
r[2]

#Add placeholder axis labels
AX1 = 'xaxis'
AX2 = 'yaxis'

#This code can work with EDS or EELS signal types
if (r[2] == ' EDS'){
  print('Data type is EDS')
  AX1 = 'keV'
  AX2 = 'counts'
} else if (r[2] == ' ELS'){
    print('Data type is EELS')
    AX1 = 'eV'
    AX2 = 'counts'
  } else{
    stop('Data type not recognised')
  }

#Now get the energy and count values as a dataframe
MSA <- read.csv(file = fname, header = FALSE, sep = ",", comment.char = "#")

# Get the X and Y values as numbers
x_num <- as.numeric(MSA$V1)
y_num <- as.numeric(MSA$V2)

###################################  
#Subroutine for handling EDX files
###################################
EDSPlot <- function(){

#Grep any labels for element peaks/edges
LABDAT = rl[grep('OXINSTLABEL', rl)]
# Split as string at commas, as we only need the x-value and label
r <- unlist(strsplit(LABDAT, ","))
#Split as ID, energy, element
datlab = data.frame(id=(r[c(TRUE, FALSE, FALSE)]), energy=as.numeric(r[c(FALSE, TRUE, FALSE)]), element=r[c(FALSE, FALSE, TRUE)])

# use the c() command with approx to look up the counts for the data label x values 
new <- c(approx(x = x_num, y = y_num, xout = datlab$energy))

#Add the counts values from new to a new column of datlab
datlab['ecounts'] <- new[2]


#Get the median value of the data for later use
MedVal = summary(MSA)[3, 2]
mv <- unlist(strsplit(MedVal, ":"))
mval = as.numeric(mv[2])

#clear some no longer needed variables
rm(MedVal)
rm(mv)
rm(new)
rm(r)

#Set up plot
p <- ggplot() +
  geom_line(data = MSA, aes(x=x_num, y=V2, group=1), color="red")+
  xlab(AX1)+ ylab(AX2)+  
  geom_point(data = datlab,alpha = 0, aes(x=energy, y=ecounts))+    #alpha set to zero so points are transparent
  geom_text(data = datlab,
            aes(x=energy, y=ecounts, label = element),
            vjust = 0,
            nudge_y = 5*mval,            ##nudge the y value so the label
                                         ## does not overlap the peak
            ## use the mean value we found earlier
            check_overlap=TRUE, 
            size = 5)  #Define font size here, can't change it later
p  

#Adjust with limits and font size
#Apologies for all the warnings
#I haven't found how to suppress these
pfin <- suppressWarnings(p + xlim(-0.5, 10)+ theme(text = element_text(size=20)))  #try to ignore the warnings
pfin
return(pfin)
}


###################################  
#Subroutine for handling EELS files
###################################
EELSPlot <- function(){
  #Get the minimum value of the data for later use
  MinVal = summary(MSA)[1, 2]
  mv <- unlist(strsplit(MinVal, ":"))
  mval = as.numeric(mv[2])
  
  #clear some no longer needed variables
  rm(MinVal)
  rm(mv)
  
  pfin <- ggplot() +
    geom_line(data = MSA, aes(x=x_num, y=V2, group=1), color="red")+
    xlab(AX1)+ ylab(AX2)+
    theme(text = element_text(size=20))
  pfin
  return(pfin)
}


###################################
#Now back to the main script
##################################
if (r[2] == ' EDS'){
  pfin = EDSPlot()
} else if (r[2] == ' ELS'){
  pfin = EELSPlot()
}


#Set up the output filename - we'll save with the same file name but .png added to the end
fout = paste(fname, '_R.png')

# define the export details for the graph
# default unit is inches
# for approx four in one on-screen slide
figwid = 7
fighgt = 3
dots = 300

##save the plot using ggsave
##and try to ignore the warnings again
ggsave(plot = pfin, width = figwid, height = fighgt, dpi = dots, filename = fout)

#Clear data at the end of this process
rm(MSA)
rm(AX1)
rm(AX2)
rm(dots)
rm(fighgt)
rm(figwid)
rm(fname)
rm(fout)
rm(rl)
rm(x_num)
rm(y_num)
rm(r)

