#
#
#MSA plotter - for files saved in msa format (either .msa or .txt)
# This code uses the ggplot library
library(ggplot2)


###################################  
#Subroutine for handling EDX files
###################################
EDSPlot <- function(AX1='keV', AX2='counts'){
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

  # Get the maximum value between the range defined
  # Numbers defined with file selection
  ypmin = (ypeakmin+0.42)*50
  ypmax = (ypeakmax+0.42)*50
  
  countsmax = 1.25*max(y_num[ypmin:ypmax])

  #Set up plot
  p <- ggplot() +
    geom_line(data = MSA, aes(x=x_num, y=V2, group=1), color=linecol, size = linewid)+
    xlab(AX1)+ ylab(AX2)+  
    geom_point(data = datlab,alpha = 0, aes(x=energy, y=min(countsmax, ecounts)))+    #alpha set to zero so points are transparent
    geom_text(data = datlab,
              aes(x=energy, y=ecounts, label = element),
              vjust = 0,
              nudge_y = 5*mval,            ##nudge the y value so the label
              ## does not overlap the peak
              ## use the mean value we found earlier
              check_overlap=TRUE, 
              size = 4)  #Define font size here, can't change it later
  p  
  
  #Adjust with limits and font size

  
  #Apologies for all the warnings
  #I haven't found how to suppress these
  pfin <- suppressWarnings(p + theme_bw()+xlim(-xmin, xmax)+ ylim(0,countsmax)+theme(text = element_text(size=16)))+theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
           panel.background = element_blank()) 
  pfin
  return(pfin)
}


###################################  
#Subroutine for handling EELS files
###################################
EELSPlot <- function(AX1='eV', AX2='counts'){
  #Get the minimum value of the data for later use
  MinVal = summary(MSA)[1, 2]
  mv <- unlist(strsplit(MinVal, ":"))
  mval = as.numeric(mv[2])
  
  #clear some no longer needed variables
  rm(MinVal)
  rm(mv)
  
  p <- ggplot() +
    geom_line(data = MSA, aes(x=x_num, y=V2, group=1), color=linecol, size = linewid)+
    xlab(AX1)+ ylab(AX2)+
    theme(text = element_text(size=20))
  p
  plot(p)
  
  pfin <- suppressWarnings(p + theme_bw()+xlim(800, 1200)+ylim(-200, 1200)+theme(text = element_text(size=16)))+theme(panel.grid.minor = element_blank(),
                                                                                                                       panel.grid.major = element_blank(),
                                                                                                                       panel.background = element_blank()) 
  
  pfin
  plot(pfin)
  pfin <- pfin+geom_vline(xintercept=931)# add line for Cu L3 edge
  pfin <- pfin+geom_vline(xintercept=1072)# add line for Cu L3 edge
  pfin <- pfin + geom_text(aes(x=931, label="\nCu L", y=-100), colour="black", angle=90) +
                  geom_text(aes(x=1072, label="\nNa K", y=-100), colour="black", angle=90)
  plot(pfin)
  return(pfin)
}


###################################  
#Subroutine for parsing and plotting
###################################

parsemsa_function <- function(fname) {
  #Check this file is an suitable type - read in lines
  rl <<- readLines(fname)
  #use grep to loof for the SIGNALTYPE
  r <<- unlist(strsplit(rl[grep('SIGNALTYPE', rl)], ":"))
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
  MSA <<- read.csv(file = fname, header = FALSE, sep = ",", comment.char = "#")
  
  # Get the X and Y values as numbers
  x_num <<- as.numeric(MSA$V1)
  y_num <<- as.numeric(MSA$V2)
  
  if (r[2] == ' EDS'){
    EDSPlot(AX1, AX2)
    pfin = EDSPlot()
    print('Plotting EDS')
  } else if (r[2] == ' ELS'){
    pfin = EELSPlot()
  }
  
  #For some reason the bit above doesn't trigger, so just run the 
  #relevent section here
  EDSPlot()
  
  
  #Set up the output filename - we'll save with the same file name but .png added to the end
  fout = paste(fname, '.png')

  
  ##save the plot using ggsave
  ##and try to ignore the warnings again
  ggsave(plot = pfin, width = figwid, height = fighgt, dpi = dots, filename = fout)
  
  plot(pfin)
  
  #Clear data at the end of this process
  rm(AX1)
  rm(AX2)
  rm(dots)
  rm(fighgt)
  rm(figwid)
  rm(fname)
  rm(fout)
  
  
}

#########################
# Main part
##########################

###############################################
###DEFINE FILE NAME HERE                    ###
###NOTE, had to be forward slashes,         ###
### or double back slashes                  ###
###                                         ###
### Output file will be saved in same folder###
###############################################

#Example EDX plot
fname = 'D:/_ExampleData/Blank Grid References/ContinousCarbon.txt'

#Or if rstudioapi is installed can use user interface to load
#library(rstudioapi)
#fname <- rstudioapi::selectFile(caption = "Select msa File",
#                               filter = "MSA or Text Files (*.msa | *.txt)",
#                               existing = TRUE)

# Define x plot range in keV - note C at 0.277
xmin = 0.25
xmax = 20

# Define keV range to look for highest peak
# For TEM samples on carbon film on Cu, may want to select between the two Cu peaks (0.930 and 8.040keV)
ypeakmin = 0.25
ypeakmax = 20

# Define line width and colour
linewid = 1
linecol = "red"

# define the export details for the graph
# default unit is inches
# for approx four in one on-screen slide
figwid = 7
fighgt = 3
dots = 300

parsemsa_function(fname)

## Remember, CTRL+SHIFT+ENTER is keyboard shortcut to run whole file

#########################################################################
## TODO - option to find and process all msa format text files in folder
#########################################################################


