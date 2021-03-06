
#linux
fileName <- "/home/kresimir/Projects/FormatAnalysis/fmts-cleaned.tsv"
#mac
#fileName <- "/Users/kresimir/Projects/Format Analysis/fmts-cleaned.tsv"
#test
#fileName <- "/home/kresimir/Projects/FormatAnalysis/test.tsv"

#sync folder for gathering all plots 
syncFolder <- "/home/kresimir/Dropbox/formatanalysis (1)/figures and other material/201605 figures and other material/201605251200/models"

colNames <- c("server", "tika", "droid", "year", "amount")


#experiments <- c("HTML-2")
#marketFiles <- c("Format markets - HTML and XHTML versions.tsv")

# experiments <- c("PDFS figures", "GIFS figures", "HTML figures", "DOCUMENTS figures", "DISTILLER figures", "IMAGES figures")
# marketFiles <- c("Format markets - PDF versions.tsv", "Format markets - GIFS.tsv", "Format markets - HTML and XHTML versions.tsv",
#                  "Format markets - DOCUMENTS.tsv", "Format markets - DISTILLER.tsv", "Format markets - IMAGES.tsv")

 experiments <- c("ARCHIVE", "AUDIO", "BMPS", "DISTILLER", "DOCUMENTS",
                  "FLASH", "GIFS", "HTML", "IMAGES", "PDFS", "VIDEO")
 marketFiles <- c("Format markets - ARCHIVE.tsv", "Format markets - AUDIO.tsv", "Format markets - BMPS.tsv",
                  "Format markets - DISTILLER.tsv", "Format markets - DOCUMENTS.tsv", "Format markets - FLASH versions.tsv",
                  "Format markets - GIFS.tsv", "Format markets - HTML and XHTML versions.tsv", "Format markets - IMAGES.tsv",
                  "Format markets - PDF versions.tsv", "Format markets - VIDEO.tsv")




# # market definition
# experiments <- c("DOCUMENTS-3-no-text")
# marketFiles <- c("Format markets - DOCUMENTS.tsv")


# # market definition
# experiments <- c("IMAGES", "PDFS", "DISTILLER", "HTML", "DOCUMENTS")
# marketFiles <- c("Format markets - IMAGES.tsv", "Format markets - PDF versions.tsv", "Format markets - DISTILLER.tsv",
#                  "Format markets - HTML and XHTML versions.tsv", "Format markets - DOCUMENTS.tsv")

#years used to estimate the model
start <- 1994
end <- 2010


#years for which predictions should be done
predictionYears <- c(2009, 2010, 2011)

#if a three year moving average should be used 
useMovingAverage <- TRUE

# multiplication factor for normalizing markets sizes
multiplicationFactor <- 1000 


# #plot settings
# includeInterval <- TRUE
# includeRateOfChange <- TRUE
# includePoints <- TRUE
# includeResidual <- TRUE

# load conflict resolution functions and assign them to variables to be used later
source('conflictResolution.R')
resolveConflicts <- conflictResolution
afterConflictsResolution <- afterResolution
conflictCategory <- conflictCategoryDetermination






#plot parameters

themeMain <- theme(axis.ticks=element_line(color="gray", linetype = 2, size = 0.2), 
                   axis.text = element_text(size=9),
                   axis.title = element_text(size=9),
                   plot.margin=unit(c(0.0,0.0,0.0,0.0), "mm"), 
                   panel.background=element_blank(),
                   panel.grid.major = element_line(color="gray", linetype = 2, size = 0.2),
                   panel.grid.minor = element_blank(),
                   legend.key = element_rect(colour = NA),
                   axis.text=element_text(size = 15),
                   panel.background = element_blank(),
                   axis.line.x = element_line(color="black", size = 0.4),
                   axis.line.y = element_line(color="black", size = 0.4))







