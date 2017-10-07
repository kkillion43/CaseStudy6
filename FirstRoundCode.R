
options(digits = 2)
setwd("C:/Users/kkillion/Desktop/SMU - MSDS/Quant the World/CaseStudy6")
txt = readLines("offline.txt")

sum(substr(txt, 1, 1) == "#")

length(txt)

strsplit(txt[4], ";")[[1]]

tokens = strsplit(txt[4], "[;=,]")[[1]]

tokens

tokens[c(2, 4, 6:8, 10)]

tokens[ - ( 1:10 ) ]

tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
mat = cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp),
                   ncol = 6, byrow = TRUE), 
            tmp)

dim(mat)

#####################################

#####################################

processLine = function(x) {
  tokens = strsplit(x, "[;=,]")[[1]]
  tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp),
               ncol = 6, byrow = TRUE), tmp)
}

tmp = lapply(txt[4:20], processLine)

sapply(tmp, nrow)

offline = as.data.frame(do.call("rbind", tmp))
dim(offline)

lines = txt[ substr(txt, 1, 1) != "#" ]
tmp = lapply(lines, processLine) # This line generates the error

##########################################

##########################################

offline_backup = offline        # Note that R has state -- add a "checkpoint"

names(offline) = c("time", "scanMac", "posX", "posY", "posZ", 
                   "orientation", "mac", "signal", 
                   "channel", "type")

numVars = c("time", "posX", "posY", "posZ", 
            "orientation", "signal")
offline[ numVars ] =  lapply(offline[ numVars ], as.numeric)  # idempotent

offline = offline[ offline$type == "3", ]                     # idempotent
offline = offline[ , "type" != names(offline) ]               # idempotent
dim(offline)

offline$rawTime = offline$time
offline$time = offline$time/1000
class(offline$time) = c("POSIXt", "POSIXct")         # Use of "functional" assignment

unlist(lapply(offline, class))

summary(offline[, numVars])

summary(sapply(offline[ , c("mac", "channel", "scanMac")],
               as.factor))

offline = offline[ , !(names(offline) %in% c("scanMac", "posZ"))]  # Get rid of two columns

length(unique(offline$orientation))

plot(ecdf(offline$orientation))

####################################

####################################

pdf(file = "Geo_ECDFOrientation.pdf", width = 10, height = 7)
oldPar = par(mar = c(4, 4, 1, 1))
plot(ecdf(offline$orientation), pch = 19, cex = 0.3,
     xlim = c(-5, 365), axes = FALSE,
     xlab = "orientation", ylab = "Empirical CDF", main = "")
box()
axis(2)
axis(side = 1, at = seq(0, 360, by = 45))
par(oldPar)
dev.off()

pdf(file = "Geo_DensityOrientation.pdf", width = 10, height = 5)
oldPar = par(mar = c(4, 4, 1, 1))
plot(density(offline$orientation, bw = 2), 
     xlab = "orientation", main = "")
par(oldPar)
dev.off()
roundOrientation = function(angles) {
  refs = seq(0, by = 45, length  = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

offline$angle = roundOrientation(offline$orientation)


# pdf(file = "Geo_BoxplotAngle.pdf", width = 10)
# oldPar = par(mar = c(4, 4, 1, 1))
# boxplot(offline$orientation, bw = 2, xlab = "orientation", main = "")
# par(oldPar)
# dev.off()
c(length(unique(offline$mac)), length(unique(offline$channel)))

table(offline$mac)

subMacs = names(sort(table(offline$mac), decreasing = TRUE))[1:7] # Get the 7 most common mac addresses
offline = offline[ offline$mac %in% subMacs, ]

# establish that macChannel and mac are redundant
macChannel = with(offline, table(mac, channel))     # clever use of `table`
apply(macChannel, 1, function(x) sum(x > 0))

offline = offline[ , "channel" != names(offline)]

xlocDF = with(offline, 
              by(offline, posX, function(x) x))  # what does `by` do?


locDF = with(offline, 
             by(offline, list(posX, posY), function(x) x))  # for each value X and Y, return the data frame that matches
# posX and posY have to be *factors*
# But note that it takes the cartesian product of posX x posY, creating nulls

length(locDF)

# since this is a list, use sapply
sum(sapply(locDF, is.null))

locDF = locDF[ !sapply(locDF, is.null) ]

length(locDF)

locCounts = sapply(locDF, nrow)
locCounts  #166

# create a "map" of location counts
locCounts = sapply(locDF, 
                   function(df) 
                     c(df[1, c("posX", "posY")], count = nrow(df)))


class(locCounts)  # sapply created a matrix---why?

dim(locCounts)

locCounts[ , 1:8]


locCounts = t(locCounts)      # common occurence: transpose after running sapply

# again run in single block
pdf(file = "Geo_XYByCount.pdf", width = 10)
oldPar = par(mar = c(3.1, 3.1, 1, 1))
plot(locCounts, type = "n", xlab = "", ylab = "")     # first argument x, second y
text(locCounts, labels = locCounts[,3], cex = .8, srt = 45)
par(oldPar)
dev.off()


