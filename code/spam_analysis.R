#3.3 Reading the email Messages

#spamPath = system.file(package = "RSpamData")

spamPath = "C:/Users/tekanayake/Documents/R_projects/identifying_spam/data/"

list.dirs(spamPath, full.names = FALSE)

list.files(path = paste(spamPath, "messages",
                        sep = .Platform$file.sep))

head(list.files(path = paste(spamPath,"messages","easy_ham",
                             sep = .Platform$file.sep)))

head(list.files(path = paste(spamPath, "messages", "spam_2",
                             sep = .Platform$file.sep)))

dirNames = list.files(path = paste(spamPath, "messages",
                                   sep = .Platform$file.sep))

length(list.files(paste(spamPath, "messages", dirNames,
                        sep = .Platform$file.sep)))

sapply(paste(spamPath, "messages", dirNames,
             sep = .Platform$file.sep),
       function(dir) length(list.files(dir)) )

fullDirNames = paste(spamPath, "messages", dirNames,
                     sep = .Platform$file.sep)

fileNames = list.files(fullDirNames[1], full.names = TRUE)
fileNames[1]

msg = readLines(fileNames[1])
head(msg)

indx = c(1:5, 15, 27, 68, 69, 329, 404, 427, 516, 852, 971)
fn = list.files(fullDirNames[1], full.names = TRUE)[indx]
sampleEmail = sapply(fn, readLines)

#3.5 Finding the words in a message

msg = sampleEmail[[1]]
which(msg == "")[1]


splitPoint = match("", msg)

msg[ (splitPoint - 2):(splitPoint + 6) ]

header = msg[1:(splitPoint-1)]
body = msg[ -(1:splitPoint) ]

splitMessage = function(msg) {
  splitPoint = match("", msg)
  header = msg[1:(splitPoint-1)]
  body = msg[ -(1:splitPoint) ]
  return(list(header = header, body = body))
}

sampleSplit = lapply(sampleEmail, splitMessage)

#Removing attachments from the Message Body

header = sampleSplit[[1]]$header
grep("Content-Type", header)

grep("multi", tolower(header[[46]]))

header[46]

headerList = lapply(sampleSplit, function(msg) msg$header)
CTloc = sapply(headerList, grep, pattern = "Content-Type")
CTloc

sapply(headerList, function(header){
  CTloc = grep("Content-Type", header)
  if(length(CTloc) == 0) return(NA)
  CTloc
})

hasAttach = sapply(headerList, function(header){
  CTloc = grep("Content-Type", header)
  if(length(CTloc) == 0) return(FALSE)
  grepl("multi", tolower(header[CTloc]))
})

hasAttach


