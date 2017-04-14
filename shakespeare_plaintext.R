# Exploratory Text Analysis in R
# Workshop by Carolyn Moritz and Sarah Stanley
# shakespeare_plaintext.R by Sarah Stanley
# Last updated 2017/04/12

plays.v <- scan("shakespeare.txt", what = "character", sep="\n")

#TEXT PREP
# really hackily removing metadata. Carolyn Moritz has a better version 
plays.v <- plays.v[which(plays.v!="<<THIS ELECTRONIC VERSION OF THE COMPLETE WORKS OF WILLIAM")]
plays.v <- plays.v[which(plays.v!="SHAKESPEARE IS COPYRIGHT 1990-1993 BY WORLD LIBRARY, INC., AND IS")]
plays.v <- plays.v[which(plays.v!="PROVIDED BY PROJECT GUTENBERG ETEXT OF ILLINOIS BENEDICTINE COLLEGE")]
plays.v <- plays.v[which(plays.v!="WITH PERMISSION.  ELECTRONIC AND MACHINE READABLE COPIES MAY BE")]
plays.v <- plays.v[which(plays.v!="DISTRIBUTED SO LONG AS SUCH COPIES (1) ARE FOR YOUR OR OTHERS")]
plays.v <- plays.v[which(plays.v!="PERSONAL USE ONLY, AND (2) ARE NOT DISTRIBUTED OR USED")]
plays.v <- plays.v[which(plays.v!="COMMERCIALLY.  PROHIBITED COMMERCIAL DISTRIBUTION INCLUDES BY ANY")]
plays.v <- plays.v[which(plays.v!="SERVICE THAT CHARGES FOR DOWNLOAD TIME OR FOR MEMBERSHIP.>>")]

# define start of the plays
start.v <- which(plays.v=="1603")

# define beginning of the end metadata (end of plays)
end.v <- which(plays.v=="End of the Project Gutenberg EBook of The Complete Works of William")

# define the plays.v vector as plays.v object from start.v - end.v minus one line
plays.v <- plays.v[start.v:(end.v-1)]

# find the positions of lines with only four-digit years (marks the beginning of a new play)
play.positions.v <- grep("^\\d{4}", plays.v)

# create an extra line at the end of plays.v with the text "END". 
# sum total lines of all plays + end is the last position
# add last.position.v to play.positions.v
plays.lines.v <- c(plays.v, "END")
last.position.v <- length(plays.lines.v)
play.positions.v <- c(play.positions.v, last.position.v)

# create an empty list for output
play.word.l <- list()

# create a for loop that will separate out the plays
# use `if` to make the loop stop before trying to process the final position
for(i in 1:length(play.positions.v)){
  if(i != length(play.positions.v)){
    # create a play.title object that contains the title of the play (contained on line after year)
    play.title[i] <- plays.v[play.positions.v[i]+1]
    # create start and end objects for each play; each play goes from start to end
    start <- play.positions.v[i]+1
    end <- play.positions.v[i+1]-1
    play.lines.v <- plays.lines.v[start:end]
    # make the lines lower case and paste the lines into a single vector
    play.words.v <- tolower(paste(play.lines.v, collapse=" "))
    # create a list of all of the individual words and then unlist them
    play.words.l <- strsplit(play.words.v, "\\W")
    play.word.v <- unlist(play.words.l)
    # remove all of the blank values
    play.word.v <- play.word.v[which(play.word.v!="")]
    # output this as a list where each item on the list is a vector of each play's words
    play.word.l[[play.title[i]]] <- play.word.v
  }
}
# create an object called merchant.v 
# (which is the play.word.l vector with the title `THE MERCHANT OF VENICE`)
merchant.v <- play.word.l$`THE MERCHANT OF VENICE`
# create an object that is just all of the words "antonio" in merchant.v
antonios.v <- merchant.v[which(merchant.v == "antonio")]
length(antonios.v)

# NARRATIVE TIME
# a lot of this mirrors the process for breaking stuff out into plays; see comments above
merchant.lines.start <- which(plays.v=="THE MERCHANT OF VENICE")
merchant.lines.start
merchant.lines.end <- which(plays.v=="    So sore as keeping safe Nerissa's ring.               Exeunt")
merchant.lines.end
merchant.lines.v <- plays.v[merchant.lines.start:merchant.lines.end]
merchant.lines.start <- which(merchant.lines.v=="ACT I. SCENE I.")
merchant.lines.v <- merchant.lines.v[merchant.lines.start:merchant.lines.end]
scene.positions.v <- grep("SCENE V*I*V*", merchant.lines.v)
scene.positions.v
scene.nums.v <- merchant.lines.v[scene.positions.v]
scene.nums.v
merchant.lines.v <- c(merchant.lines.v, "END")
merchant.last.pos <- length(merchant.lines.v)
scene.positions.v <- c(scene.positions.v, merchant.last.pos)

scene.word.l <- list()
for(i in 1:length(scene.positions.v)){
  if(i!=length(scene.positions.v)){
    scene.number <- merchant.lines.v[scene.positions.v[i]]
    start.scene <- play.positions.v[i]+1
    end.scene <- play.positions.v[i+1]-1
    scene.lines.v <- merchant.lines.v[start.scene:end.scene]
    scene.words.v <- tolower(paste(scene.lines.v, collapse=" "))
    scene.words.l <- strsplit(scene.words.v, "\\W")
    scene.word.v <- unlist(scene.words.l)
    scene.word.v <- scene.word.v[which(scene.word.v!="")]
    scene.word.l[[scene.number]] <- scene.word.v
  }
}
scene.word.l$`ACT I. SCENE I.`[1:10]