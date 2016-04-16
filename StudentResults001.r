# start of script 
# load all required packages
#
library(MASS);library(nnet)    # required for car
library(car)                   # has recode function !
library(ggplot2)
library(grid)                  # required for multiplot function
library(reshape2)

# get Data
#
name <- c("Abe Adams","Beth Brown","Cath Cawley","Dave Dagg","Doug Douglas",
         "Edith Everton","Fay Fromberg","Gina Goeswell","Hillary Howell",
         "Ivy Indigo","John Jackson","Kim Kloher","Lyn Lovelock",
         "May Mayweather","Mike Mosley","Nellie Nova","Olga Osberg","Pat Price",
         "Paul Puruwai","Quin Quax","Ray Reardon","Steve Stew","Tina Tocker",
         "Tom  Tumu","Ursla Urin","Val Vowel","Wendy Wayne","Xero Xian",
         "Yue Yihue","Zoe Zhang")
gender <- c("Male","Female","Female","Male","Male","Female","Female","Female",
            "Female","Female","Male","Female","Female","Female","Male","Female",
            "Female","Male","Male","Male","Male","Male","Female","Male",
            "Female","Female","Female","Male","Male","Female")
quiz <- c(8,5,10,11,12,11,11,11,12,8,14,9,12,11,8,9,9,9,10,11,12,8,4,9,11,8,11,
          10,7,8)
assign <- c(26,23,26,27,23,0,29,26,31.5,0,34.5,26,27,26,23,29,23,22,28,29,32.5,
            0,25,29,28,21,27,16,14,23)
test <- c(26,28,31,36,30,24,43,37,42,24,40,28,33,35,30,30,28,29,48,31,45.5,3.5,
         25,46,31,32,35,32,32,0)
total <- c(60,56,67,74,65,35,83,74,86,32,88,63,72,72,61,68,60,60,86,71,90,11,
          54,84,70,61,73,58,53,31)
#
sdata01 <- data.frame(name,gender,quiz,assign,test,total)
rm (name,gender,quiz,assign,test,total)
#
#
# bash data into shape
#
sdata01$totb <- as.character(sdata01$total) # new column for recode
#View(sdata01)
#
sdata01$grade <- recode(sdata01$totb, "c(40,41,42,43,44)='D-'; 
c(45,46,47,48,49)='D'; c(50,51,52,53,54) ='C-'; c(55,56,57,58,59) ='C'; 
c(60,61,62,63,64) ='C+'; c(65,66,67,68,69) ='B-'; c(70,71,72,73,74) ='B'; 
c(75,76,77,78,79) ='B+'; c(80,81,82,83,84) ='A-'; c(85,86,87,88,89) ='A'; 
c(90,91,92,93,94,95,96,97,98,99) ='A+'; else = 'Fail'")
#View(sdata01)        
#
sdata01$assignper <- (sdata01$assign/35*100)  # convert score to % & new column
sdata01$testper <- (sdata01$test/50*100)    # convert score to % & new column
sdata01$quizper <- (sdata01$quiz/15*100)    # convert score to % & new column
q1 <- sdata01 
str(q1)
q1$assignper <- round(q1$assignper,digits=1) # round = 1 decimal place
q1$quizper <- round(q1$quizper,digits=1)
str(q1)
q2 <- q1[order(sdata01$total),] # order data
#
names(q2)
str(q2)
plotbar <- q2
#
# subset or select relevant three variables and then stack for boxplots
qs <- q2[9:11]  # select relevant columns
str(qs)
qp <- stack(qs)  # stacks variables on top
str(qp)
qs <- qp
colnames(qs) <- c("score", "task") # add column names
str(qs)
plotbox <- qs
#
# subset again for stacked bar graph
qt <- q2[1:4]
#
qu <- melt(qt, id.vars=c('name'),var='task')  # stack data required for stacked bar graphs
plotstbar <- qu
#
rm (q1,q2,qp,qs,qt,qu)   # remove objects no longer needed
#
# Now do plots
# load required functions - in this case multiplot -which requires grid
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
#
#
# vertical bar of quiz (no good)
ggplot(plotbar, aes(x=name, y=quiz)) + geom_bar(stat="identity", fill="blue",size=1.5) + theme_bw(base_size = 24)
#
# horizontal bar of quiz (better)
ggplot(plotbar, aes(x=name,y=quiz)) + geom_bar(stat="identity",fill="blue",size=1.5) + theme_bw(base_size = 24) + coord_flip()

# horizontal bar of quiz (ordered, better still)
plotb01 <- ggplot(plotbar, aes(x=reorder(name,quiz),y=quiz)) + geom_bar(stat="identity",fill="blue",size=1.5) + theme_bw(base_size = 24) + coord_flip() + ylim(0,15) 
plotb01 + theme(axis.title.y=element_blank())
#
# horizontal line of quiz 
ggplot(plotbar, aes(x=reorder(name,quiz), y=quiz, group=quiz)) + geom_line(size=1.5) + theme_bw(base_size = 24)+ coord_flip() + ylim(0,15) 
#
ggplot(plotbar, aes(x=reorder(name,quiz), y=quiz, group=quiz)) + geom_point(size=1.5) + theme_bw(base_size = 24)+ coord_flip() + ylim(0,15) 
#
# vertical bar of 3 tasks of top of each other
ggplot(plotbox, aes(score, fill=task)) + geom_bar(binwidth=3.3)+ theme_bw(base_size = 24) + xlim(0,100) 
#
# vertical bar of final grade unordered
ggplot(plotbar, aes(x=grade)) + geom_bar(fill="blue",size=1.5) + theme_bw(base_size = 24)
# vertical bar of final grade ordered
plotb20 <- ggplot(plotbar, aes(x=grade)) + geom_bar(fill="blue",size=1.5) + theme_bw(base_size = 24) 
plotb20 + scale_x_discrete(limits=c("Fail","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))
#
#
# Cleveland dot plot of final grades
plotc30 <- ggplot(plotbar,aes(x=grade,y=reorder(name,total))) + geom_point(size=3) + theme_bw(base_size = 24)
plotc31 <- plotc30 + scale_x_discrete(limits=c("Fail","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))
plotc31  + theme(axis.title.y=element_blank())
#
# Cleveland dot plot of final grades with dotted lines
plotc40 <- ggplot(plotbar,aes(x=grade,y=reorder(name,total))) + geom_point(size=3) + theme_bw(base_size = 20)
plotc41 <- plotc40 +scale_x_discrete(limits=c("Fail","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))
plotc41 + theme(axis.title.y=element_blank()) +
     theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(colour="grey60",linetype="dashed"))


# Cleveland dot plot of final grades with dotted lines, but one label only
plotc50 <- ggplot(plotbar,aes(x=grade,y=reorder(nameB,total))) + geom_point(size=3) + theme_bw(base_size = 20) 
plotc51 <- plotc50 + scale_x_discrete(limits=c("Fail","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))
plotc51 + theme(axis.title.y=element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60",linetype="dashed"))


# boxplots of 3 tasks unordered
ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4) + theme_bw(base_size = 24) + ylim(0,100) + ylab("score (%)") + scale_fill_manual(values=c("red","green","blue"))
#
# boxplots of 3 tasks - get order of tasks right
plotbx50 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
plotbx50  + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_manual(values=c("red","green","blue"))+ xlim("quizper","assignper","testper")
#
# boxplots of 3 tasks - ordered get rid of task labels
plotbx51 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
plotbx51  + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_manual(values=c("red","green","blue"), guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") 
#
# boxplots of 3 tasks - ordered get rid of task labels, blue palette
plotbx511 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
plotbx511  + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_brewer(palette="Blues", guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") 
#
# boxplots of 3 tasks - ordered get rid of task labels, blue palette
plotbx512 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
plotbx512  + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_brewer(guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") 
#
# boxplots of 3 tasks - ordered get rid of task labels, blue palette
plotbx513 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4) + coord_flip()
plotbx513  + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_brewer(guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") 
#
#
# boxplots of 3 tasks - ordered get rid of task labels, no colour fills
plotbx52 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
plotbx52  + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_manual(values=c("white","white","white"), guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") 

# boxplots of 3 tasks - no task labels, and means as diamonds
plotbx53 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
plotbx53 + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_manual(values=c("white","white","white"), guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") +
stat_summary(fun.y = "mean", geom ="point", shape = 23, size = 4, fill ="blue")

# boxplots of 3 tasks - ordered get rid of task labels, no colour fills + text
plotbx54 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
qq <- plotbx54 + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_manual(values=c("white","white","white"), guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") 
qp <- qq + annotate("text", x =Inf, hjust=31.15, y =80, label="*", size = 14.6, colour ="blue")
qr <- qp + annotate("text", x =Inf, hjust=19.3, y =77, label ="*", size = 14.6, colour ="blue")
qr +       annotate("text", x =Inf, hjust=7.55, y=60, label ="*", size = 14.6, colour ="blue")
#

#

# Line density plot (filled) of tasks
ggplot(plotbox, aes(x=score,fill=task)) + geom_density() + geom_density(alpha=.3) + theme_bw(base_size = 24)+ xlim(0,100)
#
# Line density plot (unfilled) of tasks
ggplot(plotbox, aes(x=score,colour=task)) + geom_density() + geom_density(alpha=.3) + theme_bw(base_size = 24)+ xlim(0,100)
#
plotstbar01 <- ggplot(plotstbar, aes(x=name, y=value, fill=task))+ theme_bw(base_size = 24)
plotstbar01 + geom_bar(stat="identity")+ coord_flip() + theme(axis.title.y=element_blank())
#
plotstbar02 <- ggplot(plotstbar, aes(x=reorder(name, value), y=value, fill=task))+ theme_bw(base_size = 24)
plotstbar02 + geom_bar(stat="identity")+ coord_flip() + theme(axis.title.y=element_blank()) + ylab("score (%)") 
#
p5a  <- ggplot(plotstbar, aes(x=reorder(name, value), y=value, fill=task))+ theme_bw(base_size = 24)
p5   <- p5a  + geom_bar(stat="identity")+ coord_flip() + theme(axis.title.y=element_blank()) + ylab("score (%)") 

#
p1 <- plotc41 + theme(axis.title.y=element_blank()) + theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(), panel.grid.major.y
           = element_line(colour="grey60",linetype="dashed"))

p2 <- ggplot(plotbox, aes(x=score,colour=task)) +geom_density()+geom_density(alpha=.3) + theme_bw(base_size = 24)+ xlim(0,100)+ xlab("score (%)") 

p3 <- ggplot(plotbox, aes(score, fill=task)) + geom_bar(binwidth=3.3)+ theme_bw(base_size = 24) + xlim(0,100) + xlab("score (%)") 

p4 <- ggplot(plotbox, aes(x=task,y=score, fill=task)) +geom_boxplot(binwidth=4) + theme_bw(base_size = 24) + ylim(0,100) + ylab("score (%)") + scale_fill_manual(values=c("red","green","blue"), guide=FALSE)+ xlim("quizper","assignper","testper")


p5 <- ggplot(plotbar,aes(x=grade,y=reorder(nameB,total))) + geom_point(size=3) + theme_bw(base_size = 20) 
p5a <- p5 + scale_x_discrete(limits=c("Fail","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))
p5b <- p5a + theme(axis.title.y=element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60",linetype="dashed"))

p6 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
p6a <- p6 + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_manual(values=c("white","white","white"), guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") 
p6b <- p6a + annotate("text", x =Inf, hjust=30.15, y =80, label="*", size = 14.6, colour ="blue")
p6c <- p6b + annotate("text", x =Inf, hjust=16.3, y =77, label ="*", size = 14.6, colour ="blue")
p6d <- p6c + annotate("text", x =Inf, hjust=8.25, y=60, label ="*", size = 14.6, colour ="blue")


p7 <- ggplot(plotbox, aes(x=task, y=score, fill=task)) + geom_boxplot(binwidth=4)
p7a <- p7 + theme_bw(base_size = 24) + ylim(0,100) + scale_fill_brewer(guide=FALSE) + xlim("quizper","assignper","testper") + ylab("score (%)") 

p8 <- ggplot(plotstbar, aes(x=reorder(name, value), y=value, fill=task))+ theme_bw(base_size = 24)
p8b <- p8 + geom_bar(stat="identity")+ coord_flip() + theme(axis.title.y=element_blank()) + ylab("score (%)") 


# play with layouts
#
multiplot (p1, p2, p3, p4, cols=2)
multiplot (p1, p3,cols=2)
#multiplot (p1, p3,cols=1)
multiplot (p1, p2, p3, cols=2)
#multiplot (p1, p2,cols=1)
multiplot (p1, p4, cols=2) # 2nd best solution
multiplot (p1, p7a, cols=2) # best solution
multiplot (p5b, p6d, cols=2)
multiplot (p1, p8b, cols=2) # best solution
#
#
#
# end of script  #############################

