#make object
BarLength <- 7
datafile <- "USDJPY1-DATA.scsv"
  
Currency <- strsplit(datafile,"-")[[1]][1]
require(ggplot2)
MartingaleTest <- function(StartIndex=0,TradingSequence=c("Bu","Bu","Bu","Bu"),Initial=1,MaximumBet=30,ProfitPercent=.7){
  if(!is.character(TradingSequence)) stop("TradingSequence must be vector of strings \"Bu\" or \"Be\"")
  
  self <- list(
    TradingSequence=TradingSequence,
    MaxLength=length(TradingSequence),
    CurrentSequence=NULL,
    CurrentBet=Initial,
    BetSequenceIndex=1,
    StartIndex=StartIndex,
    Initial=Initial,
    MaximumBet=MaximumBet,
    ProfitPercent=ProfitPercent,
    Profit=0,
    ProfitHistory=c(),
    LastMartingale=FALSE,
    LifeSpan=0, #how many bars has it gone through without dying
    id=toString(c(StartIndex,MaximumBet,Initial,TradingSequence)),
    TurnedOver=FALSE, #has reached it reached the MaximumBet $ before it's death sequence
    Dead=FALSE,
    DeathSequence = {#wrong
      Bu_indices <- which(TradingSequence=="Bu")
      Be_indices <- which(TradingSequence=="Be")
      DeathSequence <- replace(TradingSequence,Bu_indices,"Be")
      DeathSequence <- replace(DeathSequence,Be_indices,"Bu")
    }
  )


  self$new_bar <- function(Bu_Be_OR_D){
    self$Check()
    switch(Bu_Be_OR_D,
    "Bu"={
      self$ChangeCurrent("Bu")
    },
    "Be"={
      self$ChangeCurrent("Be")
    }, 
    "D"={
   #   print("D: Doji: I do nothing")
    },
    stop("Please enter \"Bu\" \"Be\" or \"D\"")
    )
    self$LifeSpan <- self$LifeSpan+1
  }#check
  self$ChangeCurrent <- function(Bu_or_Bear){

    if(length(self$CurrentSequence)<self$MaxLength)
    {
      self$CurrentSequence <- append(self$CurrentSequence,Bu_or_Bear)
      #check if bar was for or agianst us
      if(self$CurrentSequence[self$BetSequenceIndex]==self$TradingSequence[self$BetSequenceIndex])
      {#if the current bar was the wrong one 
      # cat("This Bar is good. (Reset)")
        self$Profit <- self$Profit+(self$CurrentBet*self$ProfitPercent)
        self$ProfitHistory <- append(self$ProfitHistory,self$Profit)
        self$MartingaleCount[which(self$BetArray==self$CurrentBet)] <-self$MartingaleCount[which(self$BetArray==self$CurrentBet)] +1
        self$Reset()
      }
      else
      {
     #   print("I don't like this Bar I must increase bet")
        self$Profit <- round(self$Profit-self$CurrentBet,2)
        self$ProfitHistory <- append(self$ProfitHistory,self$Profit) 
   #     print(paste(self$CurrentBet,"removed"))
     
        self$IncrementMartingale()
        }
    }
    else
    {
        if(all(self$CurrentSequence==self$DeathSequence))
        {
          #cat("Martingale Stratinging at",self$StartIndex,"Has Died")
          self$Dead <- TRUE # Rest in Peace 
          
        }
      else
        {#if the current sequence has gone thorugh all them and not died restart
         self$Reset()
        }
    }
   #self$MaxLength
  }#check
  self$Reset <- function(){
    self$CurrentSequence <-NULL 
    self$CurrentBet <- Initial
    self$BetSequenceIndex<-1
    #print("RESETTING")
  }#check
  self$Check <- function(){
    if(self$Dead) stop("This Martingale is Dead, stop trying to use it")
  }#check
  self$IncrementMartingale <- function(){
    if(self$BetArray[length(self$BetArray)-1]>self$CurrentBet) #if it is not more than the last bet in the sequence already
      {    
      self$CurrentBet <- self$BetArray[which(self$BetArray==self$CurrentBet)+1] #get next value in BetArray
     # cat("Now betting",self$CurrentBet)
     
      self$MartingaleCount[which(self$BetArray==self$CurrentBet)] <-self$MartingaleCount[which(self$BetArray==self$CurrentBet)] +1
       }
    else if (self$BetArray[length(self$BetArray)-1]==self$CurrentBet)
    {
      self$CurrentBet <- self$BetArray[which(self$BetArray==self$CurrentBet)+1] #get next value in BetArray
      #cat("Now betting",self$CurrentBet," {Last One}")
     self$LastMartingale=TRUE
      
      self$MartingaleCount[which(self$BetArray==self$CurrentBet)] <-self$MartingaleCount[which(self$BetArray==self$CurrentBet)] +1
      
    }
    else
      {
      
   #   print("SELF DESTRUCT THE PREVIOUS BET WAS THE LAST ONE")
      self$Dead=TRUE
      } 
    self$BetSequenceIndex<-self$BetSequenceIndex+1
  }#check

  self$BetArray <- {
    BetArray <- Initial   
    #practilaly infinite
    for (index in 1:100)
    {
      NewVal <-
        round(BetArray[index] + (BetArray[index] / self$ProfitPercent), 2)
      if (NewVal >= self$MaximumBet)
        break()
      BetArray <- append(BetArray, NewVal)
    }
    self$BetArray <-BetArray
  }
  #make a table and turn everything into 0s
  self$MartingaleCount <- sapply(table(c(1:length(self$BetArray))),function(x) 0) 

    
  self <- list2env(self)
  class(self)<-"MartingaleTest"
  return(self)
}



##--Do work
PriceResults <-read.csv2(
  paste("/home/khalfani/.wine/drive_c/Program Files (x86)/SubVector Metatrader/MQL4/Files/",datafile,sep="")
)#-DATA means there is no weekends in it
#head(PriceResults)
#              Time      Open    High     Low   Close   BollHigh  BollMid   BollLow
#89 2016.01.01 04:28:00 0.72947 0.72947 0.72935 0.72943 0.7295312 0.7293560 0.7291808
#90 2016.01.01 04:29:00 0.72942 0.72948 0.72941 0.72948 0.7295378 0.7293580 0.7291782
#91 2016.01.01 04:30:00 0.72948 0.72952 0.72948 0.72952 0.7295592 0.7293660 0.7291728
#92 2016.01.01 04:31:00 0.72951 0.72952 0.72949 0.72952 0.7295790 0.7293750 0.7291710
#93 2016.01.01 04:32:00 0.72952 0.72959 0.72952 0.72955 0.7296020 0.7293875 0.7291730
#94 2016.01.01 04:33:00 0.72956 0.72962 0.72955 0.72955 0.7296203 0.7293945 0.7291687
#95 2016.01.01 04:34:00 0.72955 0.72956 0.72948 0.72955 0.7296385 0.7294065 0.7291745

PriceResults<- na.omit(PriceResults)
PriceResults<- PriceResults[1:3000,]
for(i in names(PriceResults)){
  if(i!="Time")
  {#Convert to Numeric
    PriceResults[[i]] <- as.numeric(as.character(PriceResults[[i]]))
    #convert to character or as.numeric will forece it to numbers by index
  }
  
}

numbers_left_of_decimal_place <- strsplit(as.character(PriceResults$Close[1]),"\\.")[[1]][2]
nchar(numbers_left_of_decimal_place)

smallest_increment <-as.numeric(paste("1e-0",nchar(numbers_left_of_decimal_place),sep = ""))

Bu_Be_D <- ifelse(abs(PriceResults$Close-PriceResults$Open)==0,
                  "D",
                  ifelse((PriceResults$Close>PriceResults$Open),"Bu","Be")
)
Martin <-list()

library(gtools)

allperms<-gtools::permutations(n=2,r=BarLength,v=c("Bu","Be"),repeats=TRUE)
for (i in 1:nrow(allperms))
{
  Martin[[i]] <- MartingaleTest(TradingSequence=allperms[i,],MaximumBet = 250,StartIndex = 300)
}

for(i in Martin[[1]]$StartIndex:length(Bu_Be_D))
{
  for(x in 1:length(Martin)) 
  {
    if(!Martin[[x]]$Dead) try(Martin[[x]]$new_bar(Bu_Be_D[i]),silent = TRUE)
  } 
 
}
TradingSequences <- sapply(Martin,function(x) toString(get("TradingSequence",x)))
ProfitHistory <-sapply(Martin,function(x) get("ProfitHistory",x))
names(ProfitHistory) <- "TradingSequence"
Profits <- Sequences <- Age<- NULL
for( i in 1:length(ProfitHistory))
{
  #adjust the lenghths
  ProfitHistory[[i]]<-append(ProfitHistory[[i]],rep(NA,max(sapply(ProfitHistory,length))-length(ProfitHistory[[i]]))  ) 
  Profits <- append(Profits,ProfitHistory[[i]]) 
  Sequences <- append(Sequences,rep(TradingSequences[i],length(ProfitHistory[[i]]))) 
  Age <- append(Age,1:length(ProfitHistory[[i]])) 
} 
cbind( ProfitHistory)
Results <-data.frame(
  Age=Age,
  Profits=Profits,
  Sequence=Sequences)

Results <- na.omit(Results)

#--------------------------------------------

Best <- Results$Sequence[which.max(Results$Profits)]
Worst<-Results$Sequence[which.min(Results$Profits)]
Graph <-qplot(y=Profits,Age,data=Results, geom = "line",color=Sequence) + labs(title = paste(Currency,": Life Span of a Martingale ",BarLength,sep=""))
BestGraph <-function() plot(Results$Profits[which(Results$Sequence==Best)])
WorstGraph <-function() plot(Results$Profits[which(Results$Sequence==Worst)])
Graph