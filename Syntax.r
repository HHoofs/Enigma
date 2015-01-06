require(stringr)
require(gridExtra)

RotorsReflectors <- function(name,newL=NULL){
  #Rotor 1
  I     <- list("EKMFLGDQVZNTOWYHXUSPAIBRCJ","Q")
  names(I) <- c("Enc","Step")
  
  #Rotor 2
  II    <- list("AJDKSIRUXBLHWTMCQGZNPYFVOE","E")
  names(II) <- c("Enc","Step")
  
  #Rotor 3
  III   <- list("BDFHJLCPRTXVZNYEIWGAKMUSQO","V")
  names(III) <- c("Enc","Step")
  
  #Rotor 4
  IV    <- list("ESOVPZJAYQUIRHXLNFTGKDCMWB","J")
  names(IV) <- c("Enc","Step")
  
  #Rotor 5
  V     <- list("VZBRGITYUPSDNHLXAWMJQOFECK","Z")
  names(V) <- c("Enc","Step")
  
  #Rotor 6
  VI    <- list("JPGVOUMFYQBENHZRDKASXLICTW",c("Z","M"))
  names(V) <- c("Enc","Step")
  
  #Rotor 7
  VII   <- list("NZJHGRCXMYSWBOUFAIVLPEKQDT",c("Z","M"))
  names(VII) <- c("Enc","Step")
  
  #Rotor 8
  VIII  <- list("FKQHTLXOCBJSPDZRAMEWNIUYGV",c("Z","M"))
  names(VIII) <- c("Enc","Step")
  
  #Rotor Beta
  Beta  <- list("LEYJVCNIXWPBQMDRTAKZGFUHOS",NULL)
  names(Beta) <- c("Enc","Step")
  
  #Rotor Gamma
  Gamma <- list("FSOKANUERHMBTIYCWLQPZXVGJD",NULL)
  names(Gamma) <- c("Enc","Step")
  
  RefB  <- list("YRUHQSLDPXNGOKMIEBFZCWVJAT",NULL)
  names(RefB) <- "Enc"
  
  RefC  <- list("FVPJIAOYEDRZXWGCTKUQSBNMHL",NULL)
  names(RefC) <- "Enc"
  
  RefBt <- list("ENKQAUYWJICOPBLMDXZVFTHRGS",NULL)
  names(RefBt) <- "Enc"
  
  RefCt <- list("RDOBJNTKVEHMLFCWZAXGYIPSUQ",NULL)
  names(RefCt) <- "Enc"
  
  RotRef <- list(I,II,III,IV,V,VI,VII,VIII,Beta,Gamma,RefB,RefC,RefBt,RefCt)
  names(RotRef) <- c("I","II","III","IV","V","VI","VII","VIII","Beta","Gamma","RefB","RefC","RefBt","RefCt")
  
  if(!is.null(newL)){
    NewR <- list()
    for(i in 1:length(newL)){
      New <- list(newL[[i]][[1]],newL[[i]][[2]])
      names(New) <- c("Enc","Step")
      NewR[[i]] <- New
      names(NewR)[i] <- names(newL)[i]
    }
    RotRef <- c(RotRef,NewR)
  }
  return(RotRef[[name]])  
}

C26 <- function(x){
  if(x < 1) x <- 26 + x
  if(x >26) x <- x - 26
  return(x)
}

EnigmaExtF <- function(InpL=NULL,OutL=NULL,RotorPos){
  KeyBoard <- 
    data.frame(Symb = c("Q","W","E","R","T","Z","U","I","O",
                        "A","S","D","F","G","H","J","K",
                        "P","Y","X","C","V","B","N","M","L"),
               X = c(seq(from = 1,by = 2, length.out = 9),
                     seq(from = 2,by = 2, length.out = 8),
                     seq(from = 1,by = 2, length.out = 9)
               ),
               Y = c(3,3,3,3,3,3,3,3,3,
                     2,2,2,2,2,2,2,2,
                     1,1,1,1,1,1,1,1,1),
               stringsAsFactors = FALSE              
    )
  
  pushViewport(viewport(layout = grid.layout(nrow=7, ncol=17, 
                                             widths  = unit(c(rep(1,17)), "null"),
                                             heights = unit(c(1, 1, 1, 1, 1, 1, 1), "null")
  )))
  
  grid.rect(gp=gpar(fill="grey"))
  grid.rect(gp=gpar(fill ="light grey"),vp = viewport(layout.pos.row = 1, layout.pos.col = 4))
  grid.rect(gp=gpar(fill ="light grey"),vp = viewport(layout.pos.row = 1, layout.pos.col = 6))
  grid.rect(gp=gpar(fill ="light grey"),vp = viewport(layout.pos.row = 1, layout.pos.col = 8))
  
  grid.text(RotorPos[1], vp = viewport(layout.pos.row = 1, layout.pos.col = 4), gp=gpar(col="black", fontsize=12)) 
  grid.text(RotorPos[2], vp = viewport(layout.pos.row = 1, layout.pos.col = 6), gp=gpar(col="black", fontsize=12)) 
  grid.text(RotorPos[3], vp = viewport(layout.pos.row = 1, layout.pos.col = 8), gp=gpar(col="black", fontsize=12)) 
  
  for(i in 1:nrow(KeyBoard)){
    grid.circle(r= .25, gp=gpar(col="white", fill="black"), vp = viewport(layout.pos.row = KeyBoard[i,"Y"] + 4, layout.pos.col = KeyBoard[i,"X"]))
    grid.circle(r= .25, gp=gpar(col="grey", fill="darkgray"), vp = viewport(layout.pos.row = KeyBoard[i,"Y"] + 1, layout.pos.col = KeyBoard[i,"X"]))
    grid.text(KeyBoard[i,"Symb"], vp = viewport(layout.pos.row = KeyBoard[i,"Y"] + 4, layout.pos.col = KeyBoard[i,"X"]), gp=gpar(col="white", fontsize=12)) 
    grid.text(KeyBoard[i,"Symb"], vp = viewport(layout.pos.row = KeyBoard[i,"Y"] + 1, layout.pos.col = KeyBoard[i,"X"]), gp=gpar(col="black", fontsize=12))   
  }

  if(!is.null(InpL)){
  grid.circle(r= .25, gp=gpar(col="white", fill="pink"), vp = viewport(layout.pos.row = KeyBoard[KeyBoard$Symb == InpL,"Y"] + 4, layout.pos.col = KeyBoard[KeyBoard$Symb == InpL,"X"]))
  grid.text(KeyBoard[KeyBoard$Symb == InpL,"Symb"], vp = viewport(layout.pos.row = KeyBoard[KeyBoard$Symb == InpL,"Y"] + 4, layout.pos.col = KeyBoard[KeyBoard$Symb == InpL,"X"]), gp=gpar(col="black", fontsize=12))
  } 
  
  if(!is.null(OutL)){
  grid.circle(r= .25, gp=gpar(col="grey", fill="yellow"), vp = viewport(layout.pos.row = KeyBoard[KeyBoard$Symb == OutL,"Y"] + 1, layout.pos.col = KeyBoard[KeyBoard$Symb == OutL,"X"]))
  grid.text(KeyBoard[KeyBoard$Symb == OutL,"Symb"], vp = viewport(layout.pos.row = KeyBoard[KeyBoard$Symb == OutL,"Y"] + 1, layout.pos.col = KeyBoard[KeyBoard$Symb == OutL,"X"]), gp=gpar(col="black", fontsize=12))
  }
}

Enigma <- function(Rotors = c("III","II","I"), RingSetting = c(1,1,1), RotorPos = c("A","A","A"), Reflector = "RefB", Plugboard = NULL, Text = "HENK", Figure = FALSE, Helper = NULL, NewRotRef = NULL){
  
  if(!any(Rotors[length(Rotors)] == c("I","II","III","IV","V","VI","VII","VIII","Beta","Gamma",names(NewRotRef)))) stop("Non existing name for Rotor 1")
  if(!any(Rotors[length(Rotors) - 1] == c("I","II","III","IV","V","VI","VII","VIII","Beta","Gamma",names(NewRotRef)))) stop("Non existing name for Rotor 2")
  if(!any(Rotors[length(Rotors) - 2] == c("I","II","III","IV","V","VI","VII","VIII","Beta","Gamma",names(NewRotRef)))) stop("Non existing name for Rotor 3")
  if(any(Rotors[length(Rotors)] == c("Beta","Gamma"))) warning("The Beta and/or Gamma rotor could only placed as fourth rotor")
  if(any(Rotors[length(Rotors) - 1] == c("Beta","Gamma"))) warning("The Beta and/or Gamma rotor could only placed as fourth rotor")
  if(any(Rotors[length(Rotors) - 2] == c("Beta","Gamma"))) warning("The Beta and/or Gamma rotor could only placed as fourth rotor")
  if(length(Rotors) == 4) if(!any(Rotors[length(Rotors) - 3] == c("I","II","III","IV","V","VI","VII","VIII","Beta","Gamma",names(NewRotRef)))) stop("Non existing name for Rotor 4")
  if(length(Rotors) == 4) if(any(Rotors[length(Rotors) - 3] == c("I","II","III","IV","V","VI","VII","VIII"))) warning("Only the Beta and Gamma rotor could be placed as fourth Rotor")
  if(!any(Reflector == c("RefB","RefC","RefBt","RefCt",names(NewRotRef)))) stop("Non existing name for the Reflector")
  if(length(Rotors) > 4 | length(Rotors) < 3) stop("Not enough/too many Rotors")
  if(length(RingSetting) > 4 | length(RingSetting) < 3) stop("Not enough/too many Ring Settings")
  if(length(RotorPos) > 4 | length(RotorPos) < 3) stop("Not enough/too many Rotor Positions")
  if(length(Reflector) > 1) stop("You can only have one rotor")
  if(length(Rotors) != length(RingSetting) | length(Rotors) != length(RotorPos) | length(RingSetting) != length(RotorPos)) stop("Length of Rotors/Ring Settings/Rotor Positions do not match")
  if(!is.null(Plugboard) & any(nchar(Plugboard) != 2)) stop("One (or more) of the Plugboard pairs consists of too many/not enough letters")
  if(any(duplicated(Rotors))) warning("You used the same rotor at least twice")
    
  countCharOccurrences <- function(char, s) {
    s2 <- gsub(char,"",s)
    return (nchar(s) - nchar(s2))
  }
  
  for(L in 1:26) if(sum(countCharOccurrences(LETTERS[L], Plugboard))>1) stop("A certain Letter occurs more than one time in the Plugboard settings")
  if(length(Plugboard) > 10) warning("You provide more than 10 Plugboard pairs (which is the number of cables provided for each Enigma machine)")
  
  Rotor1 <- strsplit(RotorsReflectors(Rotors[[length(Rotors)]], newL = NewRotRef)$Enc, "")[[1]]
  Rotor1 <- cbind(Rotor1,LETTERS)
  Rotor1T<- RotorsReflectors(Rotors[[length(Rotors)]])$Step
  
  Rotor2 <- strsplit(RotorsReflectors(Rotors[[length(Rotors) - 1]], newL = NewRotRef)$Enc, "")[[1]]
  Rotor2 <- cbind(Rotor2,LETTERS)
  Rotor2T<- RotorsReflectors(Rotors[[length(Rotors) - 1]], newL = NewRotRef)$Step
  
  Rotor3 <- strsplit(RotorsReflectors(Rotors[[length(Rotors) - 2]], newL = NewRotRef)$Enc, "")[[1]]
  Rotor3 <- cbind(Rotor3,LETTERS)
  Rotor3T<- RotorsReflectors(Rotors[[length(Rotors) - 2]], newL = NewRotRef)$Step
  
  
  if(length(Rotors) == 4){
    Rotor4 <- strsplit(RotorsReflectors(Rotors[[length(Rotors) - 3]], newL = NewRotRef)$Enc, "")[[1]]
    Rotor4 <- cbind(Rotor4,LETTERS)
  }
  
  if(is.numeric(RingSetting)){
    RingSetting1 <- RingSetting[length(RingSetting)]
    RingSetting2 <- RingSetting[length(RingSetting)-1]
    RingSetting3 <- RingSetting[length(RingSetting)-2]
    if(length(RingSetting) == 4) RingSetting4 <- RingSetting[length(RingSetting)-3]
  }
  
  if(is.character(RingSetting)){
    RingSetting1 <- which(LETTERS == RingSetting[length(RingSetting)])
    RingSetting2 <- which(LETTERS == RingSetting[length(RingSetting)-1])
    RingSetting3 <- which(LETTERS == RingSetting[length(RingSetting)-2])
    if(length(RingSetting) == 4) RingSetting4 <- which(LETTERS == RingSetting[length(RingSetting)-3])
  }
  
  Reflec <- strsplit(RotorsReflectors(Reflector, newL = NewRotRef)$Enc, "")[[1]]
  Reflec <- cbind(Reflec,LETTERS)
  
  Plugb <- cbind(LETTERS,LETTERS)
  if(!is.null(Plugboard)){
    for(s in 1:length(Plugboard)){
      SwapL <- strsplit(Plugboard[s],"")[[1]]
      Plugb[Plugb[,1]==SwapL[1],2] <- SwapL[2]
      Plugb[Plugb[,1]==SwapL[2],2] <- SwapL[1]
    }
  }
  
  if(is.character(RotorPos)){
    RotorPos1 <- which(LETTERS == RotorPos[length(RingSetting)])
    RotorPos2 <- which(LETTERS == RotorPos[length(RotorPos) - 1])
    RotorPos3 <- which(LETTERS == RotorPos[length(RotorPos) - 2])
    if(length(RotorPos) == 4)  RotorPos4 <- which(LETTERS == RotorPos[length(RotorPos) - 3])
  }
  if(is.numeric(RotorPos)){
    RotorPos1 <- RotorPos[length(RingSetting)]
    RotorPos2 <- RotorPos[length(RotorPos) - 1]
    RotorPos3 <- RotorPos[length(RotorPos) - 2]
    if(length(RotorPos) == 4)  RotorPos4 <- RotorPos[length(RotorPos) - 3]
  }
  
  StartSettings <- list(Rotors = Rotors,                        
                        RingSetting = list(Letters = c(LETTERS[RingSetting3],LETTERS[RingSetting2],LETTERS[RingSetting1]),
                                           Numeric = c(RingSetting3,RingSetting2,RingSetting1)),
                        RotorPos = list(Letters = c(LETTERS[RotorPos3],LETTERS[RotorPos2],LETTERS[RotorPos1]),
                                        Numeric = c(RotorPos3,RotorPos2,RotorPos1))
                        )
                        
  
  TextOut <- vector()
  
  if(is.null(Text)){
    TextInp <- readline("")
    Entry <- 1
    TextIn <- vector()
    while(any(TextInp == LETTERS)){
      RotorPos1 <- C26(RotorPos1 + 1)
      
      if(any(LETTERS[C26(RotorPos1 - 1)] == Rotor1T)) RotorPos2 <- C26(RotorPos2 + 1)
      
      if(any(LETTERS[C26(RotorPos1 - 2)] == Rotor1T) & 
           any(LETTERS[C26(RotorPos2)] == Rotor2T)){
        RotorPos3 <- C26(RotorPos3 + 1)
        RotorPos2 <- C26(RotorPos2 + 1)
      }  
      
      if(length(RotorPos) == 4) c(RotorPos4,RotorPos3,RotorPos2,RotorPos1)
      if(length(RotorPos) == 3) c(RotorPos3,RotorPos2,RotorPos1)
      
      inpL <- TextInp
      inpL <- Plugb[Plugb[,1] == inpL,2]
      
      inp1 <- C26(which(LETTERS == inpL) + (RotorPos1 - 1) - (RingSetting1 - 1))
      out1 <- Rotor1[Rotor1[,2] == LETTERS[inp1], 1]
      out1 <- C26(which(LETTERS == out1) - (RotorPos1 - 1) + (RingSetting1 - 1))
      
      inp2 <- LETTERS[C26(out1 + (RotorPos2 - 1) - (RingSetting2 - 1))]
      out2 <- Rotor2[Rotor2[,2] == inp2, 1]
      out2 <- C26(which(LETTERS == out2) - (RotorPos2 - 1) + (RingSetting2 - 1))
      
      inp3 <- LETTERS[C26(out2 + (RotorPos3 - 1) - (RingSetting3 - 1))]
      out3 <- Rotor3[Rotor3[,2] == inp3, 1]
      out3 <- C26(which(LETTERS == out3) - (RotorPos3 - 1) + (RingSetting3 - 1))
      
      if(length(RotorPos) == 4){
        inp4 <- LETTERS[C26(out3 + (RotorPos4 - 1) - (RingSetting4 - 1))]
        out4 <- Rotor4[Rotor4[,2] == inp4, 1]
        out4 <- C26(which(LETTERS == out4) - (RotorPos4 - 1) + (RingSetting4 - 1))
        
        outR <- Reflec[Reflec[,2] == LETTERS[out4], 1]
        outR <- which(LETTERS == outR)
        
        inp4 <- LETTERS[C26(outR + (RotorPos4 - 1))]
        out4 <- Rotor4[Rotor4[,1] == inp4, 2]
        outR <- C26(which(LETTERS == out4) - (RotorPos4 - 1) + (RingSetting4 - 1))
      } else {
        outR <- Reflec[Reflec[,2] == LETTERS[out3], 1]
        outR <- which(LETTERS == outR)
      }
      
      inp3 <- LETTERS[C26(outR + (RotorPos3 - 1) - (RingSetting3 - 1))]
      out3 <- Rotor3[Rotor3[,1] == inp3, 2]
      out3 <- C26(which(LETTERS == out3) - (RotorPos3 - 1) + (RingSetting3 - 1))
      
      inp2 <- LETTERS[C26(out3 + (RotorPos2 - 1) - (RingSetting2 - 1))]
      out2 <- Rotor2[Rotor2[,1] == inp2, 2]
      out2 <- C26(which(LETTERS == out2) - (RotorPos2 - 1) + (RingSetting2 - 1))
      
      inp1 <- LETTERS[C26(out2 + (RotorPos1 - 1) - (RingSetting1 - 1))]
      out1 <- Rotor1[Rotor1[,1] == inp1, 2]
      out1 <- C26(which(LETTERS == out1) - (RotorPos1 - 1) + (RingSetting1 - 1))
      outL <- LETTERS[out1]
      outL <- Plugb[Plugb[,2] == outL,1]
      
      
      if(length(RotorPos) == 4) c(RotorPos4,RotorPos3,RotorPos2,RotorPos1)
      if(length(RotorPos) == 3) c(RotorPos3,RotorPos2,RotorPos1)   
      
      #     print(outL)
      
      if(Figure){
        EnigmaExtF(InpL = TextIn[i], OutL = outL, RotorPos = c(RotorPos3,RotorPos2,RotorPos1))
        readline()
        if(i != length(TextIn)) dev.off()
      }
      
      TextIn[Entry]  <- TextInp
      TextOut[Entry] <- outL
      Entry <- Entry + 1
      cat(outL)
      cat("\n \n") 
      TextInp <- readline("")
    }       
  }
  
  if(!is.null(Text)){
    TextIn <- strsplit(gsub(" ","",Text), "")[[1]]
    
    if(Figure){
      EnigmaExtF(RotorPos = c(RotorPos3,RotorPos2,RotorPos1))
      readline()
    }
    
    
    for(i in 1:length(TextIn)){

      RotorPos1 <- C26(RotorPos1 + 1)
      
      if(any(LETTERS[C26(RotorPos1 - 1)] == Rotor1T)) RotorPos2 <- C26(RotorPos2 + 1)
      
      if(any(LETTERS[C26(RotorPos1 - 2)] == Rotor1T) & 
           any(LETTERS[C26(RotorPos2)] == Rotor2T)){
        RotorPos3 <- C26(RotorPos3 + 1)
        RotorPos2 <- C26(RotorPos2 + 1)
      }      
      
      inpL <- TextIn[i]
      inpL <- Plugb[Plugb[,1] == inpL,2]
      
      inp1 <- C26(which(LETTERS == inpL) + (RotorPos1 - 1) - (RingSetting1 - 1))
      out1 <- Rotor1[Rotor1[,2] == LETTERS[inp1], 1]
      out1 <- C26(which(LETTERS == out1) - (RotorPos1 - 1) + (RingSetting1 - 1))
      
      inp2 <- LETTERS[C26(out1 + (RotorPos2 - 1) - (RingSetting2 - 1))]
      out2 <- Rotor2[Rotor2[,2] == inp2, 1]
      out2 <- C26(which(LETTERS == out2) - (RotorPos2 - 1) + (RingSetting2 - 1))
      
      inp3 <- LETTERS[C26(out2 + (RotorPos3 - 1) - (RingSetting3 - 1))]
      out3 <- Rotor3[Rotor3[,2] == inp3, 1]
      out3 <- C26(which(LETTERS == out3) - (RotorPos3 - 1) + (RingSetting3 - 1))
      
      if(length(RotorPos) == 4){
        inp4 <- LETTERS[C26(out3 + (RotorPos4 - 1) - (RingSetting4 - 1))]
        out4 <- Rotor4[Rotor4[,2] == inp4, 1]
        out4 <- C26(which(LETTERS == out4) - (RotorPos4 - 1) + (RingSetting4 - 1))
        
        outR <- Reflec[Reflec[,2] == LETTERS[out4], 1]
        outR <- which(LETTERS == outR)
        
        inp4 <- LETTERS[C26(outR + (RotorPos4 - 1))]
        out4 <- Rotor4[Rotor4[,1] == inp4, 2]
        outR <- C26(which(LETTERS == out4) - (RotorPos4 - 1) + (RingSetting4 - 1))
      } else {
        outR <- Reflec[Reflec[,2] == LETTERS[out3], 1]
        outR <- which(LETTERS == outR)
      }
      
      inp3 <- LETTERS[C26(outR + (RotorPos3 - 1) - (RingSetting3 - 1))]
      out3 <- Rotor3[Rotor3[,1] == inp3, 2]
      out3 <- C26(which(LETTERS == out3) - (RotorPos3 - 1) + (RingSetting3 - 1))
      
      inp2 <- LETTERS[C26(out3 + (RotorPos2 - 1) - (RingSetting2 - 1))]
      out2 <- Rotor2[Rotor2[,1] == inp2, 2]
      out2 <- C26(which(LETTERS == out2) - (RotorPos2 - 1) + (RingSetting2 - 1))
      
      inp1 <- LETTERS[C26(out2 + (RotorPos1 - 1) - (RingSetting1 - 1))]
      out1 <- Rotor1[Rotor1[,1] == inp1, 2]
      out1 <- C26(which(LETTERS == out1) - (RotorPos1 - 1) + (RingSetting1 - 1))
      outL <- LETTERS[out1]
      outL <- Plugb[Plugb[,2] == outL,1]
      
      if(Figure){
        EnigmaExtF(InpL = TextIn[i], OutL = outL, RotorPos = c(RotorPos3,RotorPos2,RotorPos1))
        readline()
        if(i != length(TextIn)) dev.off()
      }
      
      TextOut[i] <- outL
    }
  }
  
  TextOutString <- paste(TextOut,collapse = "")
#   cat(TextIn,sep = "")
#   cat("\n \n") 
#   cat(TextOut,sep = "")
  if(!is.null(Helper)){
        CleanUp <- Helper
    if(Helper == "Wehrmacht") CleanUp <- list(c("\\<KLAM\\>","'"),c("\\<ZZ\\>",","),c("\\<X\\>"," "),c("\\<YY\\>","."))
    if(Helper == "Kriegsmarine") CleanUp <- list(c("\\<XX\\>",":"),c("\\<UD\\>","?"),c("\\<X\\>","."),c("\\<Y\\>","."),c("\\<YY\\>","-"))

    gsub2 <- function(pattern, replacement, x, ...) {
      for(i in 1:length(pattern))
        x <- gsub(pattern[i], replacement[i], x, ...)
      x
    } 
#     cat("\n \n")
    TextCleaned <- gsub2(unlist(lapply(CleanUp,function(x) x[1])), unlist(lapply(CleanUp,function(x) x[2])), TextOut)
#     cat(TextCleaned,sep = "")
  }
  
  Output <- list(StartSettings = StartSettings,
                 EndSettings = list(RotorPos = list(Letters = c(LETTERS[RotorPos3],LETTERS[RotorPos2],LETTERS[RotorPos1]),
                                                  Numeric = c(RotorPos3,RotorPos2,RotorPos1))),
                 InputText = Text,
                 OutputText=paste(substring(TextOutString, seq(1,nchar(TextOutString),5), seq(5,nchar(TextOutString),5)),collapse = " "))
  if(!is.null(Helper)) Output <- c(Output,CleanedText=paste(unlist(TextCleaned),collapse = ""))
  return(Output)
}


Enigma(Rotors = c("II","IV","V"), RingSetting = c(02,21,12), RotorPos = c(2,12,1), Reflector = "RefB", Plugboard = c('AV','BS','CG','DL','FU','HZ','IN','KM','OW','RX'),Helper="Wehrmacht",
       Text = "EDPUD NRGYS ZRCXN UYTPO MRMBO FKTBZ REZKM LXLVE FGUEY SIOZV EQMIK UBPMM YLKLT TDEIS MDICA GYKUA CTCDO MOHWX MUUIA UBSTS LRNBZ SZWNR FXWFY SSXJZ VIJHI DISHP RKLKA YUPAD TXQSP INQMA TLPIF SVKDA SCTAC DPBOP VHJK",Figure=FALSE) 



