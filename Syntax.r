require(stringr)

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

RefB  <- "YRUHQSLDPXNGOKMIEBFZCWVJAT"
RefC  <- "FVPJIAOYEDRZXWGCTKUQSBNMHL"

RefBt <- "ENKQAUYWJICOPBLMDXZVFTHRGS"
RefCt <- "RDOBJNTKVEHMLFCWZAXGYIPSUQ"

C26 <- function(x){
  if(x < 1) x <- 26 + x
  if(x >26) x <- x - 26
  return(x)
}

Enigma <- function(Rotors = list(III,II,I), RingSetting = c(1,1,1), RotorPos = c("A","A","A"), Reflector = RefB, Plugboard = NULL, Text = "HENK"){
  
  Rotor1 <- strsplit(Rotors[[length(Rotors)]]$Enc, "")[[1]]
  Rotor1 <- cbind(Rotor1,LETTERS)
  Rotor1T<- Rotors[[length(Rotors)]]$Step
  
  Rotor2 <- strsplit(Rotors[[length(Rotors) - 1]]$Enc, "")[[1]]
  Rotor2 <- cbind(Rotor2,LETTERS)
  Rotor2T<- Rotors[[length(Rotors) - 1]]$Step
  
  Rotor3 <- strsplit(Rotors[[length(Rotors) - 2]]$Enc, "")[[1]]
  Rotor3 <- cbind(Rotor3,LETTERS)
  Rotor3T<- Rotors[[length(Rotors) - 2]]$Step
  
  
  if(length(Rotors) == 4){
    Rotor4 <- strsplit(Rotors[length(Rotors) - 3]$Enc, "")[[1]]
    Rotor4 <- cbind(Rotor4,LETTERS)
  }
  
  RingSetting1 <- RingSetting[length(RingSetting)]
  RingSetting2 <- RingSetting[length(RingSetting)-1]
  RingSetting3 <- RingSetting[length(RingSetting)-2]
  if(length(RingSetting) == 4) RingSetting4 <- RingSetting[length(RingSetting)-3]
  
#   Rotor1 <- cbind(Rotor1,c(LETTERS[RingSetting1:26],LETTERS[0:(RingSetting1-1)]))
#   Rotor2 <- cbind(Rotor2,c(LETTERS[RingSetting2:26],LETTERS[0:(RingSetting2-1)]))
#   Rotor3 <- cbind(Rotor3,c(LETTERS[RingSetting3:26],LETTERS[0:(RingSetting3-1)]))
  
  
  Reflec <- strsplit(Reflector, "")[[1]]
  Reflec <- cbind(Reflec,LETTERS)
  
  Plugb <- cbind(LETTERS,LETTERS)
  if(!is.null(Plugboard)){
    for(s in 1:length(Plugboard)){
      SwapL <- strsplit(Plugboard[[s]],"")[[1]]
      Plugb[Plugb[,1]==SwapL[1],2] <- SwapL[2]
      Plugb[Plugb[,1]==SwapL[2],2] <- SwapL[1]
    }
  }
  
  TextIn <- strsplit(Text, "")[[1]]
  
  RotorPos1 <- 
  RotorPos2 <- 
  RotorPos3 <- 
  
  for(i in 1:length(TextIn)){
    
    RotorPos1 <- C26(RotorPos1 + 1)
    
    if(any(LETTERS[C26(RotorPos1 - 1)] == Rotor1T)) RotorPos2 <- C26(RotorPos2 + 1)
    
    if(any(LETTERS[C26(RotorPos1 - 2)] == Rotor1T) & 
         any(LETTERS[C26(RotorPos2)] == Rotor2T)){
      RotorPos3 <- C26(RotorPos3 + 1)
      RotorPos2 <- C26(RotorPos2 + 1)
    }	
    
    if(length(RotorPos) == 4) c(RotorPos4,RotorPos3,RotorPos2,RotorPos1)
    if(length(RotorPos) == 3) c(RotorPos3,RotorPos2,RotorPos1)
    
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
      outR <- C26(which(LETTERS == out4) - (RotorPos4 - 1))
    } else {
      outR <- Reflec[Reflec[,2] == LETTERS[out3], 1]
      outR <- which(LETTERS == outR)
    }
    
    inp3 <- LETTERS[C26(outR + (RotorPos3 - 1))]
    out3 <- Rotor3[Rotor3[,1] == inp3, 2]
    out3 <- C26(which(LETTERS == out3) - (RotorPos3 - 1))
    
    inp2 <- LETTERS[C26(out3 + (RotorPos2 - 1))]
    out2 <- Rotor2[Rotor2[,1] == inp2, 2]
    out2 <- C26(which(LETTERS == out2) - (RotorPos2 - 1))
    
    inp1 <- LETTERS[C26(out2 + (RotorPos1 - 1))]
    out1 <- Rotor1[Rotor1[,1] == inp1, 2]
    out1 <- C26(which(LETTERS == out1) - (RotorPos1 - 1))
    outL <- LETTERS[out1]
    outL <- Plugb[Plugb[,2] == outL,1]
    
    
    if(length(RotorPos) == 4) c(RotorPos4,RotorPos3,RotorPos2,RotorPos1)
    if(length(RotorPos) == 3) c(RotorPos3,RotorPos2,RotorPos1)   
    
    print(outL)
  }
}

