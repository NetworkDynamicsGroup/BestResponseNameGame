################################################
################################################
### THIS MODEL WAS CONCEIVED AND WRITTEN BY  ###
###   JOSHUA BECKER www.joshua-becker.com    ###
################################################
################################################

rm(list=ls())
library(igraph)


### SET THE FILENAME THAT YOU WANT TO STORE DATA
### DON'T FORGET TO SET YOUR WORKING DIRECTORY!!
filename="best_response_name_game_output_test.csv"

### IF THE FILE DOESN'T EXIST, CREATE IT AND GIVE COLUMN HEADERS
### WE DO IT THIS WAY SO THAT WE CAN RUN THE SCRIPT AGAIN LATER
### TO ADD MORE DATA TO THE SAME FILE 
if( !file.exists(filename) ) {
  write("last3,last,lastnocm,cmsize,maxmem,N,t,network,pstar", filename) 
}

### THIS FUNCTION DEFINES A USERS BEST RESPONSE
### GIVEN THEIR PAST HISTORY OF INTERACTIONS (memory)
### AND THE THRESHOLD.
### WHEN BOTH SOLUTIONS OFFER EQUAL PAYFFF
### pstar = 0.5
bestresponse = function(memory, pstar) {
  a = 1/pstar - 1
  b = 1

  if(length(memory)<1) { stop("Zero length response error..") }
  
    
  mytab = table(memory)
  mytab = mytab*(c(a,b)[as.numeric(names(mytab))])
  
  #if(length(mytab)==0) return(list(response=sample(1:100000,1), single=T))
  if(length(mytab)==0) {
    print(c("Memory:", memory))
    print(c("Mytab:", mytab))
    stop("memory error..")
  }
  
  response=as.numeric(
    names(mytab[
      which(
        max(as.numeric(mytab)) == as.numeric(mytab)
      )
      ])
  )

  if(length(response)>1) response=sample(response, 1)
  
  if(length(response)<1) { stop("zero length response error..") }
  
  if(is.na(response)) { stop("NA response error..") }

  
  response
}


bestResponseNameGame = function(cmsize=0.1, N=100, maxmem=12, pA=0.5, length=c(50,100), generate_network=graph.full) {
  
  ## WRITE DATA FOR EVERY VALUE OF checkt
  checkt = length*N
  
  ## SET UP GAME STATE
  game = list(player=1:N, g=generate_network(N))
  game$memory = lapply(1:N, FUN=function(x){NA})  
  
  ## ASSIGN SOME AGENTS TO BE "COMMITTED"
  V(game$g)$committed = F
  V(game$g)[sample(1:N, cmsize)]$committed = T
  
  
  ## ALL THE COMMITTED AGENTS
  ## START WITH A MEMORY FULL OF 1's
  game$memory[which(V(game$g)$committed)] = list(rep(1, maxmem))
  
  
  ## ALL THE NON-COMMITTED AGENTS
  ## START WITH A MEMORY FULL OF 2's
  game$memory[which(!V(game$g)$committed)] = list(rep(2, maxmem))
  
  ## VECTORS FOR TRACKING HISTORY OF PLAYS
  played=c()  
  playednocm=c()  
  
  ## KEEP RUNNING THE GAME UNTIL WE'VE HIT OUR MAX CHECKPOINT
  for(i in 1:max(checkt)) {
    
    ## FIRST SELECT AN EDGE FOR INTERACTION
    ## THIS IS THE ONLY PLACE WHERE THE GRAPH PLAYS A ROLE.
    to_interact = ends(game$g, sample(E(game$g), 1))
    
    ## USE A RANDOM BINARY TO SELECT ONE AGENT AS SPEAKER
    ## THE OTHER IS HEARER
    speaker = sample(c(1,2), 1)
    hearer = c(1,2)[-speaker]
    A = to_interact[speaker]
    B = to_interact[hearer]
    
    ## A IS THE SPEAKER, SO THEY CHOOSE A STRATEGY BASED
    ## ON THE BEST RESPONSE RULE.
    A.play = bestresponse(game$memory[[A]], pA)
    
    ## B IS THE HEARER
    ## AND SO THEY UPDATE THEIR MEMORY WITH A'S PLAY
    game$memory[[B]] = c(game$memory[[B]], A.play)
    
    ## IF THE HEARER'S MEMORY GOT TOO LONG
    ## REMOVE THE FIRST ENTRY
    if(length(game$memory[[B]])>maxmem) {
      game$memory[[B]] = game$memory[[B]][-1]
    }
    
    ## IF THEY'RE COMMITTED
    ## RESET THEIR MEMORY.
    if(V(game$g)[B]$committed) {
      game$memory[[B]] = 1  
    }
    
    
    ### RECORD ALL PLAYS
    played=c(played, A.play)
    
    ### IF THE SPEAKER IS NOT COMMITTED, WE RECORD THEIR PLAY
    ### ALSO AS A NON-COMMITTED PLAY
    if(!V(game$g)[A]$committed) {
      playednocm = c(playednocm, A.play)
    }
    
    if(i %in% checkt) {
      output = data.frame(
        last3=mean(played[(length(played)-(N*3)):length(played)])
        , last=mean(played[(length(played)-(N)):length(played)])
        , lastnocm=mean(played[(length(playednocm)-(N)):length(playednocm)])
        , cmsize=cmsize, maxmem = maxmem
        , N=N
        , t=i
        , network=game$g$name
        , pstar=pA
      )
      write.table(output, filename, sep=",", col.names=F, row.names=F, append=T)
      
    }
  }
}


pA = 0.5
maxmem = 12
N = 100

### CYCLE THROUGH DIFFERENT COMMITTED MINORITY SIZES
for(i in 1:100) {
  print(i)
  for(cmsize in round(seq(0.18, 0.28, by=0.02)*N)) {
    print(cmsize)
    bestResponseNameGame(pA, maxmem, N, cmsize)
  }
}

