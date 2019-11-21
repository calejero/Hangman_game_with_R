####################################
####
#### Hangman game with R
####
####  Created by Tomaz Kastrun
####  Date: November, 19, 2019
####  Version: 0.0.1 (bugs)
####

####  ToDo: Write checker for existing letters

#####################################

library(ggplot2)

#######################
### Helper functions
#######################

zamenjaj2 <- function(beseda, crka){

  if (regexpr(crka, beseda)[1] > 0) {
    
    pozicija <- regexpr(crka, beseda)[1]
    iskana_beseda[pozicija] <- crka
    print(paste(iskana_beseda, collapse = " "))
    
    #convert back to single string to check for equality
    if (paste(iskana_beseda, collapse = "") == beseda) {
      return(iskana_beseda)
      print("End Game!")
    }
    
    return(iskana_beseda)
    
  }
}

drawHead <- function(orig_position = c(0,0)
                     ,dia = 1
                     ,nof_points = 10
                     , group=5){

  vectT <- seq(0,2*pi, length.out = nof_points)
  r = dia/2
  x_data <- orig_position[1] + r * cos(vectT)
  y_data <- orig_position[2] + r * sin(vectT)
  return(data.frame (x = x_data, y = y_data, group=group))
}

drawMan <- function(st_napak) {
  
  ggplot(levels[which(levels$group<=st_napak),], aes(x=x, y=y, group=group)) + 
    geom_path(size=2.5) + 
    theme_void()
  
}

########################
###### Data for graph
########################
level1 <- data.frame(x=c(1,2,3,4,5,6,7,8), y=c(1,1,1,1,1,1,1,1), group=c(1,1,1,1,1,1,1,1))
level2 <- data.frame(x=c(4,4,4,4,4), y=c(1,2,3,4,5),group=c(2,2,2,2,2))
level3 <- data.frame(x=c(4,5,6), y=c(5,5,5), group=c(3,3,3))
level4 <- data.frame(x=c(6,6), y=c(5,4), group=c(4,4))
level5 <- drawHead(c(6,3.5),1,10,5)
level6 <- data.frame(x=c(6,6,5.8,6.2),y=c(3,1.5,1.5,1.5), group=c(6,6,6,6))
level7 <- data.frame(x=c(5.5,6,6.5),y=c(2,2.5,2), group=c(7,7,7))
levels <- rbind(level1,level2,level3,level4,level5,level6,level7)
rm(level1,level2,level3,level4,level5,level6,level7)


########################
### Helper variables
########################

rm(st_napak, izbor, crka,cilj,cilj_n, iskana_beseda,beseda,i,active)
st_napak = 0
i = 0
izbor = NULL
cilj = NULL
cilj_n = NULL
active = TRUE



########################
##  Hangman
#######################


beseda <- readline(prompt="Word: ")
#beseda <- 'miza'

#iskana_beseda <- replicate(nchar(beseda),'_')
#iskana_beseda <- toString(as.character(replicate(nchar(beseda), ' _ ')))
iskana_beseda <- replicate(nchar(beseda),'_')


while (active == TRUE) {
  
  if (i == 0) {
    writeLines(paste(iskana_beseda, collapse = " "))
    }
  
  crka <- readline(prompt="Enter Letter: ")
  izbor <- rbind(izbor, crka)
  
  #iskana_beseda
  if (grepl(crka, beseda) == TRUE) {
   
    cilj <- rbind(cilj, crka)
    iskana_beseda <- zamenjaj2(beseda, crka)
    #print(zamenjaj2(beseda, crka))  
    print(paste("Yay!","Try N:",i+1,"Wrong letters: {",(toString(paste0(cilj_n, sep=","))),"}")) 
 
    if (as.character(paste(iskana_beseda, collapse = "")) == beseda) {
      active == FALSE
      print("Bravo, win!")
      break
    }
    
  } else {
    cilj_n <- rbind(cilj_n, crka)
    print(paste("Nope!","Try N:",i+1,"Wrong letters: {",(toString(paste0(cilj_n, sep=","))),"}")) 
    #print(toString(paste0(cilj_n, sep=",")))
    
    #Graph
    st_napak <- as.integer(length(cilj_n))
    print(drawMan(st_napak = st_napak))
    
    if(as.integer(st_napak)==7){
      active==FALSE
      break
      print("End Game")
    }
    
  }
  
  i= i+1
  #cat("\f")  
  if(st_napak==7){
    active==FALSE
    break
    print("End game")
  }
      
}

