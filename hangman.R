####################################
####
#### Hangman game with R
####
####  Created by Tomaz Kastrun
####  Contributed by Jesus Armand Calejero Roman
####  Date: November, 26, 2019
####  Version: 0.1
####

#####################################

library(ggplot2)

#######################
### Helper functions
#######################

AnalyzeWordCandidate <- function(control.df, word.c) {
  control.df$flag <- ifelse(control.df$word == word.c, 1, 0)
  return(control.df$index[control.df$flag == 1])
}

drawHead <- function(orig_position = c(0,0),
                     dia = 1,
                     nof_points = 10,
                     group = 5){
  
  vectT <- seq(0,2*pi, length.out = nof_points)
  r <- dia/2
  x_data <- orig_position[1] + r * cos(vectT)
  y_data <- orig_position[2] + r * sin(vectT)
  return(data.frame (x = x_data, y = y_data, group = group))
}

drawMan <- function(st_napak) {
  
  ggplot(levels[which(levels$group <= st_napak), ], aes(x = x, y = y, group = group)) + 
    geom_path(size = 2.5) + 
    theme_void()
  
}

########################
###### Data for graph
########################
level1 <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8), 
                     y = c(1, 1, 1, 1, 1, 1, 1, 1), 
                     group = c(1, 1, 1, 1, 1, 1, 1, 1))
level2 <- data.frame(x = c(4, 4, 4, 4, 4), 
                     y = c(1, 2, 3, 4, 5),
                     group = c(2, 2, 2, 2, 2))
level3 <- data.frame(x = c(4, 5, 6), y= c (5, 5, 5), group = c(3, 3, 3))
level4 <- data.frame(x = c(6, 6), y = c(5, 4), group = c(4, 4))
level5 <- drawHead(c(6, 3.5), 1, 10, 5)
level6 <- data.frame(x = c(6, 6, 5.8, 6.2), 
                     y =c 3,1.5,1.5,1.5), group = c(6, 6, 6, 6))
level7 <- data.frame(x = c(5.5, 6, 6.5), y = c(2, 2.5, 2), group = c(7, 7, 7))
levels <- rbind(level1, level2, level3, level4, level5, level6, level7)
rm(level1, level2, level3, level4, level5, level6, level7)

########################
### Helper variables
########################

rm(st_napak, izbor, crka, cilj, cilj_n, iskana_beseda, beseda, i, active)
st_napak = 0
i = 0
izbor = NULL
cilj = NULL
cilj_n = NULL
active = TRUE

########################
##  Hangman
#######################

StartNewGame <- function(sensitive.flag = TRUE) { # sensitive.flag: TRUE -> capital letters are available. 
  beseda <- readline(prompt = "Word: ")
  
  if (sensitive.flag == FALSE) {
    beseda <- base::tolower(beseda)
  }
  control.df <- data.frame(word = strsplit(beseda, "")[[1]], 
                           flag = 0,
                           archived = 0,
                           index = rep(1:length(strsplit(beseda, "")[[1]])), 
                           stringsAsFactors = FALSE)
  iskana_beseda <- replicate(nchar(beseda), '_')
  
  while (active == TRUE) {
    
    if (i == 0) {
      writeLines(paste(iskana_beseda, collapse = " "))
    }
    
    crka <- readline(prompt="Enter Letter: ")
    izbor <- rbind(izbor, crka)
    
    #iskana_beseda
    if (grepl(crka, beseda) == TRUE) {
      
      cilj <- rbind(cilj, crka)
      index.c <- AnalyzeWordCandidate(control.df, crka)
      if (max(control.df$archived[index.c]) == 1) {
          print(paste("Yay!", "Try N:", i + 1, "Repeat letters, try again. Remember, wrong letters: {", (toString(paste0(cilj_n, sep = ","))), "}")) 
      } else {
        control.df$archived[index.c] <- 1
        iskana_beseda[index.c] <- crka
        cat(iskana_beseda, "\n")
        print(paste("Yay!", "Try N:", i + 1, "Wrong letters: {", (toString(paste0(cilj_n, sep = ","))), "}")) 
        
        if (as.character(paste(iskana_beseda, collapse = "")) == beseda) {
          active == FALSE
          print("Bravo, win!")
          break
        }
      }
    } else {
      cilj_n <- rbind(cilj_n, crka)
      print(paste("Nope!", "Try N:", i + 1, "Wrong letters: {", (toString(paste0(cilj_n, sep = ","))), "}")) 
      #print(toString(paste0(cilj_n, sep=",")))
      
      #Graph
      st_napak <- as.integer(length(cilj_n))
      print(drawMan(st_napak = st_napak))
      
      if (as.integer(st_napak) == 7) {
        active == FALSE
        break
        print("End Game")
      }
      
    }
    
    i <- i + 1

    if (st_napak == 7) {
      active == FALSE
      break
      print("End game")
    }
    
  }
}
