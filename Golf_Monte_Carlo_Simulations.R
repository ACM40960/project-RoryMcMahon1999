

library(dplyr) # load dplyr library
library(sn) #Load sn library
set.seed(17371086) #Set seed so results stay the same
data<-read.csv("PGAtourdata.csv") #Load in the data


#Get average strokes gained per player
player_sg_avg<-aggregate(sg_total ~ Player_initial_last,data,mean )

#Calculate Number of rounds per player(for Variance later)
num_rounds <- data %>% count(Player_initial_last)

#####Only calculate Var for players who have played more than 50 rounds #####

# Calculate Var for everyone
player_sg_var<- aggregate(sg_total ~ Player_initial_last,data, var )

# Merge mean,var and number of rounds together
colnames(player_sg_avg)<-c("Player_Name","mean") 
colnames(player_sg_var)<-c("Player_Name","var")#rename to differentiate
colnames(num_rounds)<-c("Player_Name","rounds")#rename to differentiate


player_prof<-left_join(player_sg_avg,player_sg_var,'Player_Name') #Merge mean and varr
player_prof <- left_join(player_prof,num_rounds,"Player_Name") #merge numrounds with mean and var

#Set Var for those with less than 50 rounds to NA
player_prof[player_prof$rounds<50,3] <- NA

#Now calculate the average variance for those with more than 50 rounds
mean_var<-mean(player_prof$var,na.rm=TRUE)
#Set variance to mean var for those with less than 50 rounds
player_prof$var[is.na(player_prof$var)] <- mean_var 

#Code to create output for Appendix A.2 and A.3
A2<-player_prof[order(player_prof$mean),]
head(A2)
tail(A2)

A3<-player_prof[order(player_prof$var),]
A3
head(A3)
tail(A3)




################ Distribution of scoring #################
#Lets examine what distribution players scores come from
# Plot a histogram of player scores
h2<-hist(data$sg_total,probability = TRUE,main="Strokes Gained Total For All Players",xlab="Strokes Gained Total")
lines(density(data$sg_total,na.rm = TRUE), col = 1, lwd = 2)  #Plot for report

#The data is not quite normal as the left tail is bigger than the right tail( more bad scores)
#Negative skew

#Plot of skewnormal distribution(for Appendix A1)
h3<-hist(rsn(10000, 0, 3.9, -2))
plot(h3,col="red",main="Plot of Skewnormal Distribution",xlab="Strokes Gained")


#################### Does course affect Golfers scoring ################
# Only look at golfers who have played courses more than 7 times otherwise just noise
#Add a column of ones
courses_played <- data %>% group_by(across(names(.)[c(1,26)])) %>% summarise(N=n()) #Count player/course combination occurences
courses_played <- courses_played %>% mutate(greater_7 = if_else(N>7,1,0)) # 1 if greater than 7, otherwise 0


#Join number of times played with whole dataset
data <- data %>% right_join(courses_played,by=c("Player_initial_last","course"))
data <- within(data,rm("N")) #Remove N column


# Calculate mean strokes gained by Player and course
player_course_sg<-aggregate(sg_total ~ Player_initial_last + course ,data,mean )
colnames(player_course_sg) <- c("Player_initial_last","course","sg_total_course_player") #Label columns 


#Include average strokes gained for each player
sg_p<-aggregate(sg_total ~ Player_initial_last,data,mean )

colnames(sg_p) <- c("Player_initial_last","sg_total_player") #label appropriately
#Now join with course dataset
player_course_sg <- player_course_sg %>% right_join(sg_p,by="Player_initial_last")



#Subtract average from tournament to see difference in strokes gained for course
player_course_sg$sg_course_adj <- player_course_sg$sg_total_course_player - player_course_sg$sg_total_player

#Merge with courses played
player_course_sg <- player_course_sg %>% right_join(courses_played,by=c("Player_initial_last","course"))

#Change adjustment to zero if less than 7 rounds played at course
player_course_sg <- player_course_sg %>% mutate(sg_course_adj = if_else(greater_7 == 1, sg_course_adj,0))

# Now only get columns of name,course and sg_adj
player_course_profile <- player_course_sg[,c(1,2,5)]

#Plot for report
courses_plot <- player_course_profile[player_course_profile$sg_course_adj != 0,]
h1<-hist(courses_plot$sg_course_adj)
cuts1<-cut(h1$breaks, c(-Inf,-0.01,Inf))
plot(h1,col=c("red","green")[cuts1],main="Overperformance/Underperformance in strokes gained by course",xlab="Strokes gained adjusted")




################# Previous score/run[form] ################
#relative strokes gained follow linear relationship
#Input data from graph
x<-c(-8,-7,-5,-3,-1,1,3,5,7,8)
y<-c(-.96,-.705,-.432,-.245,-.128,-.033,0.112,0.205,0.253,0.514)
df_run<-data.frame(x,y)
linear_mod <- lm(y~x,df_run)
linear_mod$coefficients[2]






##############Loop to simulate a tournament #####################





########################## Simulate All Players on non-specific course #################################









#Simulate first two days of a tournament
N <- 25 #Number of tournaments to simulate, we will set to 10,000 but also include examples with 100 and 1000 in our report
Num_rounds_2day<-N*2  #Number of rounds to simulate
rounds<-data.frame(player_prof$Player_Name) #Create a dataframe with just player names for the tournament

for (i in 1:length(player_prof$Player_Name)){
  for (j in 1:Num_rounds_2day){
    if(j%%2 == 1){ #Check if round 1 or 2
    rounds[ i, paste0("score", j)] <- player_prof$mean[i] + rsn(1,0,player_prof$var[i],-2)  #For round one no need for Hot Hand
    }else{
      rounds[ i, paste0("score", j)] <- player_prof$mean[i] + rsn(1,0,player_prof$var[i],-2)  + 0.077*(rounds[ i, paste0("score", j-1)])  #For round 2 include Hot Hand
    }
  }
}

### Calculate sum for every two columns(To determine cut and tee times for the next day)

tournament_day2<-data.frame(player_prof$Player_Name) 
for (i in 1:length(player_prof$Player_Name)){
  for (j in 0:(N - 1)){
    tournament_day2[ i, paste0("round", j+1)] <- rounds[i, (j*2)+ 2 ] + rounds[i, (j*2)+ 3 ] 
  }
}


#Now lets calculate the position of each player
#NB a plus represents strokes gained, so the higher the score the better(contrary to how golf scores are normally interpreted)
tournamnet_day2_pos <- data.frame(player_prof$Player_Name)
for (j in 2:(N + 1)){
  tournamnet_day2_pos[ , paste0("tournament", j-1)] <- rank(-tournament_day2[,j])
}

#Now set all golfers in bottom half after 2 days to NA i.e missed the cut
Num_p <- length(player_prof$Player_Name) #Number of players
cut_line <- ceiling(Num_p/2) #Calculate cut line(half of players) NB We round up
morning <- ceiling(Num_p/4) #Those who play in the morning(worst half of players who arent cut)

#Simulate Scores for round 3(Set to NA if in bottom half)
tournament_day3<-data.frame(player_prof$Player_Name) 
for (i in 1:length(player_prof$Player_Name)){
  for (j in 1:N){ if(tournamnet_day2_pos[i,j+1] > cut_line){ #Set to NA if in bottom half after 2 days
    tournament_day3[ i, paste0("round", j)] <- NA
  } else if(tournamnet_day2_pos[i,j+1] > morning ){ # Morning tee time so adjust formula acccodingly
    tournament_day3[ i, paste0("round", j)] <- player_prof$mean[i] + rsn(1,0,player_prof$var[i],-2)  + 0.077*(rounds[ i, (j*2)+1]) + 0.075
  }else{ #Afternoon Tee time
    tournament_day3[ i, paste0("round", j)] <- player_prof$mean[i] + rsn(1,0,player_prof$var[i],-2)  + 0.077*(rounds[ i, (j*2)+1]) - 0.075
  }
  }
}


# Get sum of scores after 3 days(for those still in the competiton)
tournamnet_day3_totalscore <- data.frame(player_prof$Player_Name)
for (i in 1:length(player_prof$Player_Name)){
  for (j in 1:N){
    if(is.na(tournament_day3[i,j+1])){
      tournamnet_day3_totalscore[i,paste0("round", j)] <- NA
    }else{
      tournamnet_day3_totalscore[i,paste0("round", j)] <- tournament_day3[i,j+1] + tournament_day2[i,j+1] 
    }
  }
}

#Now lets get the positions for day 3 so we can set the tee times for day 4
tournamnet_day3_pos <- data.frame(player_prof$Player_Name)
for (j in 2:(N + 1)){
  tournamnet_day3_pos[ , paste0("tournament", j-1)] <- rank(-tournamnet_day3_totalscore[,j])
}

#Now set those who missed the cut to NA
for (i in 1:length(player_prof$Player_Name)){
  for (j in 1:N){
    if(is.na(tournament_day3[i,j+1])){
      tournamnet_day3_pos[i,j+1] <- NA
    }
  }
}



#Now simulate scores for day 4
tournament_day4<-data.frame(player_prof$Player_Name) 
for (i in 1:length(player_prof$Player_Name)){
  for (j in 1:N){ if(is.na(tournamnet_day3_totalscore[i,j+1])){ #Set to NA for those who missed cut
    tournament_day4[ i, paste0("round", j)] <- NA
  } else if(tournamnet_day3_pos[i,j+1] > morning ){ # Morning tee time so adjust formula acccodingly
    tournament_day4[ i, paste0("round", j)] <- player_prof$mean[i] + rsn(1,0,player_prof$var[i],-2)  + 0.077*(tournament_day3[ i,j+1]) + 0.075
  }else{ #Afternoon Tee time
    tournament_day4[ i, paste0("round", j)] <- player_prof$mean[i] + rsn(1,0,player_prof$var[i],-2)  + 0.077*(tournament_day3[ i,j+1]) - 0.075
  }
  }
}

# Now calculate position after the 4 days 
tournamnet_day4_pos <- data.frame(player_prof$Player_Name)
for (j in 2:(N + 1)){
  tournamnet_day4_pos[ , paste0("tournament", j-1)] <- rank(-tournament_day4[,j])
}

#Now set those who missed the cut to NA
for (i in 1:length(player_prof$Player_Name)){
  for (j in 1:N){
    if(is.na(tournament_day3[i,j+1])){
      tournamnet_day4_pos[i,j+1] <- NA
    }
  }
}







# Now count number of wins, top 5 and top 10

position<- data.frame(player_prof$Player_Name) 
position$num_1_fin<-apply(tournamnet_day4_pos[2:N],1,function(x) length(which(x == 1)))

position$top_5<- apply(tournamnet_day4_pos[2:N],1,function(x) length(which(x %in% c(1,2,3,4,5))))

position$top_10 <- apply(tournamnet_day4_pos[2:N],1,function(x) length(which(x %in% c(1,2,3,4,5,6,7,8,9,10))))

# Now calculate the probabilities
position$prob_1 <- position$num_1_fin/N
position$prob_top5 <- position$top_5/N
position$prob_top10 <- position$top_10/N


#Now order by probability of number ten finish
positionordered <- position[order(position$prob_1),c(1,5,6,7)]

#Look at  tail for those most likely to win
tail(positionordered)






##################################### Include course element ################################################






#For our course specific element we will include an extra column in our player_profile to account for coures_specific element

course_name <- "Augusta National Golf Club - Augusta, GA"  #Set course we are simulating from

#get list of players and that specific course
course_players <- player_course_profile[player_course_profile$course == course_name,]
colnames(course_players) <- c("Player_Name","course","sg_course")



# Now merge by name  with player_profile and call player_profile_course
player_profile_course <- left_join(player_prof,course_players,'Player_Name') #Merge mean and varr

#Set to 0 if NA
player_profile_course[is.na(player_profile_course$sg_course),6] <- 0

#Now we will simulate our model using the same method as before but include course element
#We will store results in variables ending in _C
#Simulate first two days of a tournament
N_C<- 25 #Number of tournaments to simulate, we will set to 10,000 but also include examples with 100 and 1000 in our report
Num_rounds_2day_C<-N_C*2  #Number of rounds to simulate
rounds_C<-data.frame(player_profile_course$Player_Name) #Create a dataframe with just player names for the tournament

for (i in 1:length(player_profile_course$Player_Name)){
  for (j in 1:Num_rounds_2day_C){
    if(j%%2 == 1){ #Check if round 1 or 2
      rounds_C[ i, paste0("score", j)] <- player_profile_course$mean[i] + player_profile_course$sg_course[i] +  rsn(1,0,player_profile_course$var[i],-2)  #For round one no need for Hot Hand
    }else{
      rounds_C[ i, paste0("score", j)] <- player_profile_course$mean[i] + player_profile_course$sg_course[i]  + rsn(1,0,player_profile_course$var[i],-2)  + 0.077*(rounds_C[ i, paste0("score", j-1)])  #For round 2 include Hot Hand
    }
  }
}

### Calculate sum for every two columns(To determine cut and tee times for the next day)

tournament_day2_C<-data.frame(player_profile_course$Player_Name) 
for (i in 1:length(player_profile_course$Player_Name)){
  for (j in 0:(N_C - 1)){
    tournament_day2_C[ i, paste0("round", j+1)] <- rounds_C[i, (j*2)+ 2 ] + rounds_C[i, (j*2)+ 3 ] 
  }
}


#Now lets calculate the position of each player
#NB a plus represents strokes gained, so the higher the score the better(contrary to how golf scores are normally interpreted)
tournamnet_day2_pos_C <- data.frame(player_profile_course$Player_Name)
for (j in 2:(N_C + 1)){
  tournamnet_day2_pos_C[ , paste0("tournament", j-1)] <- rank(-tournament_day2_C[,j])
}

#Now set all golfers in bottom half after 2 days to NA i.e missed the cut
Num_p_C <- length(player_profile_course$Player_Name) #Number of players
cut_line_C <- ceiling(Num_p_C/2) #Calculate cut line(half of players) NB We round up
morning_C <- ceiling(Num_p_C/4) #Those who play in the morning(worst half of players who arent cut)

#Simulate Scores for round 3(Set to NA if in bottom half)
tournament_day3_C<-data.frame(player_profile_course$Player_Name) 
for (i in 1:length(player_profile_course$Player_Name)){
  for (j in 1:N_C){ if(tournamnet_day2_pos_C[i,j+1] > cut_line_C){ #Set to NA if in bottom half after 2 days
    tournament_day3_C[ i, paste0("round", j)] <- NA
  } else if(tournamnet_day2_pos_C[i,j+1] > morning_C ){ # Morning tee time so adjust formula acccodingly
    tournament_day3_C[ i, paste0("round", j)] <- player_profile_course$mean[i] +  player_profile_course$sg_course[i] +  rsn(1,0,player_profile_course$var[i],-2)  + 0.077*(rounds_C[ i, (j*2)+1]) + 0.075
  }else{ #Afternoon Tee time
    tournament_day3_C[ i, paste0("round", j)] <- player_profile_course$mean[i] + player_profile_course$sg_course[i] + rsn(1,0,player_profile_course$var[i],-2)  + 0.077*(rounds_C[ i, (j*2)+1]) - 0.075
  }
  }
}


# Get sum of scores after 3 days(for those still in the competiton)
tournamnet_day3_totalscore_C <- data.frame(player_profile_course$Player_Name)
for (i in 1:length(player_profile_course$Player_Name)){
  for (j in 1:N_C){
    if(is.na(tournament_day3_C[i,j+1])){
      tournamnet_day3_totalscore_C[i,paste0("round", j)] <- NA
    }else{
      tournamnet_day3_totalscore_C[i,paste0("round", j)] <- tournament_day3_C[i,j+1] + tournament_day2_C[i,j+1] 
    }
  }
}

#Now lets get the positions for day 3 so we can set the tee times for day 4
tournamnet_day3_pos_C <- data.frame(player_profile_course$Player_Name)
for (j in 2:(N_C + 1)){
  tournamnet_day3_pos_C[ , paste0("tournament", j-1)] <- rank(-tournamnet_day3_totalscore_C[,j])
}

#Now set those who missed the cut to NA
for (i in 1:length(player_profile_course$Player_Name)){
  for (j in 1:N_C){
    if(is.na(tournament_day3_C[i,j+1])){
      tournamnet_day3_pos_C[i,j+1] <- NA
    }
  }
}



#Now simulate scores for day 4
tournament_day4_C<-data.frame(player_profile_course$Player_Name) 
for (i in 1:length(player_profile_course$Player_Name)){
  for (j in 1:N_C){ if(is.na(tournamnet_day3_totalscore_C[i,j+1])){ #Set to NA for those who missed cut
    tournament_day4_C[ i, paste0("round", j)] <- NA
  } else if(tournamnet_day3_pos_C[i,j+1] > morning_C ){ # Morning tee time so adjust formula acccodingly
    tournament_day4_C[ i, paste0("round", j)] <- player_profile_course$mean[i] + player_profile_course$sg_course[i] + rsn(1,0,player_profile_course$var[i],-2)  + 0.077*(tournament_day3_C[ i,j+1]) + 0.075
  }else{ #Afternoon Tee time
    tournament_day4_C[ i, paste0("round", j)] <- player_profile_course$mean[i] + player_profile_course$sg_course[i] + rsn(1,0,player_profile_course$var[i],-2)  + 0.077*(tournament_day3_C[ i,j+1]) - 0.075
  }
  }
}

# Now calculate position after the 4 days 
tournamnet_day4_pos_C <- data.frame(player_profile_course$Player_Name)
for (j in 2:(N_C + 1)){
  tournamnet_day4_pos_C[ , paste0("tournament", j-1)] <- rank(-tournament_day4_C[,j])
}

#Now set those who missed the cut to NA
for (i in 1:length(player_profile_course$Player_Name)){
  for (j in 1:N_C){
    if(is.na(tournament_day3_C[i,j+1])){
      tournamnet_day4_pos_C[i,j+1] <- NA
    }
  }
}


# Now count number of wins, top 5 and top 10

position_C<- data.frame(player_profile_course$Player_Name) 
position_C$num_1_fin<-apply(tournamnet_day4_pos_C[2:N_C],1,function(x) length(which(x == 1)))

position_C$top_5<- apply(tournamnet_day4_pos_C[2:N_C],1,function(x) length(which(x %in% c(1,2,3,4,5))))

position_C$top_10 <- apply(tournamnet_day4_pos_C[2:N_C],1,function(x) length(which(x %in% c(1,2,3,4,5,6,7,8,9,10))))

# Now calculate the probabilities
position_C$prob_1 <- position_C$num_1_fin/N_C
position_C$prob_top5 <- position_C$top_5/N_C
position_C$prob_top10 <- position_C$top_10/N_C


#Now order by probability of number ten finish
positionordered_C <- position_C[order(position_C$prob_1),c(1,5,6,7)]

#Look at  tail for those most likely to win
tail(positionordered_C)




################################## Simulate an actual Tournament for a given filed and course #######################



#We will denote variables with _F at the end to show 
# First set up profile for all players at Muirfield Golf club
course_players_F <- player_course_profile[player_course_profile$course == "Muirfield Village Golf Club - Dublin, OH", ]
colnames(course_players_F) <- c("Player_Name","course","sg_course")



# Now merge by name  with player_profile and call player_profile_course
player_profile_course_F <- left_join(player_prof,course_players_F,'Player_Name') #Merge mean and varr

#Set to 0 if NA
player_profile_course_F[is.na(player_profile_course_F$sg_course),6] <- 0

#Now we want to only include players from the competition



#Input list of player(Quite tedious!!!!!!!!) , J.Piot, J. Morgan , C.Ramey not in our database
player_list<- c("B. Horschel","A. Wise","P. Cantlay","J. Niemann","M. Homa", "W. Zalatoris","D. McCarthy","S. Theegala","D. Berger","S. Im","J. Rahm","B. Steele","C. Conners","M. Pereira","S. Kim","D. Riley","C. Smith","X. Schauffele","G. Higgo","K. Mitchell","B. Harman","A. Hadwin","C. Howell III","J. Spieth","R. McIlroy","P. Perez","A. Rai","A. Schenk","L. List","F. Molinari","J. Day","E. Grillo","S. Lowry","J. Dahmen","B. Hossler","A. Ancer","J. Poston","W. Clark","M. Hughes","M. Laird","D. Lipsky","K. Bradley","M. NeSmith","J. Vegas","A. Svensson","M. Kuchar","S. Straka","D. Lingmerth","L. Herbert","C. Tringale","V. Hovland","L. Griffin","C. Davis","C. Villegas","C. Pan","C. Kirk","P. Reed","T. Merritt","K. Lee","L. Glover","B. Snedeker","D. Ghim","C. Young","K. Hickok","R. Fowler","C. Ortiz","A. Scott","C. Kim","B. Wu","R. Moore","S. Ryder","M. Fitzpatrick","C. Hoffman","K. Kitayama",
                "D. Lee","A. Lahiri","W. McGirt","L. Donald","R. Streb","R. Cabrera Bello","E. van Rooyen","A. Noren","M. Wolff","C. Morikawa","S. Power","K. Streelman","G. Woodland","P. Rodgers","M. Leishman","T. Hoge","N. Lashley","C. Champ","M. Jones","M. Lee","J. Dufner",
                "A. Long","H. Swafford","C. Luck","A. Putnam","A. Smalley","B. Hoag","T. Moore","S. Cink","D. Willett","S. Stallings","B. DeChambeau","J. Pak","H. English","B. Hagy","P. Kizzire","R. Brehm","R. Knox","N. Watney","R. Palmer","H. Matsuyama")


player_profile_event <- player_profile_course_F[player_profile_course_F$Player_Name %in% player_list,]


### Now lets simulate this event

#Simulate first two days of a tournament
N_F<- 25 #Number of tournaments to simulate, we will set to 10,000 but also include examples with 100 and 1000 in our report
Num_rounds_2day_F<-N_F*2  #Number of rounds to simulate
rounds_F<-data.frame(player_profile_event$Player_Name) #Create a dataframe with just player names for the tournament

for (i in 1:length(player_profile_event$Player_Name)){
  for (j in 1:Num_rounds_2day_F){
    if(j%%2 == 1){ #Check if round 1 or 2
      rounds_F[ i, paste0("score", j)] <- player_profile_event$mean[i] + player_profile_event$sg_course[i] +  rsn(1,0,player_profile_event$var[i],-2)  #For round one no need for Hot Hand
    }else{
      rounds_F[ i, paste0("score", j)] <- player_profile_event$mean[i] + player_profile_event$sg_course[i]  + rsn(1,0,player_profile_event$var[i],-2)  + 0.077*(rounds_F[ i, paste0("score", j-1)])  #For round 2 include Hot Hand
    }
  }
}

### Calculate sum for every two columns(To determine cut and tee times for the next day)

tournament_day2_F<-data.frame(player_profile_event$Player_Name) 
for (i in 1:length(player_profile_event$Player_Name)){
  for (j in 0:(N_F - 1)){
    tournament_day2_F[ i, paste0("round", j+1)] <- rounds_F[i, (j*2)+ 2 ] + rounds_F[i, (j*2)+ 3 ] 
  }
}


#Now lets calculate the position of each player
#NB a plus represents strokes gained, so the higher the score the better(contrary to how golf scores are normally interpreted)
tournamnet_day2_pos_F<- data.frame(player_profile_event$Player_Name)
for (j in 2:(N_F + 1)){
  tournamnet_day2_pos_F[ , paste0("tournament", j-1)] <- rank(-tournament_day2_F[,j])
}

#Now set all golfers in bottom half after 2 days to NA i.e missed the cut
Num_p_F <- length(player_profile_event$Player_Name) #Number of players
cut_line_F <- ceiling(Num_p_F/2) #Calculate cut line(half of players) NB We round up
morning_F <- ceiling(Num_p_F/4) #Those who play in the morning(worst half of players who arent cut)

#Simulate Scores for round 3(Set to NA if in bottom half)
tournament_day3_F<-data.frame(player_profile_event$Player_Name) 
for (i in 1:length(player_profile_event$Player_Name)){
  for (j in 1:N_F){ if(tournamnet_day2_pos_F[i,j+1] > cut_line_F){ #Set to NA if in bottom half after 2 days
    tournament_day3_F[ i, paste0("round", j)] <- NA
  } else if(tournamnet_day2_pos_F[i,j+1] > morning_F){ # Morning tee time so adjust formula acccodingly
    tournament_day3_F[ i, paste0("round", j)] <- player_profile_event$mean[i] +  player_profile_event$sg_course[i] +  rsn(1,0,player_profile_event$var[i],-2)  + 0.077*(rounds_F[ i, (j*2)+1]) + 0.075
  }else{ #Afternoon Tee time
    tournament_day3_F[ i, paste0("round", j)] <- player_profile_event$mean[i] + player_profile_event$sg_course[i] + rsn(1,0,player_profile_event$var[i],-2)  + 0.077*(rounds_F[ i, (j*2)+1]) - 0.075
  }
  }
}


# Get sum of scores after 3 days(for those still in the competiton)
tournamnet_day3_totalscore_F <- data.frame(player_profile_event$Player_Name)
for (i in 1:length(player_profile_event$Player_Name)){
  for (j in 1:N_F){
    if(is.na(tournament_day3_F[i,j+1])){
      tournamnet_day3_totalscore_F[i,paste0("round", j)] <- NA
    }else{
      tournamnet_day3_totalscore_F[i,paste0("round", j)] <- tournament_day3_F[i,j+1] + tournament_day2_F[i,j+1] 
    }
  }
}

#Now lets get the positions for day 3 so we can set the tee times for day 4
tournamnet_day3_pos_F <- data.frame(player_profile_event$Player_Name)
for (j in 2:(N_F + 1)){
  tournamnet_day3_pos_F[ , paste0("tournament", j-1)] <- rank(-tournamnet_day3_totalscore_F[,j])
}

#Now set those who missed the cut to NA
for (i in 1:length(player_profile_event$Player_Name)){
  for (j in 1:N_F){
    if(is.na(tournament_day3_F[i,j+1])){
      tournamnet_day3_pos_F[i,j+1] <- NA
    }
  }
}



#Now simulate scores for day 4
tournament_day4_F<-data.frame(player_profile_event$Player_Name) 
for (i in 1:length(player_profile_event$Player_Name)){
  for (j in 1:N_F){ if(is.na(tournamnet_day3_totalscore_F[i,j+1])){ #Set to NA for those who missed cut
    tournament_day4_F[ i, paste0("round", j)] <- NA
  } else if(tournamnet_day3_pos_F[i,j+1] > morning_F ){ # Morning tee time so adjust formula acccodingly
    tournament_day4_F[ i, paste0("round", j)] <- player_profile_event$mean[i] + player_profile_event$sg_course[i] + rsn(1,0,player_profile_event$var[i],-2)  + 0.077*(tournament_day3_F[ i,j+1]) + 0.075
  }else{ #Afternoon Tee time
    tournament_day4_F[ i, paste0("round", j)] <- player_profile_event$mean[i] + player_profile_event$sg_course[i] + rsn(1,0,player_profile_event$var[i],-2)  + 0.077*(tournament_day3_F[ i,j+1]) - 0.075
  }
  }
}

# Now calculate position after the 4 days 
tournamnet_day4_pos_F <- data.frame(player_profile_event$Player_Name)
for (j in 2:(N_F + 1)){
  tournamnet_day4_pos_F[ , paste0("tournament", j-1)] <- rank(-tournament_day4_F[,j])
}

#Now set those who missed the cut to NA
for (i in 1:length(player_profile_event$Player_Name)){
  for (j in 1:N_F){
    if(is.na(tournament_day3_F[i,j+1])){
      tournamnet_day4_pos_F[i,j+1] <- NA
    }
  }
}


# Now count number of wins, top 5 and top 10

position_F<- data.frame(player_profile_event$Player_Name) 
position_F$num_1_fin<-apply(tournamnet_day4_pos_F[2:N_F],1,function(x) length(which(x == 1)))

position_F$top_5<- apply(tournamnet_day4_pos_F[2:N_F],1,function(x) length(which(x %in% c(1,2,3,4,5))))

position_F$top_10 <- apply(tournamnet_day4_pos_F[2:N_F],1,function(x) length(which(x %in% c(1,2,3,4,5,6,7,8,9,10))))

# Now calculate the probabilities
position_F$prob_1 <- position_F$num_1_fin/N_F
position_F$prob_top5 <- position_F$top_5/N_F
position_F$prob_top10 <- position_F$top_10/N_F


#Now order by probability of number ten finish
positionordered_F <- position_F[order(position_F$prob_1),c(1,5,6,7)]

#Look at  tail for those most likely to win
tail(positionordered_F)



