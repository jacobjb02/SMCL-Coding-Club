
# Reach Analysis -----

# Extract data for individual participants
getParticipantTraining <- function(group, participant) {
  
  AL_file <- sprintf('data/%s/%s/%s_aligned_training.csv', group, participant, participant)
  RO_file <- sprintf('data/%s/%s/%s_rotated_training.csv', group, participant, participant)
  
  AL_df <- read.csv(file = AL_file, stringsAsFactors = F)
  RO_df <- read.csv(file = RO_file, stringsAsFactors = F)
  
  return(list('aligned' = AL_df,
              'rotated' = RO_df))
  
}

getBaseline <- function(df) {
  
  # str(df)
  df <- df[which(df$trial_num %in% c(31:45)),]
  str(df)
  
  trial_n <- unique(df$trial_num)
  
  for (trial in trial_n) {
    
    trial_df <- df[which(df$trial_num == trial),]
    
    target <- trial_df$targetangle_deg[1]
    
    reachdev <- getReachDeviation(trial_df)
  
      }
}


getReachDeviation <- function(df) {
  
  target <- df$targetangle_deg[1]
  
  X <- df$handx_cm
  Y <- df$handy_cm
  
  # At 1/4 the target distance
  # target distance at 10 cm
  # cut off at 2.5 cm
  
  distances <- sqrt(X^2 + Y^2)
  idx <- which(distances > 2.5)[1]
  
  # print(idx)
  x <- X[idx]
  y <- Y[idx]
  
  th <- (-1*target/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  
  # rotate the coordinates, add the origin back in
norm_sample <- matrix(data = c(x,y, ncol = 2)) %*% R

# print(norm_sample)

reach_dev <- (atan2(norm_sample[2], norm_sample[1]) / pi) * 180

print(reach_dev)

return(c('trial_num' = df$trial_num, 'targetangle_deg' = target, 'reachdeviation_deg' = reach_dev))

}
  


getGroupTraining <- function(group) {
  
  participants <- groupParticipants(group = group)
  
  # print(participants)
  
  for (participant in participants[c(1:3)]) {
    
    participant_df <- getParticipantTraining( group = group,
                                              participant = participant )
    
  # print(participant_df)
    
    
    baseline <- getBaseline(df = participant_df[['aligned']])
    
    
  }
  
}
