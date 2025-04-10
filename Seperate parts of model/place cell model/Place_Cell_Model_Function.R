run_trial_place <- function(weights0, Wmult, sigma_pc, sigma_ac, PC_x, PC_y, Vdecay, ac_const, beta, etdecay, lrate, discf, noise, platform_x, platform_y, starting_x, starting_y, speed, wall_pun) {
  # FIXED PARAMETERS OF THE EXPERIMENT
  
  pool_diameter <- 1.4 #Maze diameter in metres (m)
  platform_radius <- 0.06 #Platform radius
  
  N_pc <- 211 #Population of place cells
  N_ac <- 36 #Population of action cells
  which <- 0
  
  dist <- 0
  wall_zone <- 0
  quadrants <- c(0,0,0,0) #Percentage spent on each quadrant
  
  weights <- weights0 #Initialize modifiable weights
  
  el_tr <- matrix(rep(0, N_pc*N_ac), nrow = N_pc) #Initialize eligibility traces matrix
  
  #Initialize trajectories
  track_x <- starting_x #Current position of trajectory is equal to the starting location of the animal
  track_y <- starting_y
  vel_x <- 0
  vel_y <- 0
  
  # NAVIGATION LOOP
  while ((track_x[length(track_x)] - platform_x)^2 + (track_y[length(track_y)] - platform_y)^2 > platform_radius^2)
  {
    weights <- weights*(1-noise) + matrix(runif(N_pc*N_ac), nrow = N_pc)*Wmult*noise
    
    #Calculate PC activation
    PC_activation <- rep(0, N_pc)
    for (i in 1:N_pc){
      PC_activation[i] <- exp(-((track_x[length(track_x)] - PC_x[i])^2 + (track_y[length(track_y)] - PC_y[i])^2)/(2*sigma_pc^2))
    }
    
    #Calculate AC activation (i.e. value of the action, Q)
    if (length(track_x) > 1){
      prevQ <- AC_activation[which] #Displays the Q value before movement
    }
    
    AC_activation <- PC_activation %*% weights
    
    # Action selection with numerical stability
    Q <- AC_activation
    Q[is.na(Q)] <- 0                      
    Q <- Q - max(Q, na.rm = TRUE)       

    ACsel <- exp(beta * Q)
    ACsel[is.na(ACsel)] <- 0             
    ACsel <- ACsel / (sum(ACsel) + 1e-8) 

    # Sample action based on softmax probabilities
    ASrand <- runif(1)
    ASsum <- 0
    which <- 1
    while (which < N_ac && !is.na(ASsum) && ASsum < ASrand) {
      ASsum <- ASsum + ACsel[which]
      which <- which + 1
    }
    
    #Eligibility traces
    el_tr <- el_tr * etdecay
    
    for (j in 1:N_ac){
      itmp <- min(abs(j-which), N_ac-abs(j-which))
      actgaus <- exp(-(itmp*itmp)/(2*sigma_ac*sigma_ac))
      el_tr[,j] <- el_tr[,j] + actgaus*AC_activation[j]*t(t(PC_activation))
    }
    
    vel_x = c(vel_x, (vel_x[length(vel_x)]+ac_const*cos(which/N_ac*2*pi))*Vdecay)
    vel_y = c(vel_y, (vel_y[length(vel_y)]+ac_const*sin(which/N_ac*2*pi))*Vdecay)
    #velocity per time step (not second)
    track_x = c(track_x, track_x[length(track_x)]+vel_x[length(vel_x)])
    track_y = c(track_y, track_y[length(track_y)]+vel_y[length(vel_y)])
    
    #Check if not out of bounds, reset location & speed if so
    if (track_x[length(track_x)]^2 + track_y[length(track_y)]^2 > (pool_diameter/2)^2)
    {
      ratio = (track_x[length(track_x)]^2 + track_y[length(track_y)]^2)/((pool_diameter/2)^2)
      track_x[length(track_x)] = track_x[length(track_x)]/sqrt(ratio)
      track_y[length(track_y)] = track_y[length(track_y)]/sqrt(ratio)
      vel_x[length(vel_x)] = track_x[length(track_x)] - track_x[length(track_x)-1]
      vel_y[length(vel_y)] = track_y[length(track_y)] - track_y[length(track_y)-1]
    }
    
    
    if (length(track_x) > 2)
    { if ((track_x[length(track_x)]  - platform_x)^2 + (track_y[length(track_y)]  - platform_y)^2 < platform_radius^2)
    { rew = 10 } #found platform - reward
      else if (track_x[length(track_x)]^2+track_y[length(track_y)]^2 > (0.99*pool_diameter/2)^2)
      { rew = -wall_pun } #hit wall - punishment
      else
      { rew = 0 } #didn't find - no reward
      
      currQ = AC_activation[which]
      tderr = rew + discf*currQ - prevQ #temporal difference error
      weights = pmax(weights + lrate*tderr*el_tr, 0)
    }
    
    laststep = sqrt((track_x[length(track_x)]-track_x[length(track_x)-1])^2 + (track_y[length(track_y)]-track_y[length(track_y)-1])^2)
    dist = dist + laststep
    
    if (track_x[length(track_x)]^2 + track_y[length(track_y)]^2 > 0.8*(pool_diameter/2)^2)
    { wall_zone = wall_zone + 1 }
    else if (track_x[length(track_x)] > 0 && track_y[length(track_y)] > 0)
    { quadrants[1] = quadrants[1] + 1 }
    else if (track_x[length(track_x)] < 0 && track_y[length(track_y)] > 0)
    { quadrants[2] = quadrants[2] + 1 }
    else if (track_x[length(track_x)] < 0 && track_y[length(track_y)] < 0)
    { quadrants[3] = quadrants[3] + 1 }
    else
    { quadrants[4] = quadrants[4] + 1 }
    
    if (length(track_x) > 100) # evaluate latency only after 100+ steps to be accurate
    { speed_ts = mean(sqrt((vel_x[-1]^2+vel_y[-1]^2))) # speed in meters/time step
    latency = (length(track_x)-1) * speed_ts / speed # convert to seconds
    if (latency > 60) # if more than a minute, stop
    { break }
    }
    
  }
  
  latency <- length(track_x)-1 # latency in time steps
  wall_zone <- wall_zone/latency
  quadrants <- quadrants/latency
  speed_ts <- mean(sqrt((vel_x[-1]^2+vel_y[-1]^2))) # speed in meters/time step
  
  latency <- latency * speed_ts / speed # latency in seconds
  return(list(weights, track_x, track_y, vel_x, vel_y, dist, wall_zone, quadrants, latency))
}
