library(tidyr)
library(ggplot2)


N_dc <- 30 #Population of distance cells [100..300]
N_ac <- 36 #Population of action cells [25..50]

plot_trajectories <- 0 #yes - 1, no - 0
plot_cognitive_maps <- 0 #yes - 1, no - 0
pln <- plot_trajectories + plot_cognitive_maps
Nruns <- 25 #how many runs to run if not plotting anything
variable_platform <- 1 #the platform is variable - 1, the platform is fixed - 0

pool_diameter <- 1.4 #Maze diameter (\phi) in metres (m)
platform_radius <- 0.06 #Platform radius (m)
sigma_dc <- 0.1 #place cell sigma (standard deviation), in meters [0.05..0.2]
sigma_ac <- 2 #action cell sigma (standard deviation), in action cells [1..3]

etdecay <- 0.83 #Eligibility trace decay (lambda) [0.75..0.95] LESS THAN GAMMA!
beta <- 6 #Exploration-exploitation factor (\beta) [0.5..12]
alpha <- 0.01 #Learning rate (\alpha) [0.005..0.02]
gamma <- 0.85 #Discount factor (\gamma) [0.75..0.95]

Vdecay <- 0.82 #velocity decay [0.75..0.95]
ac_const <- 0.02 #acceleration const [0.01..0.03]
Wnoise <- 0.0002 #Weight noise [0.0001..0.0007]
Wmult <- 0.1 #Weight multiplier [0.05..0.15]
hitwall <- 0.5 #punishment for hitting the wall [0..1]
speed <- 0.175 #mouse speed (m/s) [0.1..0.25]



Ntrials <- 4 #number of trials per day
Ndays <- 8 #number of days


#performance measures to compute: latency, distance, time in target quadrant, opposite quadrant, and wall zone
if (pln > 0.5) #if any plots
{ PMs <- array(rep(0,5*Ndays*Ntrials), c(5,Ndays,Ntrials))
} else {
  PMs <- array(rep(0,5*Ndays*Ntrials*Nruns), c(5,Ndays,Ntrials,Nruns)) }
#multiple runs


#Platform coordinates:
platform_x <- cos(-pi/4)*pool_diameter/4 #x coordinate
platform_y <- sin(-pi/4)*pool_diameter/4 #y coordinate

#Starting locations of the modeled animal (4 different ones):
strad <- pool_diameter/2*0.85 #15% of maze radius to the wall
starting_xs <- strad * c(cos(pi/6), cos(pi/3), cos(7*pi/6), cos(4*pi/3)) #x coordinates
starting_ys <- strad * c(sin(pi/6), sin(pi/3), sin(7*pi/6), sin(4*pi/3)) #y coordinates

th <- (0:100)/50*pi #for plotting circles :)

if (pln > 0.5) {
  
#Generate initial weights
weights <- matrix(runif(N_dc*N_ac), nrow = N_dc)*Wmult

#Generate distance cells
DC <- rep(0,N_dc)

for (i in 1:N_dc) {
  #For each place cell:
  DC[i] <- runif(1)*(pool_diameter/2) #Random positions of distance cells

}

par(mfrow=c(1,4))


for (day in 1:Ndays) {
  idxs = sample(4) #randomly choose 4 starting locations
  for (trial in 1:Ntrials){
    if (variable_platform==1){ #the variable platform generation
      whichplatform=sample(c(1,-1),1)
      platform_x = platform_x*whichplatform
      platform_y = platform_y*whichplatform
    }
    
    
    idx <- idxs[trial] #take each location
    starting_x <- starting_xs[idx]
    starting_y <- starting_ys[idx]
    
    modresults <- run_trial_distance(weights, Wmult, sigma_dc, sigma_ac, DC, Vdecay, ac_const, beta, etdecay, alpha, gamma, Wnoise, platform_x, platform_y, starting_x, starting_y, speed, hitwall)
    #run trial
    weights <- modresults[[1]]
    track_x <- modresults[[2]]
    track_y <- modresults[[3]]
    vel_x <- modresults[[4]]
    vel_y <- modresults[[5]]
    
    #        weights <- wres
    
    PMs[1,day,trial] <- modresults[[9]] #latency
    PMs[2,day,trial] <- modresults[[6]] #dist
    PMs[3,day,trial] <- modresults[[8]][4]*100 #target quadrant
    PMs[4,day,trial] <- modresults[[8]][2]*100 #opposite quadrant
    PMs[5,day,trial] <- modresults[[7]]*100 #wall zone
    #record performance measures
    
    if (plot_trajectories&((day==1&trial==1)|(day==8&trial==4)))
    {
      #plot the maze
      plot(pool_diameter/2*cos(th),pool_diameter/2*sin(th),type = "l", xlab = paste("day ",day,", trial ",trial), ylab = "trajectory")
      #plot the trajectory
      lines(track_x, track_y, type = "l")
      #plot the platform
      lines(platform_x+platform_radius*cos(th),platform_y+platform_radius*sin(th),type = "l")
    }
    
    if (plot_cognitive_maps&((day==1&trial==1)|(day==8&trial==4))){
      #plot the maze
      plot(pool_diameter/2*cos(th),pool_diameter/2*sin(th),type = "l",xlab = paste("day ",day,", trial ",trial), ylab = "cognitive map")
      #plot the cognitive map
      for (x in (-3:3)*(pool_diameter/6)){
        for (y in (-3:3)*(pool_diameter/6)){
          if (x^2 + y^2 <= (pool_diameter/2)^2){
            x2 = x
            y2 = y
            dist.to.wall = pool_diameter/2-sqrt(x2^2+y2^2)
            for (k in 1:N_ac){
              DC_activation <- rep(0,N_dc)
              for (i in 1:N_dc){
                DC_activation[i] <- exp(-(dist.to.wall -DC[i])^2/(2*sigma_dc^2))
              }
              #Calculate AC activation (i.e. value of the action)
              AC_activation <- rep(0,N_ac)
              for (i in 1:N_ac){
                for (j in 1:N_dc){
                  AC_activation[i] <- AC_activation[i] + DC_activation[j]*weights[j,i]
                }
              }
              #direction of AC activation
              if(x>=0){
                central.angle = atan(y/x)
              } else {
                central.angle = pi+atan(y/x)
              }
              
              moving.dir = pi+central.angle+k/N_ac*2*pi
              
              x2 <- c(x2, x + (AC_activation[k]/10)*cos(moving.dir))
              y2 <- c(y2, y + (AC_activation[k]/10)*sin(moving.dir))
              #                 line([x x2],[y y2],'Color',[k/N_ac 0 1-k/N_ac])
            }
            lines(x2,y2,type = "l",col = "blue")
          }
        }
      }
      # plot the platform
      lines(platform_x+platform_radius*cos(th),platform_y+platform_radius*sin(th),type = "l")
    }
  }
}


}else {
  # run multiple times without plotting!
  
  for (reps in 1:Nruns){
    #Generate initial weights for each run
    weights <- matrix(runif(N_dc*N_ac), nrow = N_dc)*Wmult
    print(reps)
    #Generate distance cells for each run
    DC <- rep(0,N_dc) #1xN_dc matrix containing the each place cell
    for (i in 1:N_dc) {
      #For each place cell:
      # DC[i] <- runif(1)*(pool_diameter/2)#Random positions of distance cells
      DC[i] <- runif(1)*(pool_diameter)#Random positions of distance cells
    }
    
    for (day in 1:Ndays){
      idxs = sample(4)  #randomly choose 4 starting locations
      for (trial in 1:Ntrials){
        
        if (variable_platform==1){ #the variable platform generation
          whichplatform=sample(c(1,-1),1)
          platform_x = platform_x*whichplatform
          platform_y = platform_y*whichplatform
        }
        
        idx <- idxs[trial] #take each location
        starting_x <- starting_xs[idx]
        starting_y <- starting_ys[idx]
        
        modresults <- run_trial_distance (weights, Wmult, sigma_dc, sigma_ac, DC, Vdecay, ac_const, beta, etdecay, alpha, gamma, Wnoise, platform_x, platform_y, starting_x, starting_y, speed, hitwall)
        #run trial
        weights <- modresults[[1]]
        
        PMs[1,day,trial,reps] <- modresults[[9]] #latency
        PMs[2,day,trial,reps] <- modresults[[6]] #dist
        PMs[3,day,trial,reps] <- modresults[[8]][4]*100 #target quadrant
        PMs[4,day,trial,reps] <- modresults[[8]][2]*100 #opposite quadrant
        PMs[5,day,trial,reps] <- modresults[[7]]*100 #wall zone
        #record performance measures
      }
    }
  }
}


if(pln > 0.5){
  latency = PMs[1,,]
  dist = PMs[2,,]
  target_quadrant = PMs[3,,]
  opposite_quadrant = PMs[4,,]
  wall_zone = PMs[5,,]
  
  latency_stats <- apply(latency, c(1,2), function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x))))


  day <- rep(1:Ndays, each = Ntrials)
  trial <- rep(1:Ntrials, times = Ndays)
  mean_latency <- as.vector(latency_stats[1,,])
  se_latency <- as.vector(latency_stats[2,,])

  latency_df <- data.frame(
    Day = day,
    Trial = trial,
    MeanLatency = mean_latency,
    SE = se_latency
  )


  latency_day <- latency_df %>%
    group_by(Day) %>%
    summarise(
      latency = mean(MeanLatency),
      se = sd(MeanLatency) / sqrt(n())
    )


  ggplot(latency_day, aes(x = Day, y = latency)) +
    geom_line(color = "steelblue") +
    geom_point(size = 2, color = "steelblue") +
    geom_errorbar(aes(ymin = latency - se, ymax = latency + se), width = 0.2, alpha = 0.4) +
    labs(
      title = "Latency Across Training Days (Place Cell Model)",
      x = "Training Day",
      y = "Average Latency (s)"
    ) +
    theme_minimal()
  
  
}else{
  latency = PMs[1,,,]
  dist = PMs[2,,,]
  target_quadrant = PMs[3,,,]
  opposite_quadrant = PMs[4,,,]
  wall_zone = PMs[5,,,]
  day = c(1:Ndays)
  latency = cbind(day,latency)
  dist = cbind(day,dist)
  target_quadrant = cbind(day,target_quadrant)
  opposite_quadrant = cbind(day,opposite_quadrant)
  wall_zone = cbind(day,wall_zone)
  all = cbind(latency,dist[,2],target_quadrant[,2],opposite_quadrant[,2],wall_zone[,2])
  colnames(all)=c("day","latency","dist","target_quadrant","opposite_quadrant","wall_zone")
  
  write.csv(all,"radius_1 vari.csv")
}
