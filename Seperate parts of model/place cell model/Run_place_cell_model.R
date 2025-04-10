
N_pc <- 211 #Population of place cells [100..300]
N_ac <- 36 #Population of action cells [25..50]

variable_platform <- 1
plot_trajectories <- 0 #yes - 1, no - 0
plot_cognitive_maps <- 0 #yes - 1, no - 0
pln <- plot_trajectories + plot_cognitive_maps
Nruns <- 25 #how many runs to run if not plotting anything

pool_diameter <- 1.4 #Maze diameter (\phi) in metres (m)
platform_radius <- 0.06 #Platform radius (m)
sigma_pc <- 0.1 #place cell sigma (standard deviation), in meters [0.05..0.2]
sigma_ac <- 2 #action cell sigma (standard deviation), in action cells [1..3]

etdecay <- 0.83 #Eligibility trace decay (lambda) [0.75..0.95] LESS THAN GAMMA!
beta <- 6 #Exploration-exploitation factor (\beta) [0.5..12]
alpha <- 0.01 #Learning rate (\alpha) [0.005..0.02]
gamma <- 0.85 #Discount factor (\gamma) [0.75..0.95]

Vdecay <- 0.82 #velocity decay [0.75..0.95]
ac_const <- 0.02 #acceleration const [0.01..0.03]
Wnoise <- 0.0004 #Weight noise [0.0001..0.0007]
Wmult <- 0.1 #Weight multiplier [0.05..0.15]
hitwall <- 0.5 #punishment for hitting the wall [0..1]
speed <- 0.175 #mouse speed (m/s) [0.1..0.25]

Ntrials <- 4 #number of trials per day
Ndays <- 8 #number of days

platform_positions <- list(
  c(cos(-pi/4)*pool_diameter/4, sin(-pi/4)*pool_diameter/4),
  c(cos(pi/4)*pool_diameter/4, sin(pi/4)*pool_diameter/4)
)

if (pln > 0.5) {
  PMs <- array(rep(0, 5 * Ndays * Ntrials), c(5, Ndays, Ntrials))
} else {
  PMs <- array(rep(0, 5 * Ndays * Ntrials * Nruns), c(5, Ndays, Ntrials, Nruns))
}

platform_x <- cos(-pi / 4) * pool_diameter / 4
platform_y <- sin(-pi / 4) * pool_diameter / 4

strad <- pool_diameter / 2 * 0.85
starting_xs <- strad * c(cos(pi / 6), cos(pi / 3), cos(7 * pi / 6), cos(4 * pi / 3))
starting_ys <- strad * c(sin(pi / 6), sin(pi / 3), sin(7 * pi / 6), sin(4 * pi / 3))
th <- (0:100) / 50 * pi

if (pln > 0.5) {
  weights <- matrix(runif(N_pc * N_ac), nrow = N_pc) * Wmult
  PC_x <- rep(0, N_pc)
  PC_y <- rep(0, N_pc)
  
  for (i in 1:N_pc) {
    PC_x[i] <- (runif(1) - 0.5) * pool_diameter
    PC_y[i] <- (runif(1) - 0.5) * pool_diameter
    while ((PC_x[i]^2 + PC_y[i]^2 > (pool_diameter / 2)^2)) {
      PC_x[i] <- (runif(1) - 0.5) * pool_diameter
      PC_y[i] <- (runif(1) - 0.5) * pool_diameter
    }
  }
  
  for (day in 1:Ndays) {
    idxs <- sample(4)
    for (trial in 1:Ntrials) {
      if (variable_platform == 1){
        whichplatform = sample(c(1, -1), 1)
        platform_x = abs(platform_x) * whichplatform
        platform_y = abs(platform_y) * whichplatform
      }
      
      idx <- idxs[trial]
      starting_x <- starting_xs[idx]
      starting_y <- starting_ys[idx]
      
      modresults <- run_trial_place(weights, Wmult, sigma_pc, sigma_ac, PC_x, PC_y, Vdecay, ac_const, beta, etdecay, alpha, gamma, Wnoise, platform_x, platform_y, starting_x, starting_y, speed, hitwall)
      weights <- modresults[[1]]
      track_x <- modresults[[2]]
      track_y <- modresults[[3]]
      
      PMs[1, day, trial] <- modresults[[9]]
      PMs[2, day, trial] <- modresults[[6]]
      PMs[3, day, trial] <- modresults[[8]][4] * 100
      PMs[4, day, trial] <- modresults[[8]][2] * 100
      PMs[5, day, trial] <- modresults[[7]] * 100

      if (plot_trajectories) {
        plot(pool_diameter / 2 * cos(th), pool_diameter / 2 * sin(th), type = "l", xlab = paste("day", day, ", trial", trial), ylab = "trajectory")
        lines(track_x, track_y, type = "l")
        lines(platform_x + platform_radius * cos(th), platform_y + platform_radius * sin(th), type = "l")
      }
    }
  }
}else { 
# run multiple times without plotting!

for (reps in 1:Nruns){
#Generate initial weights for each run
weights <- matrix(runif(N_pc*N_ac), nrow = N_pc)*Wmult

#Generate place cells for each run
PC_x <- rep(0,N_pc) #1xN_pc matrix containing the x = 0 coordinate for each place cell
PC_y <- rep(0,N_pc) #1xN_pc matrix containing the y = 0 coordinate for each place cell
for (i in 1:N_pc) {
  #For each place cell:
  PC_x[i] <- (runif(1) - 0.5)*pool_diameter#Random positions of place cells
  PC_y[i] <- (runif(1) - 0.5)*pool_diameter
  while ((PC_x[i]^2 + PC_y[i]^2 > (pool_diameter/2)^2)){
    #Checks for out of bounds
    PC_x[i] <- (runif(1) - 0.5)*pool_diameter
    PC_y[i] <- (runif(1) - 0.5)*pool_diameter
  }
}

for (day in 1:Ndays){
  idxs = sample(4)  #randomly choose 4 starting locations
    for (trial in 1:Ntrials){
      if (variable_platform == 1){
        whichplatform = sample(c(1, -1), 1)
        platform_x = abs(platform_x) * whichplatform
        platform_y = abs(platform_y) * whichplatform
      }
      idx <- idxs[trial] #take each location
      starting_x <- starting_xs[idx]
      starting_y <- starting_ys[idx]

      modresults <- run_trial_place (weights, Wmult, sigma_pc, sigma_ac, PC_x, PC_y, Vdecay, ac_const, beta, etdecay, alpha, gamma, Wnoise, platform_x, platform_y, starting_x, starting_y, speed, hitwall)
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

