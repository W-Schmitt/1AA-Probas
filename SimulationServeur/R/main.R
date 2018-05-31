# Paramètres
serverCapacity = 10
serverCount = 2
mu = 1.9
lambda = 2
time.duration = 200
debug = FALSE
chart = FALSE
stats = TRUE

# Proportions Pi pour les probabilités des requêtes
priorities = c(0.1,0.3,0.6)

# Indicateur de temps
time.current = 0

# Etat de la file d'attente
queue = c(0,0,0)

# Nombre de requêtes servies, par priorité
served = c(0,0,0)

# Requêtes abandonnées, triées par priorité
dropped = c(0,0,0)

# Pas courant
step = 0
if (chart || stats) {
  # Allocation mémoire des tableaux pour les statistiques
  # Le gain de performance est substantiel, cf. rapport
  queueState = c(time.duration*max(lambda, mu))
  priorityQueueState = c(time.duration*max(lambda, mu))
  mediumQueueState = c(time.duration*max(lambda, mu))
  lowQueueState = c(time.duration*max(lambda, mu))
  timeHistory = c(time.duration*max(lambda, mu))
}
queries = 0

nextQuery.time = 0
nextQuery.priority = 0

# Initialisation des temps de service pour les différents serveurs
nextService.times = c(1:serverCount)
for (i in 1:serverCount) {
  nextService.times[i] = rexp(1, mu)
}

# Renvoie le prochain temps de service
nextService.time = function() {
  nextService.server <<- which.min(nextService.times)
  return(min(nextService.times))
}

# Retourne la taille effective de la queue
# Prenant en compte le nombre de serveurs.
effectiveCapacity = function() {
  return(serverCapacity*serverCount)
}

query = function() {
  # TODO (?) : traiter les requêtes de priorité supérieure à celles présentes dans la queue
  if (sum(queue) >= effectiveCapacity()) {
    dropped[nextQuery.priority] <<- dropped[nextQuery.priority] + 1
  } else {
    queue[nextQuery.priority] <<- queue[nextQuery.priority] + 1
  }

  # Quoi qu'il arrive, on génère la requête suivante
  prioritySelector = runif(1)

  # Sélection de la priorité
  if (prioritySelector < priorities[1]) {
    nextQuery.priority <<- 1
  } else if (prioritySelector < (priorities[1] + priorities[2])) {
    nextQuery.priority <<- 2
  } else {
    nextQuery.priority <<- 3
  }
  nextQuery.time <<- nextQuery.time + rexp(1, lambda)
}

# Sert une requête en respectant les priorités
service = function() {
  for (i in 1:length(priorities)) {
    if (queue[i] > 0) {
      queue[i] <<- queue[i] - 1
      served[i] <<- served[i] + 1
      break
    }
  }
  nextService.times[nextService.server] <<- nextService.times[nextService.server] + rexp(1, mu)
}

main <- function() {
  query()
  while (time.current < time.duration) {
    if (time.current == nextQuery.time) {
      query()
    }
    if (time.current == nextService.time()) {
      for (i in 1:serverCount) {
        service()
      }
    }

    # Sélection de l'étape suivante : un temps de service ou un temps de requête
    if (nextQuery.time < nextService.time()) {
      time.current <<- nextQuery.time
    } else {
      time.current <<- nextService.time()
    }
    queries <<- queries + sum(queue)
    queueState[step] <<- sum(queue)
    lowQueueState[step] <<- queue[3]
    mediumQueueState[step] <<- queue[2]
    priorityQueueState[step] <<- queue[1]
    timeHistory[step] <<- time.current
    step = step + 1
  }
  print("Served")
  print(served)
  print("Dropped")
  print(dropped)

  if (chart) {
    plot(x=timeHistory, y=queueState, type="l", xlab="Temps", ylab="Requêtes")
    lines(x=timeHistory, y=priorityQueueState, col="red", ylab="Priorité haute")
    lines(x=timeHistory, y=mediumQueueState, col="green", ylab="Priorité moyenne")
    lines(x=timeHistory, y=lowQueueState, col="blue", ylab="Priorité basse")
    legend("topleft",legend=c("Requêtes totales","Priorité haute","Priorité moyenne","Priorité basse"), col=c("black", "red","green","blue"),lty=c(1,1,1,1), ncol=1)
  }
  if (stats) {
    print("STATISTIQUES")
    rho <- lambda/mu
    simulated.avgQueries = queries/step
    simulated.dropRate = sum(dropped)/step
    theoretical.avgQueries = (((1-(rho^(effectiveCapacity()-1))) / (1-rho)) - effectiveCapacity()*(rho^(effectiveCapacity()))) * (rho / (1-(rho^(effectiveCapacity()+1))))
    theoretical.dropRate = ((1-rho) / (1-rho^(effectiveCapacity()+1))) * rho^(effectiveCapacity())
    gap.avgQueries = simulated.avgQueries / theoretical.avgQueries
    gap.dropRate = simulated.dropRate / theoretical.dropRate
    print("Moyenne de requêtes")
    print(paste("- Théorie :\t", theoretical.avgQueries))
    print(paste("- Simulation :\t", simulated.avgQueries))
    print(paste("- Rapport :\t", gap.avgQueries))

    print("Taux de rejet")
    print(paste("- Théorie :", theoretical.dropRate))
    print(paste("- Simulation :", simulated.dropRate))
    print(paste("- Rapport :", gap.dropRate))

    print(paste("Moyenne des priorités hautes", mean(priorityQueueState)))
    print(paste("Moyenne des priorités moyennes", mean(mediumQueueState)))
    print(paste("Moyenne des priorités basses", mean(lowQueueState)))
  }
}

main()
