model = function (
    lam = NULL,
    CL   = NULL,
    VL   = NULL,
    VF  = NULL,
    CF  = NULL,
    dem = NULL,
    pL.init  = NULL,
    pF.init  = NULL,
    n.gen = 100,
    leader.sway = 1,
    pref.can.change = T) {

  # variables

  # running from scratch
  #
  # rm(list = ls())
  # lam = 10
  # C   = 10
  # V   = 9
  # dv  = 0.75
  # dc  = 0.75
  # dem = 0.2
  # pL.init  =  0.01
  # pF.init  =  0.01
  # n.gen = 100
  # leader.sway = 1
  # pref.can.change = T

  # running from shiny. plot 1
  #
  # i = 20
  # lam       = mat()$lam[i]
  # C          = mat()$C   [i]
  # V          = mat()$V   [i]
  # dv         = mat()$dv  [i]
  # dc         = mat()$dc  [i]
  # pL.init    = mat()$pL.init[i]
  # pF.init    = mat()$pF.init[i]
  # n.gen      = input$n.gen
  # leader.sway = 1
  # pref.can.change = T
  # dem        = 0.2
  # dem = 0



  pLs = NULL
  pFs = NULL
  ps = NULL
  pL = pL.init
  pF = pF.init

  k=0.08


  for ( i in 1:n.gen){
    # for ( i in 1:7){
    #dem = 0.5
    # i = 1

    delta = 0.7*exp(-k * i)

    # vote on hawkishness
    p =   ( ( pL * leader.sway * lam ) + # proportion of leaders playing hawk * leader sway * Num. leaders
              (pF * dem * (1-lam)) ) / # what followers want * how much they're allowed per follower * number of followers
      ( ( leader.sway * lam) + # total say that everyone has, split into leaders
          ( dem * (1-lam))) # and followers

    # save before any changes are made
    pLs = c(pLs,pL) ## preference of leaders
    pFs = c(pFs,pF) ## preference of followers
    ps  = c(ps ,p ) ## played strategy by group

    # leader outcome
    lWh = p*( ( VL - CL)/2 ) +  ((1-p) * VL) ## leader payoff if hawk
    lWd = p*0 + ( (1-p)* VL/2) ## leader payoff if dove

    # follower outcome
    fWh =  (p*((VF - CF)/2)) +  ((1-p) * VF) ## follower payoff if hawk
    fWd =  p*0 + ((1-p)* VF/2) ## follower payoff if dove

    if ( pref.can.change ){
      # pL = pL * lWh/lWd  ## update leader preference
      # pF = pF * fWh/fWd ## update follower preference

      pL = pL + sign( lWh - lWd) * delta  ## update leader preference
      pF = pF + sign( fWh - fWd) * delta ## update follower preference
    }

    if ( pL > 1) {
      pL = 1
    }
    if ( pF > 1) {
      pF = 1
    }
    if ( pL < 0) {
      pL = 0
    }
    if ( pF < 0) {
      pF = 0
    }
  }
  # plot(pFs, type = "l")
  # plot(pLs, type = "l")
  #plot(ps, type = "l")

  len = length(pLs)
  vec = c(  pL = pLs[(len)],
            pF = pFs[(len)],
            p  = ps[(len)])
  names (  vec) = c ( "pL", "pF","p")
  vec
  return ( vec )
}


