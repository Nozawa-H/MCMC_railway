model <- nimbleCode(
    {
        ########model###########
        ########zero inflated###########

        for (i in 1:Nsample) {
          logitp[i] <- phi[phi_ID[i]]
          limlogitp[i] <- min(999,max(logitp[i],-999))
          p[i] <- 1/(1+exp(-(limlogitp[i])))
          col_b[i] ~ dbern(p[i])



          cols[i] ~ dpois(lambda[i])
          lambda[i] <- p[i]*(exp(popu*pop[i]+vg1*veg_1[i]+vg2*veg_2[i]+vg3*veg_3[i]+vg4*veg_4[i]+dm*dem[i]+beta)) + (1-p[i])*(0)
        }

        for(i in 1:nID){
          risk[i] <- p[i]*(exp(popu*pop[Nsample-802+i]+vg1*veg_1[i]+vg2*veg_2[i]+vg3*veg_3[i]+vg4*veg_4[i]+dm*dem[i]+beta)) + (1-p[i])*(0)
        }
        ########prior_FixedEffect###########
        popu ~ dnorm(0,tau_popu)
        tau_popu <- pow(sigma_popu,-2)
        sigma_popu ~ dunif(0, 100)

        vg1 ~ dnorm(0,tau_vg1)
        tau_vg1 <- pow(sigma_vg1,-2)
        sigma_vg1 ~ dunif(0, 100)

        vg2 ~ dnorm(0,tau_vg2)
        tau_vg2 <- pow(sigma_vg2,-2)
        sigma_vg2 ~ dunif(0, 100)

        vg3 ~ dnorm(0,tau_vg3)
        tau_vg3 <- pow(sigma_vg3,-2)
        sigma_vg3 ~ dunif(0, 100)

        vg4 ~ dnorm(0,tau_vg4)
        tau_vg4 <- pow(sigma_vg4,-2)
        sigma_vg4 ~ dunif(0, 100)

        dm ~ dnorm(0,tau_dm)
        tau_dm <- pow(sigma_dm,-2)
        sigma_dm ~ dunif(0, 100)

        beta ~ dnorm(0,tau_beta)
        tau_beta <- pow(sigma_beta,-2)
        sigma_beta ~ dunif(0, 100)



      ##ここがわからない

        phi[1:nID] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nID], tau.phi, zero_mean = 0)
        tau.phi ~ dgamma(1, 0.01)                            # precision of the ICAR component
        sigma2.phi <- pow(tau.phi, -2)

    } #model end
)





#数字で指定
# データ整理
  list_cons <- list(Nsample = length(cols),
                  phi_ID = rep(c(1:802,times=9)),
                  L = length(adjall),
                  weights = rep(1,length(adjall)),#weight,
                  nID = 802,
                  adj = adjall,
                  num = numall,
                  col_b = ifelse(cols>0,1,0)
                  )
list_data <- list(cols=cols,
                  pop=pop,
                  veg_1=veg_1,
                  veg_2=veg_2,
                  veg_3=veg_3,
                  veg_4=veg_4,
                  dem=dem
                  )

  ###数字で指定してみる
                  init1 <- list(
                                popu=0,vg1=0,vg2=0,vg3=0,vg4=0,dm=0,beta=0,
                                sigma_popu=5,sigma_vg1=5,sigma_vg2=5,sigma_vg3=5,sigma_vg4=5,sigma_dm=5,sigma_beta=1,
                                tau.phi=10, phi = rnorm(802,0,1),p=runif(length(cols),0,1)
                              )

                  init2 <- list(
                                popu=0,vg1=0,vg2=0,vg3=0,vg4=0,dm=0,beta=0,
                                sigma_popu=10,sigma_vg1=10,sigma_vg2=10,sigma_vg3=10,sigma_vg4=10,sigma_dm=10,sigma_beta=1,
                                tau.phi=1, phi = rnorm(802,0,1),p=runif(length(cols),0,1)
                              )
                  init3 <- list(
                                popu=0,vg1=0,vg2=0,vg3=0,vg4=0,dm=0,beta=0,
                                sigma_popu=1,sigma_vg1=1,sigma_vg2=1,sigma_vg3=1,sigma_vg4=1,sigma_dm=1,sigma_beta=1,
                                tau.phi=0.1, phi = rnorm(802,0,1),p=runif(length(cols),0,1)
                              )
                  inits <- list(init1, init2, init3)





                  # 監視対象パラメータ
                  parameters <- c("popu", "vg1","vg2","vg3","vg4","dm","beta","phi","risk")

                  ## MCMCの設定
                  nc <- 3 #chain数
                  nb <- 1000 #burn-in回数
                  ni <- nb*2 #iteration数、今回はburn-inの倍にしておく。
                  nt <- nb/1000  #thinの割合。1000になるようにした。


                  start <- Sys.time()
                  out <- nimbleMCMC(code=model,
                                    data=list_data,
                                    constants=list_cons,
                                    inits=inits,
                                    monitors=parameters,
                                    niter = ni,
                                    nburnin = nb,
                                    thin = nt,
                                    nchains = nc,
                                    progressBar = TRUE,
                                    samplesAsCodaMCMC = TRUE,
                                    summary = TRUE,
                                    WAIC = FALSE
                  )

                  end <- Sys.time()
                  print(end-start)
















                  model <- nimbleCode(
                      {
                          ########model###########
                          ########zero inflated###########

                          for (i in 1:Nsample) {
                            logitp[i] <- phi[phi_ID[i]]
                            limlogitp[i] <- min(999,max(logitp[i],-999))
                            p[i] <- 1/(1+exp(-(limlogitp[i])))
                            col_b[i] ~ dbern(p[i])



                            cols[i] ~ dpois(lambda[i])
                            lambda[i] <- p[i]*(exp(popu*pop[i]+vg1*veg_1[i]+vg2*veg_2[i]+vg3*veg_3[i]+vg4*veg_4[i]+dm*dem[i]+beta)) + (1-p[i])*(0)
                          }

                          for(i in 1:nID){
                            risk[i] <- p[i]*(exp(popu*pop[Nsample-802+i]+vg1*veg_1[i]+vg2*veg_2[i]+vg3*veg_3[i]+vg4*veg_4[i]+dm*dem[i]+beta)) + (1-p[i])*(0)
                          }
                          ########prior_FixedEffect###########
                          popu ~ dnorm(0,tau_popu)
                          tau_popu <- pow(sigma_popu,-2)
                          sigma_popu ~ dunif(0, 100)

                          vg1 ~ dnorm(0,tau_vg1)
                          tau_vg1 <- pow(sigma_vg1,-2)
                          sigma_vg1 ~ dunif(0, 100)

                          vg2 ~ dnorm(0,tau_vg2)
                          tau_vg2 <- pow(sigma_vg2,-2)
                          sigma_vg2 ~ dunif(0, 100)

                          vg3 ~ dnorm(0,tau_vg3)
                          tau_vg3 <- pow(sigma_vg3,-2)
                          sigma_vg3 ~ dunif(0, 100)

                          vg4 ~ dnorm(0,tau_vg4)
                          tau_vg4 <- pow(sigma_vg4,-2)
                          sigma_vg4 ~ dunif(0, 100)

                          dm ~ dnorm(0,tau_dm)
                          tau_dm <- pow(sigma_dm,-2)
                          sigma_dm ~ dunif(0, 100)

                          beta ~ dnorm(0,tau_beta)
                          tau_beta <- pow(sigma_beta,-2)
                          sigma_beta ~ dunif(0, 100)



                        ##ここがわからない

                          phi[1:nID] ~ dcar_normal(adj[1:L], weights[1:L], num[1:nID], tau.phi, zero_mean = 0)
                          tau.phi ~ dgamma(1, 0.01)                            # precision of the ICAR component
                          sigma2.phi <- pow(tau.phi, -2)

                      } #model end
                  )





                  length(phi)

                  # データ整理
                    list_cons <- list(Nsample = length(cols),
                                    phi_ID = rep(c(1:802),times=9),
                                    L = length(adjall),
                                    weights = rep(1,length(adjall)),#weight,
                                    nID = 802,
                                    adj = adjall,
                                    num = numall,
                                    col_b = ifelse(cols>0,1,0)
                                    )
                  list_data <- list(cols=cols,
                                    pop=pop,
                                    veg_1=veg_1,
                                    veg_2=veg_2,
                                    veg_3=veg_3,
                                    veg_4=veg_4,
                                    dem=dem
                                    )

                  # 初期値の設定
                  init1 <- list(
                                popu=0,vg1=0,vg2=0,vg3=0,vg4=0,dm=0,beta=0,
                                sigma_popu=5,sigma_vg1=5,sigma_vg2=5,sigma_vg3=5,sigma_vg4=5,sigma_dm=5,sigma_beta=1,
                                tau.phi=10, phi = rnorm(802,0,1),p=runif(length(cols),0,1)
                              )

                  init2 <- list(
                                popu=0,vg1=0,vg2=0,vg3=0,vg4=0,dm=0,beta=0,
                                sigma_popu=10,sigma_vg1=10,sigma_vg2=10,sigma_vg3=10,sigma_vg4=10,sigma_dm=10,sigma_beta=1,
                                tau.phi=1, phi = rnorm(802,0,1),p=runif(length(cols),0,1)
                              )
                  init3 <- list(
                                popu=0,vg1=0,vg2=0,vg3=0,vg4=0,dm=0,beta=0,
                                sigma_popu=1,sigma_vg1=1,sigma_vg2=1,sigma_vg3=1,sigma_vg4=1,sigma_dm=1,sigma_beta=1,
                                tau.phi=0.1, phi = rnorm(802,0,1),p=runif(length(cols),0,1)
                              )
                  inits <- list(init1, init2, init3)

                  # 監視対象パラメータ
                  parameters <- c("popu", "vg1","vg2","vg3","vg4","dm","beta","phi","risk")

                  ## MCMCの設定
                  nc <- 3 #chain数
                  nb <- 2000000 #burn-in回数
                  ni <- nb*2 #iteration数、今回はburn-inの倍にしておく。
                  nt <- nb/1000  #thinの割合。1000になるようにした。


                  start <- Sys.time()
                  out <- nimbleMCMC(code=model,
                                    data=list_data,
                                    constants=list_cons,
                                    inits=inits,
                                    monitors=parameters,
                                    niter = ni,
                                    nburnin = nb,
                                    thin = nt,
                                    nchains = nc,
                                    progressBar = TRUE,
                                    samplesAsCodaMCMC = TRUE,
                                    summary = TRUE,
                                    WAIC = FALSE
                  )

                  end <- Sys.time()
                  print(end-start)


                  res <- as.data.frame(out$summary$all.chain)
                  res$Rhat <- gelman.diag(out$samples, multivariate = FALSE)$psrf[,"Point est."]

                  hist(res$Rhat)
                  res %>% filter(Rhat>1.1)%>% nrow()


                  pdf("2000000.pdf")
                  plot(out$samples)
                  dev.off()



                  print(res)

                  max(res$Rhat)
