remotes::install_github("mrc-ide/odin.dust",upgrade = TRUE,force = TRUE)
library("odin.dust")
library("odin")
library("patchwork")
library('mcstate')
library(didehpc)
library(pkgdepends)
library(dplyr)
library("coda")
library(binom)
library(ggplot2)
library(bayesplot)
library(tibble)
library(zoo)
library(lubridate)

source("run_pmcmc_season.R")
source("model_parameters.R")
source("equilibrium-init-create-season.R")
source("utils.R")

data_raw_bf_banfora <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_BF_pg_banfora.RDS')
data_shift <- data_raw_bf_banfora %>%
  mutate(t=t+600)
data_raw_bf_banfora$t <- data_raw_bf_banfora$t + 600
preamble <- data.frame(t=seq(30,600,30))
data_raw_preamb <- plyr::rbind.fill(preamble,data_raw_bf_banfora)

data_raw <- data_raw_bf_banfora
test <- run_pmcmc(data_raw = data_raw_bf_banfora,
          n_particles = 200,
          proposal_matrix = matrix(c(0.0336,-0.000589,-0.000589,0.049420),nrow=2),
          max_EIR=1000,
          max_steps = 1e7,
          atol = 1e-5,
          rtol = 1e-6,
          n_steps = 5,
          n_threads = 4,
          country = 'Burkina Faso',
          admin_unit = 'Cascades')


1 - coda::rejectionRate(as.mcmc(to_return$mcmc))
coda::effectiveSize(as.mcmc(to_return$mcmc))
proposal <-  cov(to_return$pars)
windows(60,50)
plot(as.mcmc(to_return$mcmc))
plot_particle_filter(test$history,true_history=data_shift,times=data_shift$t)

cis <- addCIs(df=data_raw,Ys=data_raw$positive,Ns=data_raw$tested)
to_return$history

matplot(data_raw$t, t(to_return$history[4, , -1]), type = "l",
        xlab = "Time", ylab = "EIR",
        col = "#A6CEE3", lty = 1, ylim = range(to_return$history[4:5, , -1],na.rm=TRUE))
matlines(data_raw$t, t(to_return$history[5, , -1]))
matpoints(data_raw$t, t(log(to_return$history[2, , -1])))

##Set up cluster##
root <- "T:/jth/contexts"
sources <- c("run_pmcmc.R","run_pmcmc.R",
             "model_parameters.R","equilibrium-init-create-stripped.R")
sources_seas <- c("run_pmcmc_season.R",
             "model_parameters.R","equilibrium-init-create-season.R","utils.R")
admins <- readRDS("T:/jth/admin_units_seasonal.rds")

ctx <- context::context_save("T:/jth/contexts", sources = sources_seas,
                             packages = c('statmod','coda','dplyr','zoo','lubridate'),
                             package_sources = conan::conan_sources(c("mrc-ide/odin.dust",'mrc-ide/mcstate',
                                                                      "mrc-ide/deterministic-malaria-model")))

config_16 <- didehpc::didehpc_config(cores = 16, parallel = TRUE)
obj_16 <- didehpc::queue_didehpc(ctx,config = config_16)
seas_obj <- didehpc::queue_didehpc(ctx,config = config_16)
seas_obj$login()
seas_obj$cluster_load(TRUE)
seas_obj$enqueue()

test1 <- seas_obj$enqueue(run_pmcmc(data_raw = data_raw_bf_banfora,
                                    n_particles = 200,
                                    proposal_matrix = matrix(c(0.0336,-0.000589,-0.000589,0.049420),nrow=2),
                                    max_EIR=1000,
                                    max_steps = 1e7,
                                    atol = 1e-5,
                                    rtol = 1e-6,
                                    n_steps = 100,
                                    n_threads = 16,
                                    country = 'Burkina Faso',
                                    admin_unit = 'Cascades',
                                    preyears = 2))
test1$status()
test1$log()

result_seas <- test1$result()

1 - coda::rejectionRate(as.mcmc(result_seas$mcmc))
coda::effectiveSize(as.mcmc(result_seas$mcmc))
proposal <-  cov(result_seas$pars)
windows(60,50)
plot(as.mcmc(result_seas$mcmc))
plot_particle_filter(result_seas$history,true_history=data_shift,times=data_shift$t)

cis <- addCIs(df=data_raw,Ys=data_raw$positive,Ns=data_raw$tested)
result_seas$history

matplot(data_raw_preamb$t, t(result_seas$history[4, , -1]), type = "l",
        xlab = "Time", ylab = "EIR",
        col = "#A6CEE3", lty = 1,
        xlim = c(0,max(data_shift$t)),
        ylim = range(result_seas$history[4:5, , -1],na.rm=TRUE))
matlines(data_raw_preamb$t, t(result_seas$history[5, , -1]))
matpoints(data_raw_preamb$t, t(log(result_seas$history[2, , -1])))



##Original model for comparison
sources <- c("run_pmcmc.R",
             "model_parameters.R","equilibrium-init-create-stripped.R")

ctx_orig <- context::context_save("T:/jth/contexts", sources = sources,
                             packages = c('statmod','coda','lubridate'),
                             package_sources = conan::conan_sources(c("mrc-ide/odin.dust",'mrc-ide/mcstate',
                                                                      "mrc-ide/deterministic-malaria-model")))

config_16_orig <- didehpc::didehpc_config(cores = 16, parallel = TRUE)
orig_obj <- didehpc::queue_didehpc(ctx_orig,config = config_16_orig)
orig_obj$login()

test_orig <- orig_obj$enqueue(run_pmcmc(data_raw = data_raw_bf_banfora,
                                    n_particles = 200,
                                    proposal_matrix = matrix(c(0.0336,-0.000589,-0.000589,0.049420),nrow=2),
                                    max_EIR=1000,
                                    max_steps = 1e7,
                                    atol = 1e-5,
                                    rtol = 1e-6,
                                    n_steps = 100,
                                    n_threads = 16))
test_orig$status()
test_orig$log()

result_orig <- test_orig$result()

1 - coda::rejectionRate(as.mcmc(result_orig$mcmc))
coda::effectiveSize(as.mcmc(result_orig$mcmc))
proposal <-  cov(result_orig$pars)
windows(60,50)
plot(as.mcmc(result_orig$mcmc))
plot_particle_filter(result_orig$history,true_history=data_raw_bf_banfora,times=data_raw_bf_banfora$t)

cis <- addCIs(df=data_raw,Ys=data_raw$positive,Ns=data_raw$tested)
result_orig$history

matplot(data_raw$t, t(result_orig$history[4, , -1]), type = "l",
        xlab = "Time", ylab = "EIR",
        col = "#A6CEE3", lty = 1, ylim = range(result_orig$history[4:5, , -1],na.rm=TRUE))
matlines(data_raw$t, t(result_orig$history[5, , -1]))
matpoints(data_raw$t, t(log(result_orig$history[2, , -1])))
