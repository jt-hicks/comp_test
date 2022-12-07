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
seas_obj <- didehpc::queue_didehpc(ctx,config = config_16)
seas_obj$login()
seas_obj$cluster_load(TRUE)
seas_obj$enqueue()

data_raw_ng_pg_asa <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_ng_pg_asa.RDS')
data_raw_ng_pg_ifenorth <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_ng_pg_ifenorth.RDS')
data_raw_ng_pg_ejigbo <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_ng_pg_ejigbo.RDS')
data_raw_ng_pg_moro <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_ng_pg_moro.RDS')

data_raw_bf_pg_banfora <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_bf_pg_banfora.RDS')
data_raw_bf_pg_orodara <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_bf_pg_orodara.RDS')
data_raw_bf_pg_gaoua <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_bf_pg_gaoua.RDS')

data_raw_mz_pg_guro <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_mz_pg_guro.RDS')
data_raw_mz_pg_chemba <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_mz_pg_chemba.RDS')
data_raw_mz_pg_changara <- readRDS('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/PregnancyModel/PATH/Analysis/nnp_explor/data_raw_mz_pg_changara.RDS')

nnp_pg_list <- list(data_raw_bf_pg_banfora,data_raw_bf_pg_gaoua,data_raw_bf_pg_orodara,
                    data_raw_mz_pg_changara,data_raw_mz_pg_chemba,data_raw_mz_pg_guro,
                    data_raw_ng_pg_asa,data_raw_ng_pg_ejigbo,data_raw_ng_pg_ifenorth,data_raw_ng_pg_moro)
country_list <- c('Burkina Faso','Burkina Faso','Burkina Faso',
                  'Mozambique','Mozambique','Mozambique',
                  'Nigeria','Nigeria','Nigeria','Nigeria')
admin_unit_list <- c('Cascades','Sud-Ouest','Haut-Bassins',
                     'Tete','Sofala','Manica',
                     'Kwara','Osun','Osun','Kwara')

nnp_pg_bulk <- seas_obj$enqueue_bulk(1:10, function(i,data_site,country_list,admin_unit_list){
  run_pmcmc(data = data_site[[i]],
            n_particles = 200,
            proposal_matrix = matrix(c(0.0336,-0.000589,-0.000589,0.049420),nrow=2),
            max_EIR=1000,
            max_steps = 1e7,
            atol = 1e-5,
            rtol = 1e-6,
            n_steps = 1000,
            n_threads = 16,
            country = country_list[i],
            admin_unit = admin_unit_list[i],
            preyears = 2)
},data_site=nnp_pg_list,country_list=country_list,admin_unit_list=admin_unit_list)

nnp_pg_bulk$status()
nnp_pg_bulk$times()
nnp_pg_bulk$tasks$`1ff9b955b197bc8c8aa348f9da73146d`$log()
nnp_pg_bulk$tasks$`464a8fb3cdcbff345858d4ec31636e5b`$log()
nnp_pg_bulk$tasks$`24d492c24a8244bcad228811cb21ab98`$log()

nnp_pgseas_result_list <- lapply(1:10, function(id){
  nnp_pg_bulk$tasks[[id]]$result()
})
saveRDS(nnp_pgseas_result_list,'nnp_pgseas_results_061222.RDS')

##Run bulk original model to compare times
nnp_pg_bulk_orig <- orig_obj$enqueue_bulk(1:10, function(i,data_site,country_list){
  run_pmcmc(data_raw = data_site[[i]],
            n_particles = 200,
            proposal_matrix = matrix(c(0.0336,-0.000589,-0.000589,0.049420),nrow=2),
            max_EIR=1000,
            max_steps = 1e7,
            atol = 1e-5,
            rtol = 1e-6,
            n_steps = 1000,
            n_threads = 16)
},data_site=nnp_pg_list,country_list=country_list)

orig_obj$cluster_load(TRUE)
orig_obj$config
nnp_pg_bulk_orig$status()
