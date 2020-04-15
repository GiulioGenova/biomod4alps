if(!interactive()){
  setwd("/home/lv71284/g_genova/data/")
}else{
  setwd("/data/OneDrive/01_PhD/05_projects/boimod2/")  
}

library(biomod2)
library(raster)
library(doParallel)
library(foreach)
source("biomod4alps/project_current.R")

#####################################
# model options
if(!interactive()){
  args = commandArgs(trailingOnly=TRUE)
  model =  args[1]
  n_cores = as.numeric(args[2])
  }else {
  model = "RF"
  n_cores = 2
}

# species extents
t <- read.table("spec_extents.txt", head = TRUE, sep = "\t")
#####################################
# loading current environmental data
cur=stack(dir("env_predictors/climate/present", full.names=T))

####################################
out_dir = paste0(getwd(),"/models_future")
if(!dir.exists(out_dir)){dir.create(out_dir)}
setwd(out_dir)

# cl <- makeCluster(n_cores)
# registerDoParallel(cl)
registerDoParallel(cores = n_cores)
#getDoParWorkers()

rasterOptions(tmpdir = paste0(getwd(),"/temp_rast_dir"),
              maxmemory = 4.9e+09#,
              #memfrac = (1/n_cores)*0.8
)
#raster::tmpDir(create = TRUE)
tmpDir()

models_rds = list.files(path = "models_rds" , pattern = ".rds",full.names = T)
if(model!="all"){models_rds = models_rds[grep(pattern = model,x = models_rds)]}

## do the job - current projections
models_current = foreach(mod = models_rds) %dopar% {
  
  project_current(myBiomodModelOut = mod,cur = cur,t = t,read_from_file = TRUE)
  
}

models_current

unlink(tmpDir(), recursive=T, force=FALSE)

print("End of the script")