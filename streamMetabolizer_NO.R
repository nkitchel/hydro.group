suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(streamMetabolizer)
})
require(chron)
require(mcmc)
require(lubridate)

riveroxy<-read.csv("riveroxy.csv", as.is = TRUE)

riveroxy <- riveroxy[, -c(11:17)]
# delete the stupid NA columns

#Make a column of date time, convert to a chron object

dtime<-chron(dates=as.character(riveroxy$date), times=as.character(riveroxy$time))
riveroxy$dtime<-as.POSIXct(dtime)
# coerce to the UTC timezone
riveroxy$dtime<-force_tz(riveroxy$dtime, tzone = "UTC")

riveroxy$dtime<-convert_UTC_to_solartime(riveroxy$dtime, 106, time.type = c("apparent solar", "mean solar"))


#subset data to get only the data for the Platte River and from station down1
riveroxy <- riveroxy[with(riveroxy, river == "buffalo" & station == "down1"), ]


# 
# riveroxy<- riveroxy[riveroxy$Date %in% as.numeric(chron(dates="07/21/12", 
#    times="22:00:00")):as.numeric(chron(dates="07/23/12", times="06:00:00")), ]

# & 
# riveroxy<-riveroxy$dtime>=as.numeric(chron(dates="07/21/12", times="22:00:00"))  
# riveroxy$dtime<=as.numeric(chron(dates="07/23/12", times="06:00:00"))


# did it with the buffalo so that there would be a light column

# here are the values Bob hard-coded in his homerolled code, I am going to 
#add them as variables for lack of a better idea
z<-1.01   # depth I believe  --- gets converted to a column later
bp<-643   # barometric pressure, needed for DO saturation
ts<-0.0034722   # not really sure frankly
Kmean<-3  # mean K value      
Ksd<-1    # Standard Deviation of K values

# now we can calculate DOsat
dosat<-calc_DO_sat(temp.water= riveroxy$temp, pressure.air = bp)

#cbind to riveroxy
riveroxy<-cbind(riveroxy, dosat)

#make riveroxy into dat  ... so I dont have to change anything in the provided code
dat<-riveroxy

# I think this is a super important deal here>>>>>>>>>>>>>>>>>
# gist is that we need the data in chunks of 3 days that start at 4am and end at 4am
# I am going to cheat and do this in Excel
dat <- dat(num_days='3', res='15', day_start=4, day_end=28, attach.units=TRUE)

#makes a column with the depth value
dat$depth <- rep(z,nrow(dat))

dat$light<-NULL

## rename the column names to match the model/chart code
names(dat)[names(dat) == "LDO"] <- "DO.obs"
names(dat)[names(dat) == "dosat"] <- "DO.sat"
# names(dat)[names(dat) == "z"] <- "depth"
names(dat)[names(dat) == "temp"] <- "temp.water"
names(dat)[names(dat) == "modlight"] <- "light"
names(dat)[names(dat) == "dtime"] <- "solar.time"


# plots from help page

dat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')



labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
dat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, DO.sat, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=DO.sat, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')


#####
#  ------------ need to determine what y value makes sense. DO.sat plots...but 
#   ----------- that does not make any sense right? It looks like the charts on the
# ------------- help page though???


#columns and the class they need to be in. Units too, not sure how to go about coercing that
metab_inputs('mle', 'data')

# to check to see that our data matches the required model inputs.... it does!!
# May be easiest to make dtime into solar.time? LDO into DO.obs; dosat to DO.sat
# z to depth; temp to temp.water and modlight to light?? the Light still needs to 
# be calculated...
data.frame(
  colname=names(dat), 
  class=unname(sapply(unitted::v(dat), function(col) paste(class(col), collapse=','))),
  units=unname(unitted::get_units(dat)))


#---------------------------------------------------------------------------------
##----------------- Data should be in shape to run the model now -----------------
###-------------------------------------------------------------------------------

# Modeling Overview
# 
# There are three steps to fitting a metabolism model in streamMetabolizer.
# 
# Identify the name of the model structure you want using mm_name().
# Set the specifications for the model using defaults fromspecs() as a starting 
# point.
# Fit the model with metab().
# And then a fourth step once you’ve fit a model.
# 
# Inspect the output with functions including get_params(), get_fit(), 
# predict_metab(), predict_DO(), plot_metab_preds(), and plot_DO_preds().

mle_name <- mm_name(type='mle')
mle_name
# Here we will fit the default MLE model. Many others are available (see Other
#       Models below), but this one is common and fast

mle_specs <- specs(mle_name)
mle_specs
#Having chosen a model, we next need to define a list of specifications for 
#that model. The specs function creates a list appropriate to the model we chose

mle_fit <- metab(mle_specs, data=dat, 
                 info=c(site='Platte River', source='Bob Hall'))
# It’s optional, but sometimes helpful, to include some sort of metadata in the info,
# as I’ve done above. I’ve chosen to put the metadata in a character vector, but 
# metadata can take any format you like.


#Seems strange, but whatever:
#Error: data should omit these extra columns: river, station, date, time, datetime, oxy

dat$river<-NULL
dat$station<-NULL
dat$date<-NULL
dat$time<-NULL
dat$datetime<-NULL
dat$oxy<-NULL
dat$modlight<-NULL

#rerun the mle_fit code 
mle_fit <- metab(mle_specs, data=dat, 
                 info=c(site='Platte River', source='Bob Hall'))
# print the output
mle_fit

# I seem to be getting alot of NAs... not ideal 
# i think it is because there are dates that are not used or something with the 
# time  - it may want the times to start and end at 4am!


#There is a function to plot the daily metabolism estimates.

plot_metab_preds(mle_fit)

# yep, something is wrong indeed, maybe its the improperly defined light??


#There is also a function to plot the dissolved oxygen predictions (lines) along
#with the original observations (points).

plot_DO_preds(mle_fit)
# looks better I think

#You can output the daily and instantaneous predictions to data.frames for further 
#inspection.
met_preds <- predict_metab(mle_fit)
met_preds

#---------------------------------------------------------------------------------
##-----------A Bayesian Model-----------------------------------------------------
###-------------------------------------------------------------------------------

##1. Choose a model structure

bayes_name <- mm_name(
  type='bayes', pool_K600='none', 
  err_obs_iid=TRUE, err_proc_acor=FALSE, err_proc_iid=TRUE, 
  ode_method='trapezoid')
bayes_name

## 2. Set the specifications

bayes_specs <- specs(bayes_name)
bayes_specs


# I’m choosing a very small number of burnin_steps and saved_steps because I don’t 
# want to wait long for the vignette to run. When you run your own models, consider 
# bumping those numbers up substantially (to several thousand or so, depending on 
# exactly which model you’re using.)

# one way to alter specifications: call specs() again
bayes_specs <- specs(bayes_name, burnin_steps=10000, saved_steps=20000, n_cores=1, 
GPP_daily_mu=3, GPP_daily_sigma=2)
# another way: use revise()
bayes_specs <- revise(bayes_specs, burnin_steps=100, saved_steps=200, 
n_cores=1, GPP_daily_mu=3, GPP_daily_sigma=2)


#3. Fit the model
#We now fit the model with the new specifications. Bayesian models take longer 
#to run, so be patient.

bayes_fit <- metab(bayes_specs, data=dat)
## so, the compilation Error message, my old friend.. it does however produce a
### variable bayes_fit, so .... 
### maybe there are not enough burnin_steps or saved_steps? Nope goosed it from 
# 100 burnin_steps to 10000 and 200 saved steps to 20000, no dice




predict_metab(bayes_fit) %>% 
  lapply(function(col) if(is.numeric(col)) round(col, 2) else col ) %>%
  as.data.frame() %>%
  knitr::kable()


plot_DO_preds(bayes_fit)






### so, i am not exactly sure where/how to proceed. Is this a software problem,
# or did the slight fudging of the light data really play this large of a role?

## the huge bulk of this code came from the "Documentation for package 'streamMetabolizer'
## which you can find by clicking on this 'streamMetabolizer', scrolling to the
## bottom of the page and selecting index at the very bottom of the page. Then, go
## to User guides ....

