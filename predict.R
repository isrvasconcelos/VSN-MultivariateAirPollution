library(gstat)
#library(geobr)
library(hash)
library(sp)
library(tidyr)
library(EnvStats)
library(png)

source("auxiliary.R")

# -----------------------------------------------------------------
# Asserting data to input format at predict step
airpol <- read.csv("summary_table.csv")

var_name <- c("CO", "MP10", "O3", "NO2", "SO2")
station_id <- c("X1","X2","X3","X4","X5","X8","X12","X16","X27","X47")

station_coord <- sectorize_coord(map="spmap.png")
station_id_coord <- list()

for(i in 1:length(station_id)) 
	station_id_coord[[station_id[i]]] <- station_coord[i,]

# -----------------------------------------------------------------
# Assign region of interest
sp_coords <- map_coords(map="spmap.png")


# -----------------------------------------------------------------
# Assemblying combinations for all stations and variables
df_cols <- expand.grid(var_name, station_id)
df_cols <- df_cols[c("Var2", "Var1")]


# -----------------------------------------------------------------
# Assemblying summary table (data.frame names)
st_names <- {}
for(i in 1:nrow(df_cols)) 
	st_names[i] <- paste(df_cols[i,1], df_cols[i,2], sep='_')

pred_st <- data.frame(airpol$Date, airpol$Time)
names(pred_st) <- c("Date","Time")
pred_st[st_names] <- NA 

col_match = match(names(airpol), names(pred_st))


# -----------------------------------------------------------------
# Fixing NAs
for(i in 3:ncol(airpol))
	airpol[,i] <- ts_na_handle(airpol[,i])


# -----------------------------------------------------------------
# Fixing units to avoid negative samples at prediction.
# See at auxiliary.R: predict_series(), log(airpol_snapshot$var) must be > 1 
airpol[,3:ncol(airpol)] <- airpol[,3:ncol(airpol)]+0.1
airpol[,3:ncol(airpol)] <- airpol[,3:ncol(airpol)]*10


# -----------------------------------------------------------------
# Structure for predicted summary table
pred_st[,col_match] = airpol[,]


# -----------------------------------------------------------------
# Prediction 1st Turn: CO, PM10, 03
# -----------------------------------------------------------------
message("1st step")

var_name_CO_PM10_O3 <- c("CO", "MP10", "O3")
station_id_CO_PM10_03 <- c("X1","X3","X5","X16","X27")

CO_PM10_03_coords <- matrix(unlist( station_id_coord[station_id_CO_PM10_03] ), 
				    ncol=2, 
				    byrow=TRUE,
				    dimnames= list(station_id_CO_PM10_03, c("x","y")))

CO_PM10_03_snapshot_series <- snapshot_series(  CO_PM10_03_coords, 
						airpol, 
						var_name_CO_PM10_O3,
						station_id_CO_PM10_03)

CO_PM10_03_reconst <- predict_series( CO_PM10_03_snapshot_series, 
				      var_name_CO_PM10_O3, 
				      sp_coords )


# Gathering stations with missing values to  predicted ones
missing_station_coord_1st <- station_id_coord[setdiff(station_id, station_id_CO_PM10_03)]

# Assigning predicted values at gathered missing stations
message("Assigning predicted values at missing stations: 1st Turn")

pred_st <- station_assign(var_name_CO_PM10_O3, 
			  missing_station_coord_1st, 
			  CO_PM10_03_reconst, 
			  pred_st)

#write.csv(pred_st, "st_pred_1st.csv")


# -----------------------------------------------------------------
# Prediction 2nd Turn: NO2
# -----------------------------------------------------------------
message("2nd step")

# Setting stations and variables for the 2nd turn
var_name_NO2 <- "NO2"
station_id_NO2 <- c("X5","X12","X27","X47")
var_name_inc_NO2 <- union(var_name_CO_PM10_O3, var_name_NO2)

# ------------------------------------------------
# Updating input data frame to gstat()
var_airpol_pred_NO2 <- {}

for(var in names(pred_st)) 
	if(all(!is.na(pred_st[[var]])))
		var_airpol_pred_NO2 <- append(var_airpol_pred_NO2, var)

st_airpol_pred_NO2 <- pred_st[var_airpol_pred_NO2]


NO2_coords <- matrix(unlist( station_id_coord[station_id_NO2] ), 
			     ncol=2, 
			     byrow=TRUE,
			     dimnames= list(station_id_NO2, c("x","y")))

NO2_snapshot_series <- snapshot_series( NO2_coords, 
					st_airpol_pred_NO2, 
					var_name_inc_NO2,
					station_id_NO2)

NO2_reconst <- predict_series( NO2_snapshot_series, var_name_inc_NO2, sp_coords )


# -----------------------------------------------------------------
# Gathering stations with missing values to assign predicted ones: 2nd Turn
missing_station_coord_2nd <- station_id_coord[setdiff(station_id, station_id_NO2)]


message("Assigning predicted values at missing stations: 2nd Turn")
pred_st <- station_assign(var_name_NO2, 
			  missing_station_coord_2nd, 
			  NO2_reconst, 
			  pred_st)

#write.csv(pred_st, "st_pred_2st.csv")


# -----------------------------------------------------------------
# Prediction 3rd Turn: SO2
# -----------------------------------------------------------------
message("3rd step")

# Setting stations and variables for the 3rd turn
var_name_SO2 <- "SO2"
station_id_SO2 <- c("X5","X8")
var_name_inc_SO2 <- union(var_name_inc_NO2,var_name_SO2)

# ------------------------------------------------
# Updating input data frame to gstat()
var_airpol_pred_SO2 <- {}

for(var in names(pred_st)) 
	if(all(!is.na(pred_st[[var]])))
		var_airpol_pred_SO2 <- append(var_airpol_pred_SO2, var)

st_airpol_pred_SO2 <- pred_st[var_airpol_pred_SO2]


SO2_coords <- matrix(unlist( station_id_coord[station_id_SO2] ), 
			     ncol=2, 
			     byrow=TRUE,
			     dimnames= list(station_id_SO2, c("x","y")))

SO2_snapshot_series <- snapshot_series( SO2_coords, 
					st_airpol_pred_SO2, 
					var_name_inc_SO2,
					station_id_SO2)

SO2_reconst <- predict_series( SO2_snapshot_series, var_name_inc_SO2, sp_coords )

# -----------------------------------------------------------------
# Gathering stations with missing values to assign predicted ones: 3rd Turn
missing_station_coord_3rd <- station_id_coord[setdiff(station_id, station_id_SO2)]


message("Assigning predicted values at missing stations: 3rd Turn")
pred_st <- station_assign(var_name_SO2, 
			  missing_station_coord_3rd, 
			  SO2_reconst, 
			  pred_st)

write.csv(pred_st, "st_pred.csv")


# -----------------------------------------------------------------
# Prediction 4st Turn: Entire station/variable set
# -----------------------------------------------------------------

snapshot_series_all <- snapshot_series( station_coord, 
					pred_st, 
					var_name,
					station_id)

reconst_all <- predict_series( snapshot_series_all, var_name, sp_coords )


# Exporting all predicted maps as csv
pol_map(pred_st, reconst_all)

# Exporting all predicted maps as figures
export_map(reconst_all)

message("Done!")
