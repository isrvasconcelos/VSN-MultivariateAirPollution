library(gstat)
#library(geobr)
library(hash)
library(sp)
library(tidyr)
library(EnvStats)
library(png)

# -----------------------------------------------------------------
export_img <- function(name_, points) {

	xcoords <- ceiling(points$x)
	ycoords <- ceiling(points$y)

	fieldMatrix <- matrix(1, nrow=100, ncol=100)

	for(i in 1:length(xcoords)) {
		fieldMatrix[xcoords[i], ycoords[i]] = 0
	}

	graphics.off()
	postscript(name_,horizontal=FALSE,onefile=FALSE,height=8,width=8,pointsize=14)
	image(fieldMatrix, col=gray((1:32)/32), xaxt="n", yaxt="n", xlab="", ylab="")
	graphics.off()

}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
as_grid25 <- function(dataMatrix, gridsize=25, nmax=1000, disp=FALSE) {

	# REMINDER: ALWAYS FIX WITH X+1, FROM:0/TO:FIELDSIZE-1/BY:10
	step = nmax/gridsize
	sectors <- seq(from=1, to=999, by=step)
	coverageMatrix <- matrix(0, ncol=25, nrow=25)

	for(i in 1:length(sectors)) {
		for(j in 1:length(sectors)) {

			#Sweeping on each sector
			for(x in (sectors[i]):(sectors[i]+(step-1))) { 
				for(y in (sectors[j]):(sectors[j]+(step-1))) {

			# If there is a sample on this coordinate, cover the sector
					if(dataMatrix[x,y] == 1) { 

						if(disp)
							print(c(i,j))

						coverageMatrix[i,j] = 1
					}
				}
			}
		}
	}

	return(coverageMatrix)
}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Get a time serie and assign to NAs the sample of first available nearest day
hour_matrix <- function(ts) {

	#na_samples <- which(is.na(ts))

	shift_prev <- 0	
	shift_next <- 0

	h_matrix <- {}

	hours <- 24
	for(i in 1:24) {
		s <- seq(from=i, to=length(ts), by=hours)
		h <- ts[s] 
		
		h_matrix <- rbind(h_matrix, h)

	}

	return(h_matrix)

}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
na_rm <- function(list) {

	list = as.vector(list[which(!is.na(list))])
	return(list)
}

cor_narm <- function(l1, l2) {

	av_l1 <- which(!is.na(l1)) # numeric available indexes on l1
	av_l2 <- which(!is.na(l2)) # numeric available indexes on l2
	ind <- intersect(av_l1, av_l2)
	
	if(!length(ind))
		stop("Indexes does not match");

	correl <- cor(l1[ind], l2[ind])
	return(correl)
}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Ovewrite NAs with a random sample based on gaussian merge of its crossing row/col.
ts_na_handle <- function(ts) {

	require(EnvStats)
	require(tidyr)

	shift_prev <- 0	
	shift_next <- 0
	lts <- ts

	h_matrix <- {}

	# View the time serie col as a table hours x days
	hours <- 24
	for(i in 1:24) {
		s <- seq(from=i, to=length(ts), by=hours)
		h <- lts[s] 

		h_matrix <- rbind(h_matrix, h)
	}

	# Fix NAs
	for(i in 1:nrow(h_matrix)) {
		for(j in 1:ncol(h_matrix)) {

			if(is.na(h_matrix[i,j])) {

				# try to eval std dev. to check if row/col are empty
				na.row <- is.na(sd(h_matrix[i,], na.rm=TRUE))
				na.col <- is.na(sd(h_matrix[,j], na.rm=TRUE))

				if(na.row && na.col)
					stop("Blank row and col. Could not predict.")

				# Estimate a random sample based on row (hour) mean/sd
				else if(!na.row) {
					m1 <- mean(h_matrix[i,], na.rm=TRUE)
					sd1 <- sd(h_matrix[i,], na.rm=TRUE)

					h_matrix[i,j] <- round(rnorm(1,m1,sd1),2)

					# Avoid negative samples
					while(h_matrix[i,j] < 0)
						h_matrix[i,j] <- round(rnorm(1,m1,sd1),2)
				}

				# Estimate a random sample based on column (day) mean/sd
				else if(!na.col) {
					m1 <- mean(h_matrix[,j], na.rm=TRUE)
					sd1 <- sd(h_matrix[,j], na.rm=TRUE)

					h_matrix[i,j] <- round(rnorm(1,m1,sd1),2)

					# Avoid negative samples
					while(h_matrix[i,j] < 0)
						h_matrix[i,j] <- round(rnorm(1,m1,sd1),2)
				}

				# Estimate a random sample based on row/col gaussians merge
				else {
					m1 <- mean(h_matrix[i,], na.rm=TRUE)
					m2 <- mean(h_matrix[,j], na.rm=TRUE)
					sd1 <- sd(h_matrix[i,], na.rm=TRUE)
					sd2 <- sd(h_matrix[,j], na.rm=TRUE)

					h_matrix[i,j] <- abs(round(rnormMix(1,m1,sd1,m2,sd2),2))

					# Avoid negative samples
					while(h_matrix[i,j] < 0)
						h_matrix[i,j] <- abs(round(rnormMix(
								 1,m1,sd1,m2,sd2),2))
				}



			}
		}
	}

	# Turn back table as a column
	ts_corrected <- h_matrix[1:length(h_matrix)]

	return(ts_corrected)
}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Get station coords in a 1000x1000 and return equivalent in a 25x25 
sectorize_coord <- function(map="spmap.png") {

	# Station coords manually assigned (ascending by x-axis)
	all_stations = matrix(0, nrow=1000, ncol=1000)
	all_stations[210,720] = 1 # X27 - PINHEIROS
	all_stations[210,850] = 1 # X47 - HORTO FLORESTAL
	all_stations[230,600] = 1 # X5 - IBIRAPUERA
	all_stations[230,810] = 1 # X2 - SANTANA
	all_stations[240,540] = 1 # X8 - CONGONHAS
	all_stations[250,430] = 1 # X1 - P. D. PEDRO II
	all_stations[250,480] = 1 # X16 - SANTO AMARO
	all_stations[300,780] = 1 # X12 - CENTRO
	all_stations[320,680] = 1 # X4 - CAMBUCI
	all_stations[360,720] = 1 # X3 - MOOCA

	# -----------------------------------------------
	# 1) Get 1000x1000 coords as 25x25 ordered by row (x-axis)
	# 2) Asserts (x,y) as (n,2) | Note.: which() returns table sorted by col (y-axis)
	# 3) Sorting back by row to keep consistence  

	all_stations <- as_grid25(all_stations) # 1
	all_stations <- which(all_stations == 1, arr.ind=TRUE) # 2
	all_stations <- all_stations[order(all_stations[,1]),] # 3
	# -----------------------------------------------

	colnames(all_stations) <- c("x","y")

	# Include a last check to confirm if all stations coords are inside the map

	return(all_stations)
}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Coordinates in map that represents the region of interest (map pixels)
# From 1000x1000 .png image to equivalent 25x25 sectorized Matrix
map_coords <- function(map="spmap.png", display=FALSE) {
	map = readPNG("spmap.png")[,,1] 

	coords = as_grid25(map) # Getting black pixels as valid coordinates 
	coords = t(apply(coords,2,rev)) # Rotating 90 degree to show at correct view

	if(display)
		image(coords)

	coords = abs(coords-1) # Inverting to get valid pixels displayed as 1 instead 0
	coords = which(coords == 1, arr.ind=TRUE)

	colnames(coords) <- c("x","y")
	coords = as.data.frame(coords)
	gridded(coords) = ~x+y

	return(coords)
}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Sequence of snapshots after NA fix for CO, PM10, O3 (1st Turn) 
snapshot_series <- function(station_coords, st_airpol_nafix, var_name, station_id) {

	airpol <- st_airpol_nafix

	# Getting on imported dataset the column corresponding indexes to each variable
	station_indexes <- hash()
	for(var in var_name) 
		station_indexes[[var]] <- which( grepl(var,names(airpol)) == TRUE ) 

	# Filling data frame with samples relative to a timestamp
	airpol_snapshot <- data.frame(station_coords, station_id)
	airpol_snapshot[,var_name] <- NA

	coordinates(airpol_snapshot) = ~x+y

	snapshot_series <- list()

	for(i in 1:nrow(airpol)) {

		progress = ceiling(100*i/nrow(airpol))
		cat('\r',format(paste("Generating snapshots: ", progress, "%", sep='')))
		flush.console()

		for(var in var_name) {

			var_snapshot <- airpol[i,station_indexes[[var]]]

			# tokenizing by field
			stok_station <- unlist( strsplit(names(var_snapshot), "_") ) 

			# cleaning indexes
			stok_station <- stok_station[ which( stok_station != var ) ] 

			# matching indexes found		
			fs_row <- match(stok_station, airpol_snapshot$station_id) 

			# fixing NAs
			av_indexes <- which(!is.na(fs_row))
			var_snapshot <- var_snapshot[av_indexes]
			stok_station <- stok_station[av_indexes]
			fs_row <- fs_row[av_indexes]

			airpol_snapshot[[var]][fs_row] <- var_snapshot
		}

		#snapshot_series <- append(snapshot_series, airpol_snapshot) 
		snapshot_series[[i]] <- airpol_snapshot 
	}

	message("\nDone!")
	return(snapshot_series)
}

# -----------------------------------------------------------------
# Build a temporal sequence with 
predict_series <- function(snapshot_series, var_names, coords) {

	timeserie_map <- list()
	matern <- NULL


	for(i in 1:length(snapshot_series)) {
		airpol_snapshot <- snapshot_series[[i]]
		airpol.g <- NULL

		if(is.element("CO", var_names))
			airpol.g <- gstat(id="CO", 
					  formula= log(unlist(airpol_snapshot$CO)) ~ 1,
					  data=airpol_snapshot, 
					  nmax = 10)

		if(is.element("MP10", var_names))
			airpol.g <- gstat(airpol.g, 
					  "MP10", 
					  log(unlist(airpol_snapshot$MP10))~1,
					  data=airpol_snapshot, 
					  nmax = 10)

		if(is.element("O3", var_names))
			airpol.g <- gstat(airpol.g, 
					  "O3", 
					  log(unlist(airpol_snapshot$O3))~1,
					  airpol_snapshot, 
					  nmax = 10)

		if(is.element("NO2", var_names))
			airpol.g <- gstat(airpol.g, 
					  "NO2", 
					  log(unlist(airpol_snapshot$NO2))~1,
					  airpol_snapshot, 
					  nmax = 10)

		if(is.element("SO2", var_names))
			airpol.g <- gstat(airpol.g, 
					  "SO2", 
					  log(unlist(airpol_snapshot$SO2))~1,
					  airpol_snapshot, 
					  nmax = 10)


		#matern=vgm(0.1, "Mat", 3, kappa=0.5)

		#if(is.element("NO2", var_names))
			matern=vgm(5, "Mat", 3, kappa=0.5)

		airpol.g <- gstat(airpol.g, model=matern, fill.all=T)
		v <- variogram(airpol.g,50)

		airpol.fit = fit.lmc(v, airpol.g, model=matern, 
						  fit.ranges=FALSE, 
						  correct.diagonal=1.01)

		plot(v, model=airpol.fit)

		# Running reconstruction and storing output at "info"
		info <- capture.output(
			timestamp_map_rebuilt <- predict(airpol.fit, newdata = coords)
		)

		timeserie_map[[i]] <- timestamp_map_rebuilt 

		# Formatting console output
		ic <- info[1] # string log for Intrinsic Correlation
		method <- substr(info[2], 8, 25) # string log formatted for cokriging

		# Progress
		progress = ceiling(100*i/length(snapshot_series))
		cat('\r',format(paste0(ic, " ", method, ": ", progress, "% ")))
		flush.console()
	}

	message("\nDone!")
	return(timeserie_map)
}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Assign predicted samples to missing stations
# Rerturns () with
station_assign <- function(var_to_assign, 
			   missing_station_coord,
			   var_reconst_series,
 			   summary_table_out # dataset to be updated 
			   ) {

	# Early statements
	var_name <- var_to_assign
	pred_st <- summary_table_out

	# Assemblying stations with missing variables
	missing_var_station <- expand.grid(var_name,
					   names(missing_station_coord))

	missing_var_station <- missing_var_station[c("Var2", "Var1")]

	# Set of station/pollutant to be assigned with new sample i.e.:missing_station_NO2
	missing_station_var  <- {} 
	for(i in 1:nrow(missing_var_station)) 
		missing_station_var[i] <- paste(missing_var_station[i,1], 
						missing_var_station[i,2], sep='_')

	# Assigning predicted values at gathered missing stations
	for(i in 1:nrow(pred_st)) {

		# Filtering columns with missing samples, only navigate where's NA
		st_var_missing_indexes <- which(is.na(pred_st[i,]))
		st_var_missing <- names(pred_st[i,st_var_missing_indexes]) 

		# Missing station/variables to fill with predicted samples on this turn
		var_indexes <- match(missing_station_var, st_var_missing)

		# Indexes to get cols in summary table with missing samples
		st_col_assign <- na.exclude(st_var_missing_indexes[var_indexes])

		# Names of missing stations and empty vector to store missing samples
		missing_station_names <- names(pred_st[i, st_col_assign])
		missing_station_samples <- {}

		for(missing_station in missing_station_names) {

			 # Tokenizing by field and cleaning indexes
			stok_station <- unlist( strsplit(missing_station, "_") ) 

			# Getting pollutant variable to assign prediction
			var <- stok_station[2]

			# Getting respective coords from current missing station
			station <- stok_station[1]
			coord_missing <- missing_station_coord[[station]]
			x <- coord_missing[1]
			y <- coord_missing[2]

			# Map coordinates for reference
			map_coords <- coordinates(var_reconst_series[[i]])
			x_ref <- map_coords[,1]
			y_ref <- map_coords[,2]

			# Matching x and y separately
			x_match <- which(x==x_ref)
			y_match <- which(y==y_ref)

			# Finding index for get sample at missing coord (same for x,y)
			sample_index <- match(x_match, y_match)
			sample_index <- y_match[na.exclude(sample_index)] 

			# Halt execution if no matching index is found
			if(!length(sample_index)) 
				stop("No matching coordinate. Check if it's out of bounds.")

			sample <- NULL

			# Get sample from matched position x,y
			if(var == "CO")
				sample <- round(var_reconst_series[[i]]$CO.pred[sample_index],2)

			if(var == "MP10")
				sample <- round(var_reconst_series[[i]]$MP10.pred[sample_index],2)

			if(var == "O3")
				sample <- round(var_reconst_series[[i]]$O3.pred[sample_index],2)

			if(var == "NO2")
				sample <- round(var_reconst_series[[i]]$NO2.pred[sample_index],2) 

			if(var == "SO2")
				sample <- round(var_reconst_series[[i]]$SO2.pred[sample_index],2)

			# Assemblying a sequence with predicted samples to assign at missing row
			missing_station_samples <- append(missing_station_samples, sample)

		}

		# Assign predicted sample row to missing cells at summary table
		pred_st[i, st_col_assign] <- missing_station_samples

		# Progress
		progress = ceiling(100*i/nrow(pred_st))
		cat('\r',format(paste("Progress: ", progress, "%", sep='')))
		flush.console()

	}

	message("\nDone!")
	return(pred_st)
}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Create a data frame with each predicted x,y of map as cols and timestamps as rows
pol_map <- function(pred_st, reconst_series) {

	pol_map <- data.frame(pred_st$Date, pred_st$Time)
	names(pol_map) <- c("Date","Time")

	# Consistency check for x,y sequences
	for(i in 1:length(reconst_series)) {
		x_set <- all(reconst_series[[1]]$x == reconst_series[[i]]$x)
		y_set <- all(reconst_series[[1]]$y == reconst_series[[i]]$y)

		if(!x_set || !y_set)
			stop("Execution Halted: Coords for all timestamps shall have the same ordering, coord set not match.")
	}	

	# Since that reconst_series[1]$x /$y == reconst_series[n]$x / $y
	map_coords <- paste(paste(reconst_series[[1]]$x), 
			    paste(reconst_series[[1]]$y), sep=',')

	# Creating columns to store each x,y sample
	pol_map[map_coords] <- NA

	# Copying pol_map template for each variable
	pol_map_CO <- pol_map
	pol_map_MP10 <- pol_map
	pol_map_O3 <- pol_map
	pol_map_NO2 <- pol_map
	pol_map_SO2 <- pol_map

	# Creating pollution maps
	for(i in 1:length(reconst_series)) {
		pol_map_CO[i,3:ncol(pol_map_CO)] <- reconst_series[[i]]$CO.pred
		pol_map_MP10[i,3:ncol(pol_map_MP10)] <- reconst_series[[i]]$MP10.pred
		pol_map_O3[i,3:ncol(pol_map_O3)] <- reconst_series[[i]]$O3.pred
		pol_map_SO2[i,3:ncol(pol_map_SO2)] <- reconst_series[[i]]$SO2.pred
		pol_map_NO2[i,3:ncol(pol_map_NO2)] <- reconst_series[[i]]$NO2.pred

		# Progress
		progress = ceiling(100*i/length(reconst_series))
		cat('\r',format(paste("Progress: ", progress, "%", sep='')))
		flush.console()
	}

	# DOUBLE CHECK HERE (!!)
	# Exporting files to parse as input to network simulator
	write.csv(pol_map_CO, "maps/pol_map_CO.csv", row.names=FALSE)
	write.csv(pol_map_MP10, "maps/pol_map_MP10.csv", row.names=FALSE)
	write.csv(pol_map_O3, "maps/pol_map_O3.csv", row.names=FALSE)
	write.csv(pol_map_NO2, "maps/pol_map_NO2.csv", row.names=FALSE)
	write.csv(pol_map_SO2, "maps/pol_map_SO2.csv", row.names=FALSE)

	message("\nDone!")

}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Create a data frame with each predicted x,y of map as cols and timestamps as rows
export_map <- function(reconst_series) {

	for(i in 1:length(reconst_series)) {

		png(paste0("pollutant_map/map_CO/",i,"_CO.png"))
		print(spplot(reconst_series[[i]]["CO.pred"]))
		dev.off()

		png(paste0("pollutant_map/map_MP10/",i,"_MP10.png"))
		print(spplot(reconst_series[[i]]["MP10.pred"]))
		dev.off()

		png(paste0("pollutant_map/map_O3/",i,"_O3.png"))
		print(spplot(reconst_series[[i]]["O3.pred"]))
		dev.off()

		png(paste0("pollutant_map/map_SO2/",i,"_SO2.png"))
		print(spplot(reconst_series[[i]]["SO2.pred"]))
		dev.off()

		png(paste0("pollutant_map/map_NO2/",i,"_NO2.png"))
		print(spplot(reconst_series[[i]]["NO2.pred"]))
		dev.off()

		# Progress
		progress = ceiling(100*i/length(reconst_series))
		cat('\r',format(paste("Progress: ", progress, "%", sep='')))
		flush.console()
	}

	message("\nDone!")
}
