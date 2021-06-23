# Created by Israel Vasconcelos at Dec/2020
# Federal University of Alagoas

library(XML)
library(png)

PASSENGER_PREFIX <<- NULL
SAMPLE_RATE <<- 900 # Seconds
TRACE_PATH <<- "trace/"
BUS_PREFIX <<- "bus"
COORD_SEP <<- ","

# -----------------------------------------------------------------
# Extracting from XML to assemble as a data frame
# Bus ids in columns, timestamps in rows, x-y coordinates at each cell
 
summary_trace <- function(trace_file, vtype_prefix=BUS_PREFIX, export=TRUE, offset=NULL) {

	# First brackets level: Timestep 
	# Second brackets level: Vehicle ID
	message("Importing: ", trace_file)
	trace <- xmlToList(trace_file)
	steps <- length(trace)-1
	trace_df <- data.frame(matrix(ncol=1, nrow=steps))
	trace_df <- setNames(trace_df,"Timestamp")

	# Set timestamps to consider
	trace_df$Timestamp <- ((1:steps)-1)*SAMPLE_RATE

	# Handle splitted map by adding coordinate offset for each bounding box
	offset_flag <- ''
	offset_x <- 0
	offset_y <- 0

	# Offset checks and warnings
	if(!is.null(offset))
		if(length(offset) == 2)
			offset_flag = 'offset_'
		else
			stop("Invalid x,y offset format.")

	file_name <- paste0("summary_", offset_flag, strsplit(trace_file, ".xml"), ".csv")

	# Gathering Bus IDs to assemble data frame columns 
	for(t in 2:steps) {

		vehicles <- length(trace[[t]])-1
		#trace_df$Timestamp[t] <- toString(trace[[t]])

		for(veh_id in 1:vehicles) {

			id <- toString(trace[[t]][[veh_id]][1])

			# Matching prefix to get desired vtype: Bus by default
			if( grepl(vtype_prefix, id) ) {

				if( !is.null(offset) ) {
					offset_x <- offset[1] # x
					offset_y <- offset[2] # y
				}

				x  <- floor(as.double(trace[[t]][[veh_id]][2]))
				y  <- floor(as.double(trace[[t]][[veh_id]][3]))

				x <- x + offset_x
				y <- y + offset_y

				# Removing id suffix
				#id <- unlist(strsplit(id, ":"))[1] 

				# Check non repeated elements before add
				if(!any(names(trace_df) == id)) 
					trace_df[id] <- NA

				trace_df[[id]][t] <- paste(x,y,sep=COORD_SEP)
			}

		}

		# Progress 
		progress = ceiling(100*t/steps)
		cat('\r',format(paste("Progress: ", progress, "%", sep='')))
		flush.console()
	}

	bus_count <- ncol(trace_df) - 1

	if(export)
		write.csv(trace_df, file=file_name, row.names=FALSE)

	message("\nDone! Bus count: ", bus_count)
	return(trace_df)

}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Amount ouf vehicles per timestamp
vehicle_count <- function(trace_file) {

	prefix <- NULL
	trace <- xmlToList(trace_file)
	steps <- length(trace)-1
	trace_df <- data.frame(matrix(ncol=1, nrow=steps))
	trace_df <- setNames(trace_df,"Timestamp")

	vehicle_count_ts <- data.frame(matrix(ncol=2, nrow=steps))
	vehicle_count_ts <- setNames(vehicle_count_ts, c("t", "n_vehicles" ))

	vehicle_count_ts[1,] <- c(toString(trace[[1]]), 0)

	# Gathering Bus IDs to assemble data frame columns 
	for(t in 2:steps) {

		vcount_t <- length(trace[[t]])-1

		# Timestamp is the last element
		timestamp <- toString(trace[[t]][[vcount_t+1]]) 
		vehicle_count_ts[t,] <- c(timestamp, vcount_t)


		# Progress 
		progress = ceiling(100*t/steps)
		cat('\r',format(paste("Progress: ", progress, "%", sep='')))
		flush.console()
	}


	# Note: vehicle_count_ts$t[1] is getting trash data, fix later
	message("\nDone!")
	return(vehicle_count_ts$n_vehicles)

}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Plot info about vehicle generation
vcount_export_summary <- function() {

	trace_light_traffic <- paste0(TRACE_PATH,"trace_bbox1_1_15000.xml")
	trace_avg_traffic <- paste0(TRACE_PATH,"trace_bbox1_1_30000.xml")
	trace_heavy_traffic <- paste0(TRACE_PATH,"trace_bbox1_1_60000.xml")

	summary_light <- vehicle_count(trace_light_traffic)
	summary_avg <- vehicle_count(trace_avg_traffic)
	summary_heavy <- vehicle_count(trace_heavy_traffic)

	timestamps <- c(0, 900, 2700, 3600, 4500, 5400, 6300)

	png("graphics/0_Veh_generation_summary.png")

		plot(timestamps, summary_light, type='l',
		     col='darkgreen', lty=1, ylim=c(0,70000),
		     xlab='Time (s)', ylab='Amount of vehicles',
		     main='Vehicle generation within a bounding box')

		points(timestamps, summary_avg, type='l', lty=5, col='blue')
		points(timestamps, summary_heavy, type='l', lty=2, col='red')

		legend("topright", 
			legend=c("Heavy traffic", "Average traffic", " Light traffic"), 
			lty=c(2,5,1), col=c("red","blue","darkgreen"))

	dev.off()

}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Assemble each bounding box and give (x,y) offset
merge_traces <- function() {

	all_files <- list.files(path=".")
	trace_list <- which(grepl("trace", all_files) & grepl(".xml", all_files))
	trace_list <- all_files[trace_list]
	df <- NULL
	df_list <- list()

	for(trace_i in trace_list) {
		#df_i <- summary_trace(trace_i, export=FALSE)

		df_list[[trace_i]] <- summary_trace(trace_i, export=FALSE)

		#if(is.null(df))
		#	df <- df_i
		#else
		#	cbind(df_i, df)	
	}

	return(df_list)

}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Parsing trace with coords offset fixed, sectorize as equivalent 25x25
as25_trace_coords <- function(trace_df, scale_x, scale_y, as25_xmin_offset, as25_ymin_offset) {

	trace <- trace_df

	for(col_i in 2:ncol(trace_df)) {

		indexes <- which(!is.na(trace[col_i]))

		if(length(indexes) > 0) {
			coords_list <- trace[indexes, col_i] 
			coords_list <- strsplit(coords_list, COORD_SEP)

			for(j in 1:length(coords_list)) {

				x <- as.double(coords_list[[j]][1])
				y <- as.double(coords_list[[j]][2])

				x_as25 <- floor( as25_xmin_offset + (x / scale_x) ) # sectorized as 25x25
				y_as25 <- floor( as25_ymin_offset + (y / scale_y) ) # setorized as 25x25

				to_cell <- paste0(x_as25, COORD_SEP, y_as25)
				row_i <- indexes[j]

				trace[row_i, col_i] <- to_cell
			}
		}

		# Progress
		progress = ceiling(100*col_i/ncol(trace_df))
		cat('\r',format(paste("Progress: ", progress, "%", sep='')))
		flush.console()
	}

	message("\nDone!")

	return(trace)
}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Matching 25x25 trace coords with 25x25 pollutant maps
as25_coords_match <- function(trace_as25) {

	bbox_coords_list <- vector()
	valid_coords <- read.csv("spcoords_25x25.csv")
	valid_coords <- paste0(valid_coords[,1],",",valid_coords[,2]) # Parsed as string to match  

	print(valid_coords)

	# -----------------------------------------------------
	all_files <- list.files(path=".")
	pol_maps <- which(grepl("pol_map", all_files) & grepl(".csv", all_files))
	pol_maps <- all_files[pol_maps]

	# -----------------------------------------------------
	# From which timestamp window the samples will be considered (i.e. from 2700 to 5400 = 1 hour)
	#timestep_cutline <- 3
	steps_to_hour <- floor(3600/SAMPLE_RATE)
	#timestep_window <- (timestep_cutline:steps_to_hour)*SAMPLE_RATE
	timestep_window <- c(2700, 3600, 4500, 5400)

	# Check if all timestamps are available on trace
	t_window_match <- all(intersect(trace_as25$Timestamp, timestep_window) == timestep_window)


	print(trace_as25$Timestamp)
	print(timestep_window)

	if(!t_window_match || steps_to_hour < 1)
		stop("Invalid timestamp range")

	#if(length_as25$Timestamp < 4)
	#	stop("Few timesteps available.")

	indexes_window_match <- which(intersect(trace_as25$Timestamp, timestep_window) == timestep_window)

	# Browsing on pollutant maps to match with traces
	for(i in 1:length(pol_maps)) {

		for(j in 2:ncol(trace_as25)) {

			indexes <- which(!is.na(trace_as25[,j]))
			timestamps <- trace_as25$Timestamp[indexes]

			j_indexes_window_match <- intersect(indexes_window_match, indexes)

			bus_coords <- trace_as25[j_indexes_window_match,j]

			bbox_coords_list <- append(bbox_coords_list, bus_coords)
		}
	}

	# Removing duplicates, sorting data and matching with valid map coordinates
	bbox_coords_list <- sort(unique(bbox_coords_list))
	valid_bbox_coords_list <- intersect(bbox_coords_list, valid_coords) 
	return(valid_bbox_coords_list)

}


# -----------------------------------------------------------------
# Receive as input a 25x25 matrix where the seeds are all non-zero cells
# Voronoi Diagram fills the zero cells with respective seed value
voronoi_diagram <- function(sampled_field, seed_locations, map_coords, fieldLength=25, messages=FALSE) {

	maxX <- fieldLength
	maxY <- fieldLength
	voronoiMatrix <- matrix(0, nrow=maxX, ncol=maxY)

	seed_coords_list <- strsplit(seed_locations, ",")
	xcoords <- vector()
	ycoords <- vector()


	for(i in 1:length(seed_coords_list)) {
		xcoords[i] <- as.numeric(seed_coords_list[[i]][1])
		ycoords[i] <- as.numeric(seed_coords_list[[i]][2])
	}

	#--------------------------------------------
	if(messages)
		message("Generating Voronoi Diagram") 

	# Building Voronoi Diagram
	for(i in 1:maxX){
		for(j in 1:maxY){
			dist <- 99999
			for(k in 1:length(seed_coords_list)){

				temp_dist <- sqrt( (i-xcoords[k])^2 + (j-ycoords[k])^2 )

				if(temp_dist < dist){
			  		dist <- temp_dist
					x <- xcoords[k]
					y <- ycoords[k]

			  		voronoiMatrix[i, j] <- sampled_field[x,y] # Assigning sample from closest cell
				}
			}

		#print(paste("x:",i," / y:",j," / sensor:",voronoiMatrix[i, j]))
		}
		
		if(messages) {
			cat('\r',format(paste("Progress: ",(i/maxX)*100, "%", sep='')))
			flush.console() 
		}
	}

	#------------------------------------------------------
	if(messages) {
		cat('\n')
		message("Generating city area (São Paulo map)") 
	}

	for(x in 1:maxX) {

		for(y in 1:maxY) {

			vc_flag <- FALSE # v.c.: valid coord

			for(i in 1:nrow(map_coords)) {

				valid_xy <- as.numeric(map_coords[i,])
				xy <- c(x,y)

				coords_match <- all(valid_xy == xy)

				if(coords_match) {
					vc_flag <- TRUE
					break
				}
			}

			if(!vc_flag) { # Set as -1 coordinates not valid
				voronoiMatrix[x,y] <- NA
			}
		}

		if(messages) {
			cat('\r',format(paste("Progress: ",(x/maxX)*100, "%", sep='')))
			flush.console() 
		}
	}

	#----------------
	if(messages)
		cat('\n')

	return(voronoiMatrix)
}


# -----------------------------------------------------------------
# Evaluating AVRE (Absolute Value of Relative Error)
# Performance metric: error in relation to original field
error_eval <- function(voronoi_field, reference_field, map_coords, fieldLength = 25, messages=FALSE) {

	#-----------------------------------------------------
	if(messages)
		message("Evaluating Absolute Value of Relative Error") 

	k=0

	avre = 0 # absolute value of relative error
	for(x in 1:fieldLength) {
		for(y in 1:fieldLength) {

			vc_flag <- FALSE # v.c.: valid coord

			for(i in 1:nrow(map_coords)) {

				valid_xy <- as.numeric(map_coords[i,])
				xy <- c(x,y)

				coords_match <- all(valid_xy == xy)

				if(coords_match) {
					vc_flag <- TRUE
					break
				}
			}

			if(vc_flag) {# Consider to AVRE Evaluation only the valid coords
				avre = avre + abs((reference_field[x,y] - voronoi_field[x,y])/ reference_field[x,y])
			}
		}

		#-----------------------------------------------------
		if(messages) {
			cat('\r',format(paste("Progress: ",(x/fieldLength)*100, "%", sep='')))
			flush.console() 
		}
	}

	# Only evaluates map area (map_coords) instead squared fieldLength area that surrounds the map.
	avre = round((avre / nrow(map_coords))*100, 3) # Error value is given in %

	#----------------
	if(messages) {
		cat('\n')
		message("Error rate: ", avre)
	}


	return(avre)
}
