library(XML)

BUS_PREFIX = "pt_bus"
PASSENGER_PREFIX = NULL

# -----------------------------------------------------------------
# Extracting from XML to assemble as a data frame
# Bus ids in columns, timestamps in rows, x-y coordinates at each cell
 
summary_trace <- function(vtype_prefix=BUS_PREFIX, export=TRUE) {

	# First brackets level: Timestep 
	# Second brackets level: Vehicle ID
	trace <- xmlToList("trace2.xml")
	steps <- length(trace)-1
	trace_df <- data.frame(matrix(ncol=1, nrow=steps))
	trace_df <- setNames(trace_df,"Timestamp")



	# Gathering Bus IDs to assemble data frame columns 
	for(t in 2:steps) {

		vehicles <- length(trace[[t]])-1
		#trace_df$Timestamp[t] <- toString(trace[[t]])

		for(veh_id in 1:vehicles) {

			id <- toString(trace[[t]][[veh_id]][1])

			# Matching prefix to get desired vtype: Bus by default
			if( grepl(vtype_prefix, id) ) {
				x  <- floor(as.double(trace[[t]][[veh_id]][2]))
				y  <- floor(as.double(trace[[t]][[veh_id]][3]))

				# Removing id suffix
				id <- unlist(strsplit(id, ":"))[1] 

				# Check non repeated elements before add
				if(!any(names(trace_df) == id)) 
					trace_df[id] <- NA

				trace_df[[id]][t] <- paste(x,y,sep=' / ')
			}

		}

		# Progress 
		progress = ceiling(100*t/steps)
		cat('\r',format(paste("Progress: ", progress, "%", sep='')))
		flush.console()
	}

	message("\nDone!")

	if(export)
		write.csv(trace_df, file="summary_trace.csv")

}
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# Amount ouf vehicles per timestamp
vehicle_count <- function() {

	prefix <- NULL
	trace <- xmlToList("trace2.xml")
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

	message("\nDone!")
	plot(vehicle_count_ts$t, vehicle_count_ts$n_vehicles, type='l')
	#write.csv(trace_df, file="summary_trace.csv")

}
# -----------------------------------------------------------------
