# Created by Israel Vasconcelos at Jan/21
# Federal University of Alagoas

source("auxiliar_summary_trace.R")

GRAPHICS_PATH <<- "graphics/"
RESULTS_PATH <<- "results/"
MAPS_PATH <<- "maps/"
SEP <<- '\\.'

# -----------------------------------------------------------------
# Match pollutant maps with generated trace coordinates to evaluate overall coverage

dts_file <- paste0(RESULTS_PATH, "traffic_coords_summary.csv")
df_traffic_summary <- read.csv(dts_file, stringsAsFactors=FALSE)

all_files <- list.files(path=MAPS_PATH)
pol_map_filenames <- which(grepl("pol_", all_files) & grepl(".csv", all_files))
pol_map_filenames <- all_files[pol_map_filenames]

coverage_traffic_map <- df_traffic_summary # Coverage map have the same structure

# Updating column names for a clearer reading
ctm_veh_cols <- names(coverage_traffic_map)
ctm_veh_cols <- ctm_veh_cols[which( ctm_veh_cols != "seed" )]
ctm_veh_cols <- paste0("pct_coverage_", ctm_veh_cols)
colnames(coverage_traffic_map) <- c("seed", ctm_veh_cols)
#coverage_traffic_map <- as.list(coverage_traffic_map)
pol_maps <- list()

for(i in pol_map_filenames) { 

	# String operations before import table
	pol_map_i <- paste0(MAPS_PATH, i)
	pol_map_fn <- unlist(strsplit(i, '.csv'))
	pol_var <- gsub('pol_map_', '', pol_map_fn)

	pol_maps[[pol_var]] <- read.csv(pol_map_i)
}

# -----------------------------------------------
# Fixing column names

for(pol_var in names(pol_maps)) {

	# String operations before match coordinates
	map_coords <- gsub('X', '', names(pol_maps[[pol_var]]))

	# Removing first "not-coordinate" columns 
	map_coords <- map_coords[which(map_coords != c('Date', 'Time'))]

	# Fixing separators (converted to "." at csv importing)
	map_coords <- gsub(SEP, ',', map_coords)
 
	map_colnames <- map_coords[which(map_coords != '')]
	names(pol_maps[[pol_var]]) <- map_colnames
}



# -----------------------------------------------
# Asserting data as 25x25 matrix, generate Voronoi diagram
# Match visited coords with reference map
# Evaluate error 


# Using average traffic as reference
visited_coords <- df_traffic_summary[["veh_qt_30000"]][1]
visited_coords <- unlist(strsplit(visited_coords, " "))
visited_coords <- visited_coords[which(visited_coords != "")] # Removing empty cells

# Location for weather stations
station_coords <- read.csv("station_coords.csv")
str_station_coords <- paste(station_coords$x, station_coords$y, sep=",") # Fixing format 

vsn_var_avre_series <- list()
station_var_avre_series <- list()

message("Starting evaluation for Absolute Value of Relative Error.")

# Iterating through pollutant variables
for(pol_var in names(pol_maps)) {

	message("Current pollution variable: ", pol_var)

	# AVRE: Absolute Value of Relative Error
	vsn_avre_series <- vector()
	station_avre_series <- vector()

	# -----------------------------------------------
	# Limiting time range evaluation to 7 days
	# Note: To use entire samples range, call: nrow(pol_maps[[pol_var]]
	samples_range <- 24*7 # 1 sample per hour over 1 week

	# Checking if there's samples enough
	if( ! samples_range < nrow(pol_maps[[pol_var]]) ) 
		stop("Error: Check amount of samples.")

	# Iterating through data frame rows (timestamps) 
	for( i in 1:samples_range ) { 

		# Progress 
		message(paste0( "- (",pol_var ,") Overall progress: ", i, "/", samples_range, " -" ))

		# Handling data to matrix shape
		reference_field_i <- matrix(0, ncol=25, nrow=25)
		station_field_i <- matrix(0, ncol=25, nrow=25)
		vsn_field_i <- matrix(0, ncol=25, nrow=25)

		samples_i <- pol_maps[[pol_var]][i,]

		# Removing first "not-coordinate" columns 
		samples_i <- samples_i[which(names(samples_i) != c('','Date', 'Time'))]

		valid_coords <- read.csv("spcoords_25x25.csv")

		# -----------------------------------------------
		# Iterating through reference field coordinates
		for(xy_coord in names(samples_i)) {

			x <- as.numeric(unlist(strsplit(xy_coord, ","))[1])
			y <- as.numeric(unlist(strsplit(xy_coord, ","))[2])

			this_sample <- samples_i[[xy_coord]]

			# Asserting as 25x25 field format
			reference_field_i[x,y] <- this_sample
		}

		# -----------------------------------------------
		# Iterating through coordinates visited from bus routes 
		for(xy_coord in visited_coords) {

			x <- as.numeric(unlist(strsplit(xy_coord, ","))[1])
			y <- as.numeric(unlist(strsplit(xy_coord, ","))[2])

			this_sample <- samples_i[[xy_coord]]

			# Asserting as 25x25 field format
			vsn_field_i[x,y] <- this_sample
		}

		# -----------------------------------------------
		# Iterating through weather station coordinates 
		for(j in 1:nrow(station_coords)) {

			x <- as.numeric(station_coords[j,1])
			y <- as.numeric(station_coords[j,2])

			xy <- paste0(x, ",", y)

			this_sample <- samples_i[[xy]]

			# Asserting as 25x25 field format
			station_field_i[x,y] <- this_sample
		}

		# Error evaluation for busses
		vsn_voronoi_field <- voronoi_diagram(vsn_field_i, visited_coords, valid_coords)
		vsn_avre <- error_eval(vsn_voronoi_field, reference_field_i, valid_coords)
		vsn_avre_series[i] <- vsn_avre

		# Error evaluation for stations
		station_voronoi_field <- voronoi_diagram(station_field_i, str_station_coords, valid_coords)
		station_avre <- error_eval(station_voronoi_field, reference_field_i, valid_coords)
		station_avre_series[i] <- station_avre

		# Reference data
		reference_field_map <- reference_field_i
		reference_field_map[which(reference_field_map == 0)] <- NA

		# Image file names
		vsn_voronoi_img <- paste0(GRAPHICS_PATH, i, "_", pol_var , "_VSN_AVRE.eps")
		station_voronoi_img <- paste0(GRAPHICS_PATH, i, "_", pol_var , "_Station_AVRE.eps")
		reference_field_img <- paste0(GRAPHICS_PATH, i, "_", pol_var , "_Reference.eps")


		# Exporting images
		setEPS()
		postscript( vsn_voronoi_img )
		#png( vsn_voronoi_img )
		image(	vsn_voronoi_field, 
			xaxt='n', yaxt='n', frame.plot=FALSE)

		legend("topright", legend=paste0( pol_var, " Pollution Map" ), cex=1.75, bty='n')
		dev.off()

		setEPS()
		postscript( station_voronoi_img )
		#png( station_voronoi_img )
		image(	station_voronoi_field, 
			xaxt='n', yaxt='n', frame.plot=FALSE)

		legend("topright", legend=paste0( pol_var, " Pollution Map" ), cex=1.75, bty='n')
		dev.off()

		setEPS()
		postscript( reference_field_img )
		#png( reference_field_img )
		image(	reference_field_map, 
			xaxt='n', yaxt='n', frame.plot=FALSE)

		legend("topright", legend=paste0( pol_var, " Pollution Map" ), cex=1.75, bty='n')
		dev.off()
	}

	cat('\n')

	vsn_var_avre_series[[pol_var]] <- vsn_avre_series
	station_var_avre_series[[pol_var]] <- station_avre_series

	var_avre_summary <- cbind(vsn_avre_series, station_avre_series)
	avre_summary_filename <- paste0(RESULTS_PATH, pol_var, "_AVRE.csv")

	write.csv(var_avre_summary, file=avre_summary_filename)
}

# -----------------------------------------------
# After generate .csv files, plot graphics

AVRE_CO <- read.csv( paste0(RESULTS_PATH, "CO_AVRE.csv") )
AVRE_MP10 <- read.csv( paste0(RESULTS_PATH, "MP10_AVRE.csv") )
AVRE_NO2 <- read.csv( paste0(RESULTS_PATH, "NO2_AVRE.csv") )
AVRE_O3 <- read.csv( paste0(RESULTS_PATH, "O3_AVRE.csv") )
AVRE_SO2 <- read.csv( paste0(RESULTS_PATH, "SO2_AVRE.csv") )

AVRE_CO_graphic <- paste0(GRAPHICS_PATH, "AVRE_CO_summary.png")
AVRE_MP10_graphic <- paste0(GRAPHICS_PATH, "AVRE_MP10_summary.png")
AVRE_NO2_graphic <- paste0(GRAPHICS_PATH, "AVRE_NO2_summary.png")
AVRE_O3_graphic <- paste0(GRAPHICS_PATH, "AVRE_O3_summary.png")
AVRE_SO2_graphic <- paste0(GRAPHICS_PATH, "AVRE_SO2_summary.png")


# -------------------------
png( AVRE_CO_graphic )
plot(AVRE_CO[,1], AVRE_CO[,2] ,ylim=c(0,100), type='l', lty=5, pch=4, 
     col='darkgreen', ylab='Error (%)', xlab='Time (hours)', 
     main='Summary of Absolute Value of Relative Error (CO)')

points(AVRE_CO[,3], type='l',lty=1, col='red')
legend("topright", legend=c("VSN Monitoring", "CSMN Monitoring"), 
	lty=c(5,1), col=c("darkgreen","red"))
dev.off()
# -------------------------

# -------------------------
png( AVRE_MP10_graphic )
plot(AVRE_MP10[,1], AVRE_MP10[,2] ,ylim=c(0,100), type='l', lty=5, pch=4, 
     col='darkgreen', ylab='Error (%)', xlab='Time (hours)', 
     main='Summary of Absolute Value of Relative Error (MP10)')
 
points(AVRE_MP10[,3], type='l',lty=1, col='red')

legend("topright", legend=c("VSN Monitoring", "CSMN Monitoring"), 
	lty=c(5,1), col=c("darkgreen","red"))
dev.off()
# -------------------------

# -------------------------
png( AVRE_NO2_graphic )
plot(AVRE_NO2[,1], AVRE_NO2[,2] ,ylim=c(0,100), type='l', lty=5, pch=4, 
     col='darkgreen', ylab='Error (%)', xlab='Time (hours).', 
     main='Summary of Absolute Value of Relative Error (NO2)')

points(AVRE_NO2[,3], type='l',lty=1, col='red')
legend("topright", legend=c("VSN Monitoring", "CSMN Monitoring"), 
	lty=c(5,1), col=c("darkgreen","red"))
dev.off()
# -------------------------

# -------------------------
png( AVRE_O3_graphic )
plot(AVRE_O3[,1], AVRE_O3[,2] ,ylim=c(0,100), type='l', lty=5, pch=4, 
     col='darkgreen', ylab='Error (%)', xlab='Time (hours)',
     main='Summary of Absolute Value of Relative Error (O3)')

points(AVRE_O3[,3], type='l',lty=1, col='red')
legend("topright", legend=c("VSN Monitoring", "CSMN Monitoring"), 
	lty=c(5,1), col=c("darkgreen","red"))
dev.off()
# -------------------------

# -------------------------
png( AVRE_SO2_graphic )
plot(AVRE_SO2[,1], AVRE_SO2[,2] ,ylim=c(0,100), type='l', lty=5, pch=4, 
     col='darkgreen', ylab='Error (%)', xlab='Time (hours)',
     main='Summary of Absolute Value of Relative Error (SO2)') 
points(AVRE_SO2[,3], type='l',lty=1, col='red')
legend("topright", legend=c("VSN Monitoring", "CSMN Monitoring"), 
	lty=c(5,1), col=c("darkgreen","red"))
dev.off()


