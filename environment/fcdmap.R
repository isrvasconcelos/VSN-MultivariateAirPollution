# Created by Israel Vasconcelos at Dec/2020
# Federal University of Alagoas

# -----------------------------------------------------------------
# Assemble each bounding box and give (x,y) offset: Available at files sp_bbox_(i).net.xml
# Future improvement: Read the coordinates automatically from .net.xml

# BBOX 1
#<location netOffset="-313550.78,2656332.57" convBoundary="0.00,0.00,24131.19,24378.38" origBoundary="-46.829994,-24.009894,-46.595027,-23.790006" projParameter="+proj=utm +zone=23 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"/>

# BBOX 2
#<location netOffset="-337526.91,2656137.40" convBoundary="0.00,0.00,24102.01,24482.42" origBoundary="-46.595000,-24.009997,-46.360020,-23.790004" projParameter="+proj=utm +zone=23 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"/>

# BBOX 3
#<location netOffset="-313237.93,2632143.32" convBoundary="0.00,0.00,24248.61,24608.92" origBoundary="-46.829999,-23.789998,-46.595001,-23.570002" projParameter="+proj=utm +zone=23 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"/>

# BBOX 4
#<location netOffset="-313237.93,2632143.32" convBoundary="0.00,0.00,48150.84,24859.67" origBoundary="-46.829999,-23.789998,-46.360001,-23.570001" projParameter="+proj=utm +zone=23 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"/>

# BBOX 5
#<location netOffset="-312934.80,2607806.54" convBoundary="0.00,0.00,24280.14,24627.68" origBoundary="-46.829997,-23.569998,-46.595002,-23.350001" projParameter="+proj=utm +zone=23 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"/>

# BBOX 6
#<location netOffset="-336970.29,2607520.19" convBoundary="0.00,0.00,24227.47,24583.13" origBoundary="-46.594998,-23.570000,-46.360001,-23.350007" projParameter="+proj=utm +zone=23 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"/>

#fcdmap <- function() {

TRACE_PATH <<- "trace/"
RESULTS_PATH <<- "results/"

source("auxiliar_summary_trace.R")

	full_coords_list <- vector()
	traffic_summary <- list()

	coords <- matrix(nrow=6, ncol=4)
	coords <- as.data.frame(coords)	
	names(coords) <- c("xmin","ymin","xmax","ymax")
	row.names(coords) <- c("bbox1","bbox2","bbox3","bbox4","bbox5","bbox6")

	# BBOX Col 1
	bbox_1_xmin <- 0
	bbox_1_ymin <- 0
	bbox_1_xmax <- 24131.19
	bbox_1_ymax <- 24378.38

	bbox_3_xmin <- 0
	bbox_3_ymin <- bbox_1_ymax
	bbox_3_xmax <- bbox_3_xmin + 24248.61
	bbox_3_ymax <- bbox_3_ymin + 24608.92

	bbox_5_xmin <- 0
	bbox_5_ymin <- bbox_3_ymax
	bbox_5_xmax <- bbox_5_xmin + 24280.14
	bbox_5_ymax <- bbox_5_ymin + 24627.68

	# BBOX Col 2
	bbox_2_xmin <- bbox_1_xmax
	bbox_2_ymin <- 0
	bbox_2_xmax <- bbox_2_xmin + 24102.01
	bbox_2_ymax <- bbox_2_ymin + 24482.42

	bbox_4_xmin <- bbox_2_xmin
	bbox_4_ymin <- bbox_2_ymax
	bbox_4_xmax <- bbox_4_xmin + 24248.61
	bbox_4_ymax <- bbox_4_ymin + 24859.67

	bbox_6_xmin <- bbox_4_xmin
	bbox_6_ymin <- bbox_4_xmax
	bbox_6_xmax <- bbox_6_xmin + 24227.47
	bbox_6_ymax <- bbox_6_ymin + 24583.13

	coords[1,] <- c(bbox_1_xmin, bbox_1_ymin, bbox_1_xmax, bbox_1_ymax)
	coords[2,] <- c(bbox_2_xmin, bbox_2_ymin, bbox_2_xmax, bbox_2_ymax)
	coords[3,] <- c(bbox_3_xmin, bbox_3_ymin, bbox_3_xmax, bbox_3_ymax)
	coords[4,] <- c(bbox_4_xmin, bbox_4_ymin, bbox_4_xmax, bbox_4_ymax)
	coords[5,] <- c(bbox_5_xmin, bbox_5_ymin, bbox_5_xmax, bbox_5_ymax)
	coords[6,] <- c(bbox_6_xmin, bbox_6_ymin, bbox_6_xmax, bbox_6_ymax)

	print(coords)

	# -----------------------------------------------------
	xmin <- min(coords$xmin)
	ymin <- min(coords$ymin)
	xmax <- max(coords$xmax)
	ymax <- max(coords$ymax)
	bounds <- as.data.frame(cbind(xmin, ymin, xmax, ymax))

	as25_coords <- read.csv("spcoords_25x25.csv")

	xmin <- min(as25_coords[,1])
	ymin <- min(as25_coords[,2])
	xmax <- max(as25_coords[,1])
	ymax <- max(as25_coords[,2])
	as25_bounds <- as.data.frame(cbind(xmin, ymin, xmax, ymax))

	scaled_x <- (bounds$xmax - bounds$xmin) / (as25_bounds$xmax - as25_bounds$xmin)
	scaled_y <- (bounds$ymax - bounds$ymin) / (as25_bounds$ymax - as25_bounds$ymin)
	# -----------------------------------------------------

	# -----------------------------------------------------
	all_files <- list.files(path=TRACE_PATH)
	trace_files <- which(grepl("trace", all_files) & grepl(".xml", all_files))
	trace_files <- all_files[trace_files]

	# Getting information at trace file names: [2] Bbox / [3] Seed / [4] Vehicle amount
	trace_names <- unlist(strsplit(trace_files, '.xml'))
	par_traces <- strsplit(trace_names, '_')

	# For each iteration, the equivalent offset will be applied to coords
	for(i in 1:length(trace_files)) {

		trace_file_i <- paste0(TRACE_PATH, trace_files[i])
		bbox_i <- par_traces[[i]][2] # Bbox info at index [2] from par_traces
		bbox_i <- as.numeric(gsub("bbox", "", bbox_i))

		x_offset <- coords$xmin[bbox_i]
		y_offset <- coords$ymin[bbox_i]

		# Asserting trace as data frame, offset coords and sectorize as 25x25
		trace_i <- summary_trace(trace_file_i, export=FALSE, offset=c(x_offset,y_offset))
		sectorized_trace <- as25_trace_coords(trace_i, scaled_x, scaled_y, as25_bounds$xmin, as25_bounds$ymin)

		# Matching trace coordinates with pollutant map samples
		bbox_coords_list <- as25_coords_match(sectorized_trace)
		traffic_summary[[trace_names[i]]] = bbox_coords_list
		print(bbox_coords_list)

		file_name <- paste0(TRACE_PATH,"as25_", strsplit(trace_files[i], ".xml"), ".csv")
		write.csv(sectorized_trace, file=file_name, row.names=FALSE)
	}

	# Match pol map timestamps ()

	#return(full_coords_list)
#}

# -----------------------------------------------------
# Assemble a data frame where:
# - Columns are vehicle count
# - Lines are seeds 
# - Cells stores entire set of bbox_coords_list for all bboxes in that seed

tbl_par_traces <- matrix(unlist(par_traces), ncol=4, byrow=TRUE)

df_col_prefix <- "veh_qt_" # To avoid naming problems with data frame columns
df_cols <- paste0(df_col_prefix , unique(tbl_par_traces[,4]) )
df_rows <- unique(tbl_par_traces[,3])

df_traffic_summary <- data.frame(matrix(ncol=1+length(df_cols), nrow=length(df_rows)))
colnames(df_traffic_summary) <- c("seed", df_cols)

for(i in 1:length(traffic_summary)) {

	# Information about trace file names: [2] Bbox / [3] Seed / [4] Vehicle amount
	trace_i <- names(traffic_summary)[i]
	trace_i <- strsplit(trace_i, '_')
	trace_i <- unlist(trace_i)

	seed <- as.numeric(trace_i[3])
	veh_qt <- paste0(df_col_prefix, trace_i[4]) # Appending prefix to avoid naming problems with df cols
	coords_to_cell <- paste(traffic_summary[[i]], collapse=' ')

	df_traffic_summary$seed[seed] <- seed

	#if( !is.element(df_traffic_summary$seed, seed) )
	#	df_traffic_summary$seed[seed] <- seed

	# Appending all bboxes at each respective cell
	if( is.na(df_traffic_summary[[veh_qt]][seed]) ) 
		df_traffic_summary[[veh_qt]][seed] <- coords_to_cell
	else
		df_traffic_summary[[veh_qt]][seed] <- paste(df_traffic_summary[[veh_qt]][seed],
							    coords_to_cell, collapse=' ')

}

dts_file <- paste0(RESULTS_PATH, "traffic_coords_summary.csv")
write.csv(df_traffic_summary, file=dts_file,  row.names=FALSE)
