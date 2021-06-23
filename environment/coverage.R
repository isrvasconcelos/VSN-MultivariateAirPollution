# Created by Israel Vasconcelos at Dec/2020
# Federal University of Alagoas

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
# Coverage is the same for all variables, taking just one index for this reason

# String operations before match coordinates
map_coords <- gsub('X', '', names(pol_maps[[1]]))

# Removing first "not-coordinate" columns 
map_coords <- map_coords[which(map_coords != c('','Date', 'Time'))]

# Fixing separators (converted to "." at csv importing)
map_coords <- gsub(SEP, ',', map_coords)

# -----------------------------------------------
# Coordinates match and evaluation for overall coverage in %

for(i in 1:nrow(coverage_traffic_map)) {
	for(veh_qt_col in ctm_veh_cols) {

		# Splitting trace coords stored as string
		trace_coords <- toString(coverage_traffic_map[[veh_qt_col]][i])
		trace_coords <- unlist(strsplit(trace_coords, ' '))
		trace_coords <- trace_coords[which(trace_coords != "")] # Removing empty cells

		coords_match <- intersect(map_coords, trace_coords)

		coverage <- length(coords_match) / length(map_coords)
		coverage <- round( 100*coverage, 2 )

		coverage_traffic_map[[veh_qt_col]][i] <- coverage

	}
}

# -----------------------------------------------------------------
# Assigning different traffic densities to each part of the day (temporal coverage)

# Removing prefix to define traffics as veh_qt[1]: Light / veh_qt[2]: Avg / veh_qt[3]: Heavy
veh_qt = sort(as.numeric(gsub('pct_coverage_veh_qt_','', ctm_veh_cols)))

# Final column to be appended to data frame
overall_coverage <- "overall_coverage"
coverage_traffic_map[[overall_coverage]] <- rep(NA, nrow(coverage_traffic_map))

for(i in 1:nrow(coverage_traffic_map)) {

	traffic_coverage_seed_i <- 0

	for(veh_qt_col in ctm_veh_cols) {

		# Extracting traffic intensity
		veh_qt_i <- unlist(strsplit(veh_qt_col ,'_'))
		veh_qt_i <- tail(veh_qt_i, 1)
		veh_qt_i <- as.numeric(veh_qt_i)

		traffic_intensity = which(veh_qt == veh_qt_i)
		part_tc <- 0 # tc: traffic coverage

		# -----------------------------------------------
		# Light traffic: from 0h to 6h
		if(traffic_intensity == 1) { # Light
			part_tc <- as.numeric(coverage_traffic_map[[veh_qt_col]][i])
			part_tc <- part_tc*(6/24)
		}

		# -----------------------------------------------
		# Avg traffic: from 6h to 7h / 9h to 11h / 13h to 17h / 19h to 0h
		else if(traffic_intensity == 2) { # Average
			part_tc <- as.numeric(coverage_traffic_map[[veh_qt_col]][i])
			part_tc <- part_tc*(12/24)
		}
		# -----------------------------------------------
		# Heavy traffic: from 7h to 9h / 11h to 13h / 17h to 19h
		else if(traffic_intensity == 3) { # Heavy
			part_tc <- as.numeric(coverage_traffic_map[[veh_qt_col]][i])
			part_tc <- part_tc*(6/24)
		}

		traffic_coverage_seed_i <- traffic_coverage_seed_i + part_tc
	}

	coverage_traffic_map[[overall_coverage]][i] <- round(traffic_coverage_seed_i, 2)
}

file_name <- paste0(RESULTS_PATH, "traffic_coverage_summary.csv")
write.csv(coverage_traffic_map, file_name, row.names=FALSE)

