library("hash")

# File handling strings
dataset_path = "CETESB_INPE (Raw data)/"
dataset_files = list.files(path=dataset_path)


# Empty lists
dataset_variables = hash()
frequency_stations = {}
frequency_dates = {}
frequency_times = {}
station_names = {}
id_name_station = {}

# Reading files
for(var_file in dataset_files) {

	temp_file = paste(dataset_path, var_file, sep='')
	temp_table = read.table(temp_file, sep=';')

	var_file = gsub('.{9}$','',var_file)# Fixing name removing '-2005.txt' substring
	dataset_variables[[var_file]] = temp_table

	paste(cat(var_file, ' '), cat( dim(temp_table) , '\n'))

	#cat(levels(temp_table$V1), '\n')

	# Converting to numeric and removing NAs
	frequency_stations = append(frequency_stations, levels(temp_table$V1))
	frequency_dates = append(frequency_dates,levels(temp_table$V2))
	frequency_times = append(frequency_times, levels(temp_table$V3))
	station_names = append(station_names, levels(temp_table$V6))

	str_id_name_station = paste(temp_table$V1,temp_table$V6)
	id_name_station = append(id_name_station, str_id_name_station)

}

# Getting the frequency of occurrence at each element 
# Data fix: Column names on the last index are removed 
frequency_stations = head(table(frequency_stations), -1) 
frequency_dates = head(table(frequency_dates), -1)
frequency_times = head(table(frequency_times), -1)
station_names = table(station_names)
id_name_station = names(head(table(id_name_station), -1))

# Handling by station IDs in relation to its respective sensors
station_sensors = list()
sp_capital_stations = c("1,00","2,00","3,00","4,00","5,00",
			"8,00","12,00","16,00","27,00","47,00")
# 9 and 21 excluded due lack of samples

cat("\n")

temp_list = list()

dates = sort(as.Date(as.factor(names(frequency_dates)), format="%d/%m/%Y"))
times = sort(names(frequency_times))

# Filter by month with more available data
filter_october <- which(grepl("2005-10",dates) == TRUE)
dates_filtered <- dates[filter_october]
dates <- dates_filtered[15:length(dates_filtered)] # Getting dates after day 15 (15-31)
###

df_names = c("Date", "Time")
rows = length(dates)*length(times)
cols = 2

sensors_by_station = data.frame(matrix(nrow=rows , ncol=cols))

sensors_by_station = setNames(sensors_by_station, df_names)

sensors_by_station[,1] = sort(rep(dates, length(times)))
sensors_by_station[,2] = times

it = 1
for(station in sp_capital_stations) { # browse only at listed IDs

	for(sensor_name in names(dataset_variables)) {
		stations = levels(dataset_variables[[sensor_name]]$V1)
		var_match = as.logical(length(which(stations == station)))

		if(var_match) {
			station_sensors[[station]] = append(station_sensors[[station]],
							    sensor_name)
		}
	}

	###############################################################################
	# Progress bar

	total = length(sp_capital_stations)

	# Progress
	progress = ceiling(100*it/total)
	cat('\r',format(paste("Progress: ", progress, "%", sep='')))
	flush.console()

	###############################################################################
	# Creating csv file by station

	temp_station = as.numeric(gsub(",",".",station))

	station_cols = length(station_sensors[[station]])
	col_names = paste(temp_station,station_sensors[[station]],sep="_")

	station_samples = data.frame(matrix(nrow=rows , ncol=station_cols))
	station_samples = setNames(station_samples,col_names)

	sensors_by_station = cbind(sensors_by_station, station_samples)

	# Formating data from imported files
	for(i in 1:length(station_sensors[[station]])) {
		
		sensor_name = station_sensors[[station]][i]

		sensor_dates <- as.Date(
				as.factor(
				dataset_variables[[sensor_name]]$V2),
				format="%d/%m/%Y"
				)

		sensor_timestamps <- dataset_variables[[sensor_name]]$V3

		sensor_datetimes <- paste(sensor_dates, sensor_timestamps)

		df_datetimes = paste(sensors_by_station$Date, sensors_by_station$Time)


		for(j in 1:nrow(sensors_by_station)) {

			index_match <- which(df_datetimes[j] == sensor_datetimes)

			station_match <- which(dataset_variables[[sensor_name]]$V1[index_match] == station)
			
			if(as.logical(length(station_match))) {

				# Iterator to shift pointer to correct column
				col_shift = ncol(sensors_by_station) - station_cols

				sensor_index = index_match[station_match]
				sensor_sample = dataset_variables[[sensor_name]]$V4[sensor_index]

				sensor_sample = as.numeric(gsub(",",".",sensor_sample))

				sensors_by_station[j,(i+col_shift)] = sensor_sample

				#print(paste("Station:", station_match,"-",sensor_name))
				#print((sensors_by_station[j,(i+col_shift)]))
				#print("-")


			}

		}
	}

	it = it+1
}

message("\nDone!")

# Selecting NA columns
na_col_drop = NULL
for(i in 1:ncol(sensors_by_station)) {

	if( all(is.na(sensors_by_station[,i])) ) 
		na_col_drop = c(na_col_drop, i)

}

# Removing NA columns, if exists
if(!is.null(na_col_drop))
	sensors_by_station <- subset(sensors_by_station, select=-na_col_drop)

# Exporting
write.csv(sensors_by_station, file="summary_table.csv", row.names=FALSE)

