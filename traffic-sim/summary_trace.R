# -----------------------------------------------------------------
#route_loop <- function(name_, points) {



#}
# -----------------------------------------------------------------

# Extracting from XML to assemble as a data frame
# Bus ids in columns, timestamps in rows, x-y coordinates at each cell 

library(XML)

trace <- xmlToList("trace.xml")

# First brackets level: Timestep 
trace[[1]][[1]][1]
trace[[1]][[1]][2]
trace[[1]][[1]][3]


steps <- length(trace)-1
bus_list <- NULL

for(t in 2:steps) {

	vehicles <- length(trace[[t]])-1

	for(veh_id in 1:vehicles) {

		id <- toString(trace[[t]][[veh_id]][1])
		x  <- trace[[t]][[veh_id]][2]
		y  <- trace[[t]][[veh_id]][3]

		if(!any(bus_list == id)) # Check non repeated elements before append
			bus_list <- append(bus_list, id)

		#message(id," - ", x, "," , y)
	}

}
