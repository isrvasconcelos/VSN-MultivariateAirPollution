#!/bin/bash
# Created by Israel Vasconcelos at Dec/2020

#-------------------------------------------------------------
# OSM handling: Placing large map files outside repository 
osm_dir=$HOME/maps
log_dir=log_files
tm_dir=task_monitor
time=3600
bus_rate=1

#-------------------------------------------------------------
# Daemon to kill application in case of high RAM usage 
nohup bash $tm_dir/killtask.bash > $log_dir/killtask.tmp 2>&1 &
echo $! >> $log_dir/killtask_pid.out

pid_killtask=$(echo $!)

echo "Starting traffic generation, RAM usage monitor enabled." ; echo

for seed in {1..7}
do
	for car_rate in 0.2 0.1 0.05
	do
		
		echo "Generating routes: Seed: $seed, Car rate: $car_rate." >> $log_dir/sequence_duarouter.out
		echo "Current task - Seed: $seed, Car rate: $car_rate." >> $log_dir/killtask.tmp

		for n in {1..6}
		do

			#-----------------------------------------------------
			# Running task monitor
			bash $tm_dir/task_monitor.bash

			#-----------------------------------------------------
			# Generate overall traffic parameters
			python "$SUMO_HOME/tools/ptlines2flows.py" -n \
			$osm_dir/sp_bbox_$n.net.xml -e $time -p 600 --random-begin --seed $seed --ptstops \
			$osm_dir/osm_stops_$n.add.xml --ptlines \
			$osm_dir/osm_ptlines_$n.xml -o \
			$osm_dir/osm_pt_$n'_'$seed.rou.xml --ignore-errors --vtype-prefix pt_ --stopinfos-file \
			$osm_dir/stopinfos_$n'_'$seed.xml --routes-file \
			$osm_dir/vehroutes_$n'_'$seed.xml --trips-file \
			$osm_dir/trips_$n'_'$seed.trips.xml --min-stops 0 --extend-to-fringe --verbose \

			sleep 1s

			#-----------------------------------------------------
			# Generate car/bus routes
			nohup python "$SUMO_HOME/tools/randomTrips.py" -n \
			$osm_dir/sp_bbox_$n.net.xml --fringe-factor 5 -p $car_rate -o \
			$osm_dir/osm_bbox_$n'_'$seed'_'$car_rate.passenger.trips.xml -e $time \
			--vehicle-class passenger --vclass passenger --prefix veh --min-distance 300 \
			--trip-attributes "departLane=\"best\"" \
			--fringe-start-attributes "departSpeed=\"max\"" \
			--allow-fringe.min-length 1000 --lanes --validate &

			nohup python "$SUMO_HOME/tools/randomTrips.py" -n \
			$osm_dir/sp_bbox_$n.net.xml --fringe-factor 5 -p $bus_rate -o \
			$osm_dir/osm_bbox_$n'_'$seed.bus.trips.xml -e $time \
			--vehicle-class bus --vclass bus --prefix bus --min-distance 600 \
			--fringe-start-attributes "departSpeed=\"max\"" \
			--trip-attributes "departLane=\"best\"" --validate &
		done

		sleep 2s

		echo "" >> $log_dir/sequence_duarouter.out

		#echo "Triggering SUMO." >> $log_dir/sequence_duarouter.out
		# Running traffic on generated routes
		#bash runtraffic.bash -seed $seed -car_rate $car_rate


	done
done


#-----------------------------------------------------
# Last iteration was triggered, waiting all processes to finish before stop this script.
echo "Last iteration was triggered, waiting all processes to finish."
sleep 1s

while true
do
	duarouter_pidlist=$(pgrep duarouter)
	if [[ $duarouter_pidlist == '' ]] 
	then
		break
	fi


	sleep 1s
done

echo "Tasks finished, stopping RAM Monitor."
kill -9 $pid_killtask
rm $log_dir/killtask_pid.out
echo "Done!"

