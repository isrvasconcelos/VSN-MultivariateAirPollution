#!/bin/bash
# Created by Israel Vasconcelos at Dec/2020
# Running traffic: This step requires a long time and high RAM usage
#-------------------------------------------------------------
# Parameters
osm_dir=$HOME/maps
cfg_dir=cfg_files
log_dir=log_files
sample_time=900 # seconds
sim_time_limit=5401 # seconds
veh_qt=0
sumo_nthread_max=7

#-------------------------------------------------------------
# Parsing parameters

while echo $1 | grep -q ^-; do
    # Evaluating a user entered string!
    # Red flags!!!  Don't do this
    eval $( echo $1 | sed 's/^-//' )=$2
    shift
    shift
done

echo seed = $seed
echo car_rate = $car_rate

#-----------------------------------------------------
# Multithread to handle CPU overload: Check sumo instances

while true
do
	sumo_pidlist=$(pgrep sumo)
	echo sumo_pidlist | wc -w
	sumo_pidcount=$(echo $sumo_pidlist | wc -w)

	if [[ $sumo_pidcount < $sumo_nthread_max ]] 
	then
		break
	fi

	sleep 1s
done

echo "Slot available at SUMO Process queue: Triggering traffic in bbox sequence." >> $log_dir/sequence_traffic.out

#-----------------------------------------------------
# Triggering traffic simulation

for n in {1..6}
do

	if [[ $car_rate == '0.2' ]]
	then
		veh_qt=15000
	fi

	if [[ $car_rate == '0.1' ]]
	then
		veh_qt=30000
	fi

	if [[ $car_rate == '0.05' ]]
	then
		veh_qt=60000
	fi

	echo '<?xml version="1.0" encoding="UTF-8"?>

	<configuration xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://sumo.dlr.de/xsd/sumoConfiguration.xsd">

	    <input>
		<net-file value="'$osm_dir/'sp_bbox_'$n'.net.xml"/>
		<route-files value="'$osm_dir/'osm_pt_'$n'_'$seed'.rou.xml,'$osm_dir/'osm_bbox_'$n'_'$seed'_'$car_rate'.passenger.trips.xml,'$osm_dir/'osm_bbox_'$n'_'$seed'.bus.trips.xml"/>
		<additional-files value="'$osm_dir/'osm_stops_'$n'.add.xml"/>
	    </input>

	    <processing>
		<ignore-route-errors value="true"/>
	    </processing>

	    <routing>
		<device.rerouting.adaptation-steps value="180"/>
	    </routing>

	    <report>
		<verbose value="true"/>
		<duration-log.statistics value="true"/>
		<no-step-log value="true"/>
	    </report>

	    <gui_only>
		<start value="false"/>
	    </gui_only>

	</configuration>' > $cfg_dir/sp_bbox_$n'_'$seed'_'$veh_qt.sumo.cfg

	#-----------------------------------------------------
	# Running scenario
	nohup sumo -c  $cfg_dir/sp_bbox_$n'_'$seed'_'$veh_qt.sumo.cfg \
	--device.fcd.period $sample_time \
	--end $sim_time_limit \
	--max-num-vehicles $veh_qt \
	--fcd-output ../environment/trace/trace_bbox$n'_'$seed'_'$veh_qt.xml \
	>> $log_dir/sumo_out.out &

	sleep 1s

	#-----------------------------------------------------
	# Multithread to handle CPU overload: Check sumo instances
	while true
	do
		sumo_pidlist=$(pgrep sumo)
		sumo_pidcount=$(echo $sumo_pidlist | wc -w)

		if [[ $sumo_pidcount < $sumo_nthread_max ]] 
		then
			break
		fi

		sleep 1s
	done

	echo "Slot available at SUMO Process queue. Triggering bbox_$n." >> $log_dir/sequence_traffic.out	

done

echo "Triggered entire bbox sequence for Seed: $seed, Number of vehicles: $veh_qt." >> $log_dir/sequence_traffic.out




