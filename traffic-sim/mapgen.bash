#!/bin/bash
# Created by Israel Vasconcelos at Dec/2020
# Federal University of Alagoas

south_lat=-24.01
south_lon=-46.83
north_lat=-23.35
north_lon=-46.36

osm_dir=$HOME/maps
mkdir $osm_dir

#-------------------------------------------------------------
# Query for an entire city is too large, splitting in n=6 parts

lat_step_1=$(echo "scale=4; $south_lat + ($north_lat - $south_lat)/3" | bc -l)
lat_step_2=$(echo "scale=4; $south_lat + 2*($north_lat - $south_lat)/3" | bc -l)

lon_step=$(echo "scale=4; $south_lon + ($north_lon - $south_lon)/2" | bc -l)

p_south_lat=$south_lat
p_south_lon=$south_lon

n=1

for p_north_lat in $lat_step_1 $lat_step_2 $north_lat
do
	for p_north_lon in $lon_step $north_lon
	do
		wget -O $osm_dir/sp_bbox_$n.osm \
		'http://overpass-api.de/api/interpreter?data=(
		node('$p_south_lat','$p_south_lon','$p_north_lat','$p_north_lon');
		way('$p_south_lat','$p_south_lon','$p_north_lat','$p_north_lon');
		relation('$p_south_lat','$p_south_lon','$p_north_lat','$p_north_lon');
		);
		out;' --random-wait -r -p

		p_south_lon=$p_north_lon

		#---------------------------------------------
		# Generate network
		nohup netconvert --osm-files \
		$osm_dir/sp_bbox_$n.osm -o\
		$osm_dir/sp_bbox_$n.net.xml --osm.stop-output.length 20 --ptstop-output \
		$osm_dir/osm_stops_$n.add.xml --ptline-output \
		$osm_dir/osm_ptlines_$n.xml &

		n=$((n+1))

	done

	p_south_lat=$p_north_lat
	p_south_lon=$south_lon

done

#-------------------------------------------------------------
