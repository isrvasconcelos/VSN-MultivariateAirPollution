#!/bin/bash
# Created by Israel Vasconcelos at Jan/2021
# This script avoids machine to freeze due to high (>95%) RAM usage
# Kill all processes and halt the simulation

echo "RAM usage monitor triggered to avoid memory overhead."
echo "This monitor will halt the execution if RAM usage is higher than 98%."

while true
do
	free_ram=$(free -m | grep Mem | awk '{print ($4/$2)*100}') 

	if [[ $free_ram < 2 ]] 
	then
		break

	fi

	sleep 3s
done

echo "High RAM usage: Aborting execution and kill remaining tasks."
duarouter_pidlist=$(pgrep duarouter)
sumo_pidlist=$(pgrep sumo)
python_pidlist=$(pgrep python)

echo "Duarouter threads:"
echo  $duarouter_pidlist
echo "SUMO threads:"
echo  $sumo_pidlist
echo "Python threads:"
echo  $python_pidlist

while true
do
	killall -9 sumo
	killall -9 duarouter
	killall -9 python

	sleep 1s
done

