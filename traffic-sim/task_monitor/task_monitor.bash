duarouter_nthread_max=6
python_nthread_max=5
free_ram_threshold=35

log_dir=log_files

#-----------------------------------------------------
# Output 
free_ram=$(free -m | grep Mem | awk '{print ($4/$2)*100}') 
echo "Free RAM (%): $free_ram " >> $log_dir/sequence_duarouter.out

#-----------------------------------------------------
# Multithread to handle CPU overload: Check duarouter/python instances
# Hold new process triggering if resources usage are above the threshold 

duarouter_pidlist=$(pgrep duarouter)
echo "Duarouter pids"  >> $log_dir/sequence_duarouter.out
echo $duarouter_pidlist >> $log_dir/sequence_duarouter.out

while true
do
	sleep 2s
	free_ram=$(free -m | grep Mem | awk '{print ($4/$2)*100}') 

	duarouter_pidlist=$(pgrep duarouter)
	duarouter_pidcount=$(echo $duarouter_pidlist | wc -w)

	python_pidlist=$(pgrep python)
	python_pidcount=$(echo $python_pidlist | wc -w)

	#-----------------------------------------------------
	# Check RAM and number of active processes before go ahead
	if [[ $free_ram_threshold < $free_ram ]] 
	then
		if [[ $duarouter_pidcount < $duarouter_nthread_max ]] 
		then
			if [[ $python_pidcount < $python_nthread_max ]] 
			then
				break
			fi
		fi
	fi
done

echo "Slot available at Duarouter process queue. Ready to trigger new tasks." >> $log_dir/sequence_duarouter.out
