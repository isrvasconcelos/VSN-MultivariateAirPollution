#netconvert --osm-files map.osm -o saopaulo.net.xml
#polyconvert --osm map.osm -o saopaulo.poly.xml


#########
randomtrip --vehicle-class passenger --prefix car -n saopaulo.net.xml -l -e 500 -o saopaulo_car.trips.xml

randomtrip --vehicle-class bus --prefix public -n saopaulo.net.xml -l -e 500 -o saopaulo_bus.trips.xml


#########
duarouter -n saopaulo.net.xml -t saopaulo_car.trips.xml -o saopaulo_car.rou.xml --ignore-errors

duarouter -n saopaulo.net.xml -t saopaulo_bus.trips.xml -o saopaulo_bus.rou.xml --ignore-errors


#########
#sumo -c saopaulo.sumo.cfg --fcd-output trace.xml




# Misc notes
#-b t0 -e t1 -p ((t1 - t0) / n)
# Set color -> color="1,0,0"
