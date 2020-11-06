for n in $(seq 1 15)
do
	gams DIETER_v1.0.2_$n.gms -logoption=2 &
done
