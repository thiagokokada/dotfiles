start-win10() {
	sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'
	sudo cset shield --reset
	sudo cset shield --cpu 2-5 --kthread=on
	sudo virsh start win10
}


stop-win10() {
	sudo virsh shutdown win10
  sudo cset shield --reset
}
