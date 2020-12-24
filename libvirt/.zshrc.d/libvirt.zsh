set-vm-mouse() {
	mouse_path=$(ls -d /dev/input/by-id/* | fzf)
	echo "Selected mouse: ${mouse_path}"
	sudo ln -sf "${mouse_path}" /var/lib/libvirt/inputs/event-mouse
}

set-vm-keyboard() {
	keyboard_path=$(ls -d /dev/input/by-id/* | fzf)
	echo "Selected keyboard: ${keyboard_path}"
	sudo ln -sf "${keyboard_path}" /var/lib/libvirt/inputs/event-keyboard
}

# This function generates all the functions to control the VMs managed by libvirt
# Many things extract from https://rokups.github.io/#!pages/gaming-vm-performance.md
make-vm() {
	local vm_name="${1}"
	local reserved_host_cpus="${2}"
	local reserved_guest_cpus="${3}"
	local vm_functions

	read -r -d '' vm_functions <<EOF
dump-${vm_name}() {
	sudo virsh dumpxml "${vm_name}" >! "${DOTFILES_PATH}/_libvirt/${vm_name}.xml"
}

define-${vm_name}() {
	sudo virsh define "${DOTFILES_PATH}/_libvirt/${vm_name}.xml"
}

start-${vm_name}() {
	# Make sure we have sufficient memory for hugepages
	sync
	sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'
	sudo sh -c 'echo 1 > /proc/sys/vm/compact_memory'

	# Reduce VM jitter: https://www.kernel.org/doc/Documentation/kernel-per-CPU-kthreads.txt
	sudo sysctl vm.stat_interval=120
	# the kernel's dirty page writeback mechanism uses kthread workers. They introduce
	# massive arbitrary latencies when doing disk writes on the host and aren't
	# migrated by cset. Restrict the workqueue to use only cpu 0.
	sudo sh -c 'echo 00 > /sys/bus/workqueue/devices/writeback/cpumask'

	sudo cset shield --reset
	sudo cset shield --cpu "${reserved_guest_cpus}" --kthread=on
	sudo virsh start "${vm_name}"
}

stop-${vm_name}() {
	sudo virsh shutdown "${vm_name}"
	# All VMs offline
	sudo sh -c 'echo ff > /sys/bus/workqueue/devices/writeback/cpumask'
	sudo sysctl vm.stat_interval=1
	sudo cset shield --reset
}
EOF
	eval "${vm_functions}"
}

make-vm win10 0-1 2-5
