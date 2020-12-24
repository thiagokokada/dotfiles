# This function generates all the functions to control the VMs managed by
# libvirt
make-vm() {
	local vm_name="${1}"
	local reserved_guest_cpus="${2:-none}"
	local vm_functions

	read -r -d '' vm_functions <<EOF
dump-${vm_name}() {
	sudo virsh dumpxml "${vm_name}" >! "${DOTFILES_PATH}/_libvirt/${vm_name}.xml"
}

define-${vm_name}() {
	sudo virsh define "${DOTFILES_PATH}/_libvirt/${vm_name}.xml"
}

start-${vm_name}() {
	sudo sh -c 'echo 3 > /proc/sys/vm/drop_caches'
	if [[ "${reserved_guest_cpus}" != "none" ]]; then
		sudo cset shield --reset
		sudo cset shield --cpu "${reserved_guest_cpus}" --kthread=on
	fi
	sudo virsh start "${vm_name}"
}

stop-${vm_name}() {
	sudo virsh shutdown "${vm_name}"
	if [[ "${reserved_guest_cpus}" != "none" ]]; then
		sudo cset shield --reset
	fi
	sudo virsh start "${vm_name}"
}
EOF
	eval "${vm_functions}"
}

make-vm win10 2-5
