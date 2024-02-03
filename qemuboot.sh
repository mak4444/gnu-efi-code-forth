#sudo apt -y install ovmf
qemu-system-x86_64 -enable-kvm -bios /usr/share/ovmf/OVMF.fd  -hda fat:rw:./disk_nmt
