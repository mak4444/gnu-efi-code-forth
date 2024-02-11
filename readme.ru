
Создание загрузочной USBфлеш с ФортОС
Вообще, достаточно скопировать:
cp disk_nmt/EFI/BOOT/bootx64.efi USB/EFI/BOOT/
Флеш должна быть с фагом boot.
Для соврвменных биoсах требуется отключить защиту в настройках UEFI 
https://habr.com/ru/articles/680270/
https://habr.com/ru/articles/446072/

при запуске make создается новый /x86_64/apps/forth.efi 

./copyefi.sh копирует в каталог эмулирующмй диск для qemu

./qemuboot.sh запуск  qemu с ./disk_nmt в качестве загрузочного диска

sudo apt-get install qemu-system
sudo apt -y install ovmf

see also
http://wiki.forth.org.ru/ForthOS#0
http://fpauk.narod.ru/BochsSPF-OFW.tar.xz
https://github.com/openbios/openbios

