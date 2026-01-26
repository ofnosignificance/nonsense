set disassembly intel
set auto-load safe-path /
define getqemu
  target remote localhost:1234 
end
