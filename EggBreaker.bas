 rem **********************************
 rem Egg Breaker
 rem LMC 2700
 rem Project 6
 rem Lilliann Andrews, Sranee Bayapureddy, David Le
 rem **********************************

 set romsize 4k
 set kernel_options pfcolors

 pfcolors:
 $00
 $00
 $40
 $40
 $40
 $40
 $40
 $40
 $00
 $00
 $00
end
 playfield:
 ................................
 ................................
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 ................................
 ................................
  ................................
end

 COLUBK = $9E

draw_loop
 drawscreen
 goto draw_loop
