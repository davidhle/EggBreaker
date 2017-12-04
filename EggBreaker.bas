 rem **********************************
 rem Egg Breaker
 rem LMC 2700
 rem Project 6
 rem Lilliann Andrews, Sranee Bayapureddy, David Le
 rem **********************************

 set romsize 4k
 set kernel_options pfcolors

 rem setting colors for the playfield's rows
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

 player0color:
 $0E
 $0E
 $0E
 $0E
 $0E
 $0E
 $0E
 $0E
end

 player1color:
 $66
 $66
 $0E
 $0E
 $0E
 $0E
 $0E
 $0E
end

 rem drawing the playfield
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

 rem defining the player
 player0:
 %11111111
 %11111111
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
 %00000000
end

 player1:
 %01111110
 %11111111
 %11111111
 %11100111
 %11111111
 %11011011
 %01111110
 %00111100
end


 player0x = 75
 player0y =88

 player1x = 30
 player1y = 16

 rem displays the screen
draw_loop
 rem color of background
 COLUBK = $9E
 COLUP0 = 14
 COLUP1 = 14

 drawscreen
 if joy0right then player0x = player0x + 1: if player0x > 153 then player0x = 153
 if joy0left then player0x = player0x - 1: if player0x < 1 then player0x = 1
 goto draw_loop
