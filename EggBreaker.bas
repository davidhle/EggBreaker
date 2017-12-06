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
 XX0XX0XX0XX0XX0XX0XX0XX0XX0XX0XX
 XX0XX0XX0XX0XX0XX0XX0XX0XX0XX0XX
 XX0XX0XX0XX0XX0XX0XX0XX0XX0XX0XX
 XX0XX0XX0XX0XX0XX0XX0XX0XX0XX0XX
 ................................
 ................................
  ................................
end

 rem defining the player
 player0:
  %1111111
  %1111111
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

 dim xDirection = 1
 dim yDirection = 1

 dim missile0dx = a
 dim missile0dy = b
 dim pixelX = c
 dim pixelY = d

startgame
 player0x = 75
 player0y =88

 player1x = 30
 player1y = 16

 missile0x=80
 missile0y=70
 missile0dx = 0
 missile0dy = 0
 missile0height= 1

 rem displays the screen
draw_loop
 rem color of background
 COLUBK = $9E
 COLUP0 = 14
 COLUP1 = 14

 drawscreen
 if joy0fire && missile0dx = 0 then gosub startball0
 if joy0right then player0x = player0x + 1: if player0x > 153 then player0x = 153: xDirection = 1
 if joy0left then player0x = player0x - 1: if player0x < 1 then player0x = 2: xDirection = -1
 missile0x = missile0x + missile0dx
 missile0y = missile0y + missile0dy
 if missile0y >= 88 then goto startgame
 rem COLLISIONS
 if collision(player0, missile0) then gosub collidep0b0
 if missile0y <=01 then missile0dy=-missile0dy
 if collision(missile0, playfield) then gosub pixelcollide0
 if missile0x <=1 then missile0dx=-missile0dx
 if missile0x >=153 then missile0dx=-missile0dx



 goto draw_loop

pixelcollide0
 pixelX = missile0x%31
 pixelY = missile0y%11
 pfpixel pixelX pixelY off
 if missile0dx < 0 then missile0dx = 1
 if missile0dy = 1 then missile0dy = #-1 else missile0dx = 1
 rem missile0dy = #-missile0dy
return

collidep0b0 
 if missile0dx <= 0 then missile0dx = 1: if missile0dx > 0 then missile0dx = -1
 missile0dy = -1
 return

startball0
 missile0dy = 1
 missile0dx = 0
 return