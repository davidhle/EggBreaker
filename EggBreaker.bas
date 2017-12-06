 rem **********************************
 rem Egg Breaker
 rem 
 rem Humpty Dumpty has a neighbor he doesn't like 
 rem who also lives on the wall with him.
 rem There's a wall between them like fences for human 
 rem  neighbors. Get your ball past them to win a point
 rem  and have your ball hit the other egg to win 10 points
 rem 
 rem LMC 2700
 rem Project 6
 rem Lilliann Andrews, Sranee Bayapureddy, David Le
 rem **********************************
 include div_mul.asm
 rem include fixed_point_math.asm
 set legacy
 set romsize 4k
 pfclear
 player0x=2:player0y=52
 player0:
 %00111100
 %01111110
 %01111110
 %11111111
 %11100111
 %11111111
 %11011011
 %01111110
 %00111100
 %00011000
end
 player1x=158:player1y=52
 player1:
 %00111100
 %01111110
 %01111110
 %11111111
 %11100111
 %11111111
 %11011011
 %01111110
 %00111100
 %00011000
end

 scorecolor = 60
 dim missile0dx = a
 dim missile0dy = b
 dim missile1dx = c
 dim missile1dy = d
 dim tempx = e
 dim tempy = f
 dim musicPointer = g
 dim musicTimer = h
 dim tempaudv = i
 dim musicDist = j
 dim soundTimer = k
 dim randCountDown = l
 randCountDown = 30
 musicPointer=$FF
 musicTimer=0
 AUDV0=0
 AUDC0=4
 AUDV1=0
 AUDC1=14

startgame
 player0x=2:player0y=52
 player1x=158:player1y=52
 missile0x=40
 missile0y=44
 missile0dx = 0
 missile0dy = 0
 missile1x=150
 missile1y=44
 missile1dx = 0
 missile1dy = 0
  
drawborders
 pfvline 14 0 11 on
 pfvline 15 0 11 on
 pfvline 16 0 11 on


gameloop
 COLUP0 = 14
 COLUP1 = 14
 COLUPF = 64
 COLUBK = 70
 drawscreen
 if joy0fire && missile0dx = 0 then gosub startball0
 if joy0up then player0y = player0y - 2: if player0y < 16 then player0y = 16
 if joy0down then player0y = player0y + 2: if player0y > 88 then player0y = 88
 if joy1fire && missile1dx = 0 then gosub startball1
 if joy1up then player1y = player1y - 2: if player1y < 16 then player1y = 16
 if joy1down then player1y = player1y + 2: if player1y > 88 then player1y = 88
 missile0x = missile0x + missile0dx
 missile1x = missile1x + missile1dx
 missile0y = missile0y + missile0dy
 missile1y = missile1y + missile1dy
 rem PADDLE COLLISIONS
 if collision(player1, missile0) then soundTimer = 16
 if collision(player1, missile0) then gosub collidep1b0
 if collision(player0, missile0) then soundTimer = 16
 if collision(player0, missile0) then gosub collidep0b0
 if collision(player1, missile1) then soundTimer = 16
 if collision(player1, missile1) then gosub collidep1b1
 if collision(player0, missile1) then soundTimer = 16
 if collision(player0, missile1) then gosub collidep0b1
 rem VERTICAL BORDER COLLISION
 if missile0y <= 1 then missile0dy=#-missile0dy
 if missile0y <= 1 then missile0y=1
 if missile1y <= 1 then missile1dy=#-missile1dy
 if missile1y <= 1 then missile1y=1
 if missile0y >= 88 then missile0dy= #-missile0dy
 if missile0y >= 88 then missile0y=88
 if missile1y >= 88 then missile1dy= #-missile1dy
 if missile1y >= 88 then missile1y=88
 rem HORIZONTAL BORDER COLLISION
 if missile0x <= 16 then goto player2win
 if missile0x >= 175 then goto player1win
 if missile1x <= 16 then goto player2win
 if missile1x >= 175 then goto player1win
 rem BLOCK COLLISION
 
 if collision(missile0, playfield) then gosub pixelcollide0
 if collision(missile1, playfield) then gosub pixelcollide1
 
 rem MUSIC
 if musicTimer <=1 then gosub changeMusicNote
 musicTimer = musicTimer - 1
 if soundTimer > 0 then AUDF1 = 26
 if soundTimer > 0 then AUDV1 = 5
 if soundTimer >= 1 then soundTimer = soundTimer - 1 else AUDV1 = 0
 goto gameloop

 return
 
pixelcollide0
 tempy=(missile0y)/8
 tempx = missile0x
 if tempx <= 86 then tempx=(missile0x)/5 - 4
 if tempx < 96 && tempx > 86 then tempx=(missile0x)/5 - 3
 if tempx >= 96 && tempx <100 then tempx=(missile0x)/5 - 3
 if tempx >= 100 then tempx=(missile0x)/5 - 2
 pfpixel tempx tempy off
 rem if missile0dx = #-1 then missile0dx = 1
 if missile0dx = 1 then missile0dx = #-1 else missile0dx = 1
 rem missile0dy = #-missile0dy
 return
pixelcollide1
 tempy=(missile1y)/8
 tempx = missile1x
 if tempx <= 86 then tempx=(missile1x)/5 - 4
 if tempx < 96 && tempx > 86 then tempx=(missile1x)/5 - 3
 if tempx >= 96 && tempx <100 then tempx=(missile1x)/5 - 3
 if tempx >= 100 then tempx=(missile1x)/5 - 2
 pfpixel tempx tempy off
 rem if missile1dx = #-1 then missile1dx = 1
 if missile1dx = 1 then missile1dx = #-1 else missile1dx = 1
 rem missile1dy = #-missile1dy
 return

collidep0b0
 z = player0y - missile0y
 z = z/4
 if z >= 2 then missile0dy = #-1
 if z <= 1 then missile0dy = 1
 missile0dx = 1
 missile0x = missile0x + missile0dx
 missile0y = missile0y + missile0dy
 return
 
collidep1b0
 z = player1y - missile0y
 z = z/4
 if z >= 2 then missile0dy = #-1
 if z <= 1 then missile0dy = 1
 missile0dx = #-1
 missile0x = missile0x + missile0dx
 missile0y = missile0y + missile0dy
 score = score + 10000
 return
 
collidep0b1
 z = player0y - missile1y
 z = z/4

 if z >= 2 then missile1dy = #-1
 if z <= 1 then missile1dy = 1
 missile1dx = 1
 missile1x = missile1x + missile1dx
 missile1y = missile1y + missile1dy
 score = score + 10
 return
 
collidep1b1
 z = player1y - missile1y
 z = z/4

 if z >= 2 then missile1dy = #-1
 if z <= 1 then missile1dy = 1
 missile1dx = #-1
 missile1x = missile1x + missile1dx
 missile1y = missile1y + missile1dy
 return
 
startball0
 missile0dx = #-1
 return

startball1
 missile1dx = 1
 return

playsound
 AUDV1 = 8
 soundTimer = 10
 return
 
player1win
 score = score + 1000
 goto startgame

player2win
 score = score + 1
 goto startgame
 
changeMusicNote
 musicPointer = musicPointer + 1


 musicPointer = musicPointer + 1
 rem value is (2 * #_OF_NOTES) - 1
 if musicPointer > 29 then musicPointer = #-1
 return
