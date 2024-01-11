## Project3-- Treasure Island 
print("Welcome to treasure island. Your mission is to find the treasure.")

## Get started

left_right = input("Do you wanna go left or right? ").upper()
print(left_right)

if left_right not in ["LEFT", "RIGHT"]:
  print("Please choose either left or right")
elif left_right == 'RIGHT':
  print("Game over!!")
else: 
  swim_wait = input("Do you wanna swim or wait: ").upper()
  if swim_wait not in ["SWIM", "WAIT"]:
    print("Please choose either wait or swim")
  elif swim_wait == "SWIM":
    print("Game over!!")
  else:
    color = input("Choose a color (Red, Blue or Yellow): ").upper()
    if color not in ["BLUE", "RED", "YELLOW"]:
      print("Please choose one of red, blue, and yellow")
    elif color == "BLUE" or color == "RED":
      print("Game over!!")
    else:
      print("You won!! Congrats!!")