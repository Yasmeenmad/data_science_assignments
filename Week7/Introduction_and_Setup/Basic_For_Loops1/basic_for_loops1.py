# task1 -- Basic: Print all integers from 0 to 150
for i in range(0, 151):
    print(i)

# task2 -- Multiples of Five: Print all the multiples of 5 from 5 to 1,000
for i in range(5, 1001):
    print(i*5)
    
# task3 -- Counting, the Dojo Way:  Print integers 1 to 100. If divisible by 5, print "Coding" instead. If divisible by 10, print "Coding Dojo".
number = 1
while number <= 100:
    if number%10 == 0:
        print("Coding Dojo")
        number = number+1
    elif number%5 == 0:
        print("Coding")
        number = number+1
    else:
        print(number)
        number = number+1
        
# task4 -- Whoa. That Sucker's Huge: Add odd integers from 0 to 500,000, and print the final sum.
sum_odd_integers = 0
for number in range(0, 500000):
    if(number % 2 != 0):
        
        sum_odd_integers = sum_odd_integers + number
print("The Sum of Odd Integers from 0 to 500,000 is :", sum_odd_integers) 

# task5 -- Countdown by Fours: Print positive numbers starting at 2018, counting down by fours.
for number in range(2018, 0, -4):
    print(number)

# optional task -- Flexible Counter: Set three variables: lowNum, highNum, mult. Starting at lowNum and going through highNum, print only the integers that are a multiple of mult.
lowNum  = int(input("Enter the minimum value:"))
highNum = int(input("Enter the maximum value:"))
mult    = int(input("Enter the number that you want to multiply by:"))

for i in range(lowNum, highNum +1):
    if i%mult == 0:
        print(i)