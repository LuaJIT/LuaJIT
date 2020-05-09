-- To run this file use luajit binary as below
-- ./luajit test.lua

print("*********Addition*******")
print("a=20;b=10;c=a+b;print(c)")
a=20
b=10
c=a+b
print(c)
print("***************")

print("************* Loops ***************")
print("for i=1,10 do print(i) end")
for i=1,10 do print(i) end
print("for i=10,1,-1 do print(i) end")
for i=10,1,-1 do print(i) end

print("************* While Loop ********")
print("x=10;i=1;while i<x do ; x=i*2 ; print(x); i=i+1;end")
x=10
i=1
while i<x do
x=i*2
print(x)
i=i+1
end
print("***************")

print("************ Tables ***********")
print("days = {'sun','mon'};print(days[1]);print(days[2])")
days = { "sun" , "Mon"};
print(days[1])
print(days[2])

print("revDays = {['Sunday']=1,['Monday']=2};x='Monday'")
revDays = {["Sunday"] = 1, ["Monday"] = 2,}
x="Monday"
print(x)
print(revDays[x])

print("Equating 2 tables")
print("a={};a.x=1;a.y=0;b={};b.x=1;b.y=0;c=a")
a = {}; a.x = 1; a.y = 0
b = {}; b.x = 1; b.y = 0
c = a

print(" i=10 ; j='10';k='+10' ; a ={} ; a[i] = 'one value' ; a[j] = 'another value' ; a[k]='yet another value'")
print("print(a[j];print(a[k];print(a[tonumber(j)];print(a[tonumber(k)]")
i = 10; j = "10"; k = "+10"
a = {}
a[i] = "one value"
a[j] = "another value"
a[k] = "yet another value"
print(a[j])            --> another value
print(a[k])            --> yet another value
print(a[tonumber(j)])  --> one value
print(a[tonumber(k)])  --> one value
print("***************")

print("**********Ipairs****")
print("a = {1,2,3,4,5,6} for i , line in ipairs(a) do print(line) end")
a = {1,2,3,4,5,6}
for i , line in ipairs(a) do print(line) end
print("***************")

print("****************Numbers *******************")
print("a=1.000000;a=1.01;a=4.57e-3")
a= 1.00000000
print(a)

a=1.01
print(a)

a=4.57e-3
print(a)
print("***************")

print("********************* Strings *******************")
print("a='one string';b=string.gsub(a,'one','another');print(b);print(a)")
a = "one string"
b = string.gsub(a, "one", "another")
print(b)
print(a)

print("a=10;b=tostring(a);print(b)")
a=10
b=tostring(a)
print(b)
print("***************")

print("*******Escape characters********")
print("one line\nnext line\n\"in quotes\", 'in quotes'")
print('a backslash inside quotes: \'\\\'')
print("a simpler way: '\\'")
print("***************")

print("************** Input - ouput ***************")
print("Please enter a valid integer")
line = io.read()
n = tonumber(line)
if n == nil then error("line .. is not a valid number") else print(n*2) end
print("************************************************")

print("********************* Logical Operators********************")
print("4 and 5 ; nil and 13 ; false and 13 ; 4 or 5 ; false or 5")
print(4 and 5)
print(nil and 13)
print(false and 13)
print(4 or 5)
print(false or 5)

print("not nil ; not false ; not 0 ; not not nil")
print(not nil)
print(not false)
print(not 0)
print(not not nil)
print("********************************************")


print("**********Functions*****************")
print("function twice(x) return 2*x end")
function twice(x)
return 2*x
end 
b=twice(3)
print(b)

print("************Switch case *************")
function switch(operator)
local op = operator;
a = 20 ; b= 10

if op == "+"
then
c=a+b
print("Add Result",c)
elseif op == "-"
then 
c=a-b
print("Sub Result",c)
elseif op == "*"
then
c=a*b
print("Mul Result",c)
elseif op == "/"
then
c=a/b
print("Div Result",c)
else
error(" Invalid operator")
end
end
switch("+")
print("********************************************")

print("****************Math Functions *******")
radianVal = math.rad(math.pi / 2)
io.write("RadianVal=" , radianVal,"\n")
io.write("Sin Value=",string.format("%.1f ", math.sin(radianVal)),"\n")
io.write("Cosine Value=",string.format("%.1f ", math.cos(radianVal)),"\n")
io.write("Tan Value=",string.format("%.1f ", math.tan(radianVal)),"\n")
io.write("Cosh Value=",string.format("%.1f ", math.cosh(radianVal)),"\n")
io.write("Math.deg",math.deg(math.pi),"\n")
io.write("Floor of 10.5055 is ", math.floor(10.5055),"\n")
io.write("Ceil of 10.5055 is ", math.ceil(10.5055),"\n")
io.write("Square root of 16 is ",math.sqrt(16),"\n")
io.write("10 power 2 is ",math.pow(10,2),"\n")
io.write("100 power 0.5 is ",math.pow(100,0.5),"\n")
io.write("Absolute value of -10 is ",math.abs(-10),"\n")

math.randomseed(os.time())
io.write("Random number between 1 and 100 is ",math.random(),"\n")
io.write("Random number between 1 and 100 is ",math.random(1,100),"\n")
io.write("Maximum in the input array is ",math.max(1,100,101,99,999),"\n")
io.write("Minimum in the input array is ",math.min(1,100,101,99,999),"\n")
print("********************************************")

print("****************OS Functions *******")
io.write("The date is ", os.date("%m/%d/%Y"),"\n")
io.write("The date and time is ", os.date(),"\n")
io.write("The OS time is ", os.time(),"\n")
io.write("Lua started before ", os.clock(),"\n")
print("********************************************")

print("****************String Processing Functions *******")
string1 = "Lua";
print("String Upper",string.upper(string1))
print("String Lower",string.lower(string1))
string = "Lua Tutorial"
print("String Indices",string.find(string,"Tutorial"))
print("String Reverse",string.reverse(string))
string1 = "Lua"
string2 = "Tutorial"
number1 = 10
number2 = 20
print(string.format("Basic formatting %s %s",string1,string2))
date = 2; month = 1; year = 2014
print(string.format("Date formatting %02d/%02d/%03d", date, month, year))
print("String to ASCII",string.byte("Lua"))
print("ASCII for 3 character in word",string.byte("Lua",3))
print("ASCII for 1 character from last in word",string.byte("Lua",-1))
print("ASCII for 2 character in word",string.byte("Lua",2))
print("ASCII for 2 character from last in word",string.byte("Lua",-2))
print("ASCII Value to string",string.char(97))

string1 = "Lua"
string2 = "Tutorial"
print("Concatenated string",string1..string2)
print("Length of string1 is ",string.len(string1))
print("Repeated String",string.rep(string1,3))
print("********************************************")

print("****************Table Functions *******")
fruits = {"banana","orange","apple"}
print("Table contents are ")
for key,value in ipairs(fruits) do print(value) end
print("Concatenated string ",table.concat(fruits))
print("Concatenated string ",table.concat(fruits,", "))
print("Concatenated string ",table.concat(fruits,", ", 2,3))
print("Inserting new fruit mango")
table.insert(fruits,"mango")
for key,value in ipairs(fruits) do print(value) end
print("Concatenated string ",table.concat(fruits,", "))
print("The maximum elements in table is",table.maxn(fruits))
print("Table contents after sorting are ",table.sort(fruits))
for key,value in ipairs(fruits) do print(value) end
print("Removing the last element from the table:" ,table.remove(fruits))
print("Table contents are ")
for key,value in ipairs(fruits) do print(value) end
print("********************************************")
