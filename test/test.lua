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


