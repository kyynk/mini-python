# a = 6
# print(a)
# ab = -a
# print(a)
# b = True
# print(b)
# c = not False
# print(c)
# ppp = list(range(2))
# print(ppp)
# d = "hello"
# print(d)
# dd = " world"
# print(d+dd)
# e = [1,[2,["hrt",[True,[-5]]]]]
# print(e)

# ee = e+e
# print(ee)
# f = None
# print(f)
# g_arg = True
# g = [[2, not g_arg, 4]]
# print(g)
# hh = 2
# i = [1,[2,["hrt",[True,[-5]]]]]
# print(i[1][0])
# print(i+i)
# # # print(1 < 2 and len(1))

# print([] + [])
# print([1,2] + [3,4,5])

# h = [-1, 2, ["asd", True], "dasd"]
# print(len(h))

# print(h)
# x = 43
# aaaa = 3
# for x in h:
#     print(x)
# print(x)
# print(4+1)
# a = "asd"
# b = "adgg"
# print(a+b)
# print(a)
# print(b)

# print(2<4)
# print(9>2)
# print(1>=2)
# print(6<=3)
# print(3==2)

# print(3!=3)
# print([1] == [1])
# print([1] == [2])
# print(1==1)
# print(1==2)

# print(True == True)
# print(False == False)
# print([1,4] == [1,1,4])
# print("a" == "a")
# print("a" == "b")

# print("a" != "A")
# print("a" != "aa")
# print("a" != "ab")
# print("a" != None)
# print("a" != "a")

# print(1<2)
# print(3<1)
# print(1<1)
# print(2<2)
# print("\n")
# print("bbb"<"bb")
# print("aa"<"aaa")
# print("aa"<"aa")
# print("\n")
# print([1,1,2]<[1])
# print([1,1,2]<[3])
# print([1]<[1,1,2])
# print([1,1,2]<[1,1,2])

# print(1<=2)
# print(3<=1)
# print(1<=1)
# print(2<=2)
# print("\n")
# print("bbb"<="bb")
# print("aa"<="aaa")
# print("aa"<="aa")
# print("\n")
# print([1,1,2]<=[1])
# print([1,1,2]<=[3])
# print([1]<=[1,1,2])
# print([1,1,2]<=[1,1,2])

# print(1>2)
# print(3>1)
# print(1>1)
# print(2>2)
# print("\n")
# print("bbb">"bb")
# print("aa">"aaa")
# print("aa">"aa")
# print("\n")
# print([1,1,2]>[1])
# print([1,1,2]>[3])
# print([1]>[1,1,2])
# print([1,1,2]>[1,1,2])

# print(1>=2)
# print(3>=1)
# print(1>=1)
# print(2>=2)
# print("\n")
# print("bbb">="bb")
# print("aa">="aaa")
# print("aa">="aa")
# print("\n")
# print([1,1,2]>=[1])
# print([1,1,2]>=[3])
# print([1]>=[1,1,2])
# print([1,1,2]>=[1,1,2])

# row = [1, 2, 3]
# badmatrix = [row, row, row]
# print(badmatrix)
# badmatrix[1][1] = 42
# print(badmatrix)
# i = [1,[2,["hrt",[True,[-5]]]]]
# i[1] = 3
# print(i)

# print([1, [2,3], 4] != [1, 2, 3])
# print([1, [2,3], 4] == [1, [2,3], 3])
# print([1, [2,3], 4] == [1, [2,3], 4])
# x = 1
# print([x,x,x] == [x,[x],x])

# def f(t, a, b):
#     l = [1, 2, 3, t, a, b]
#     return l
# print(f(4, 5, 6))
# note
# compile_def

# l = [4,5,6,5]
# for x in l:
#     l = []
#     print(x)
#     a = 12
# print(l)
# def f(i, j):
#     if i >= j:
#         return []
#     return [i] + f(i+1, j)

# # print([] + [])
# print([1,2] + [3,4,5])
# print(f(4, 7))

# print([1] + [])


### IMPORTANT NOTE ###
### This is the only test for which the expected output
### is *not* the same as the one obtained with Python.
### The difference comes from arithmetic: Mini Python uses
### machine arithmetic for division and module, whereas Python uses
### something else (where the modulus has the sign of the *second* operand).

# fixed-point arithmetic
# precision q = 8192 i.e. 13 bits for the decimal part

# def add(x, y):
#     return x + y
# def sub(x, y):
#     return x - y
# def mul(x, y):
#     t = x * y
#     return (t + 8192 // 2) // 8192
# def div(x, y):
#     t = x * 8192
#     return (t + y // 2) // y
# def of_int(x):
#     return x * 8192

# def iter(n, a, b, xn, yn):
#     if n == 100: return 1
#     xn2 = mul(xn, xn)
#     yn2 = mul(yn, yn)
#     if add(xn2, yn2) > of_int(4): return 0
#     return iter(n+1, a, b, add(sub(xn2, yn2), a), add(mul(of_int(2), mul(xn, yn)), b))

# def inside(x, y):
#     return iter(0, x, y, of_int(0), of_int(0))

# def main():
#     xmin = of_int(-2)
#     xmax = of_int(1)
#     steps = 40
#     deltax = div(sub(xmax, xmin), of_int(2 * steps))
#     ymin = of_int(-1)
#     ymax = of_int(1)
#     deltay = div(sub(ymax, ymin), of_int(steps))
#     for i in list(range(steps)):
#         y = add(ymin, mul(of_int(i), deltay))
#         s = ""
#         for j in list(range(2 * steps)):
#             x = add(xmin, mul(of_int(j), deltax))
#             if inside(x, y): s = s + "0"
#             else: s = s + "1"
#    mini-python-ocaml/test.s     print(s)

# main()
s = "4"

# for i in list(range(3)):
s = s + "1"
s = s + "2"
print(s)