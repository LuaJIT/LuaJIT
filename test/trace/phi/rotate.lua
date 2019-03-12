do --- rot8
  local function rot8r(n)
    local a,b,c,d,e,f,g,h=1,2,3,4,5,6,7,8
    for x=1,n do
      a,b,c,d,e,f,g,h=h,a,b,c,d,e,f,g
    end
    return table.concat{a,b,c,d,e,f,g,h}
  end

  local function rot8l(n)
    local a,b,c,d,e,f,g,h=1,2,3,4,5,6,7,8
    for x=1,n do
      a,b,c,d,e,f,g,h=b,c,d,e,f,g,h,a
    end
    return table.concat{a,b,c,d,e,f,g,h}
  end

  assert(rot8r(0) == "12345678")
  assert(rot8r(10) == "78123456")
  assert(rot8r(105) == "81234567")
  assert(rot8r(0) == "12345678")
  assert(rot8r(1) == "81234567")
  assert(rot8r(2) == "78123456")
  assert(rot8r(0) == "12345678")
  assert(rot8r(1) == "81234567")
  assert(rot8r(2) == "78123456")
  assert(rot8r(105) == "81234567")

  assert(rot8l(0) == "12345678")
  assert(rot8l(10) == "34567812")
  assert(rot8l(105) == "23456781")
  assert(rot8l(0) == "12345678")
  assert(rot8l(1) == "23456781")
  assert(rot8l(2) == "34567812")
  assert(rot8l(0) == "12345678")
  assert(rot8l(1) == "23456781")
  assert(rot8l(2) == "34567812")

  assert(rot8r(100) == "56781234")
  assert(rot8l(100) == "56781234")
end

do --- rot9
  local function rot9r(n)
    local a,b,c,d,e,f,g,h,i=1,2,3,4,5,6,7,8,9
    for x=1,n do
      a,b,c,d,e,f,g,h,i=i,a,b,c,d,e,f,g,h
    end
    return table.concat{a,b,c,d,e,f,g,h,i}
  end

  local function rot9l(n)
    local a,b,c,d,e,f,g,h,i=1,2,3,4,5,6,7,8,9
    for x=1,n do
      a,b,c,d,e,f,g,h,i=b,c,d,e,f,g,h,i,a
    end
    return table.concat{a,b,c,d,e,f,g,h,i}
  end

  assert(rot9r(0) == "123456789")
  assert(rot9r(10) == "912345678")
  assert(rot9r(105) == "456789123")
  assert(rot9r(0) == "123456789")
  assert(rot9r(1) == "912345678")
  assert(rot9r(2) == "891234567")
  assert(rot9r(0) == "123456789")
  assert(rot9r(1) == "912345678")
  assert(rot9r(2) == "891234567")
  assert(rot9r(105) == "456789123")

  assert(rot9l(0) == "123456789")
  assert(rot9l(10) == "234567891")
  assert(rot9l(105) == "789123456")
  assert(rot9l(0) == "123456789")
  assert(rot9l(1) == "234567891")
  assert(rot9l(2) == "345678912")
  assert(rot9l(0) == "123456789")
  assert(rot9l(1) == "234567891")
  assert(rot9l(2) == "345678912")

  assert(rot9r(100) == "912345678")
  assert(rot9l(100) == "234567891")
end

do --- rot18
  local function rot18r(N)
    local a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18
    for x=1,N do
      a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r=r,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q
    end
    return table.concat{a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r}
  end

  local function rot18l(N)
    local a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18
    for x=1,N do
      a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r=b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,a
    end
    return table.concat{a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r}
  end

  assert(rot18r(0)	== "123456789101112131415161718")
  assert(rot18r(10)	== "910111213141516171812345678")
  assert(rot18r(105)	== "456789101112131415161718123")
  assert(rot18r(0)	== "123456789101112131415161718")
  assert(rot18r(1)	== "181234567891011121314151617")
  assert(rot18r(2)	== "171812345678910111213141516")
  assert(rot18r(0)	== "123456789101112131415161718")
  assert(rot18r(1)	== "181234567891011121314151617")
  assert(rot18r(2)	== "171812345678910111213141516")
  assert(rot18r(105)	== "456789101112131415161718123")

  assert(rot18l(0)	== "123456789101112131415161718")
  assert(rot18l(10)	== "111213141516171812345678910")
  assert(rot18l(105)	== "161718123456789101112131415")
  assert(rot18l(0)	== "123456789101112131415161718")
  assert(rot18l(1)	== "234567891011121314151617181")
  assert(rot18l(2)	== "345678910111213141516171812")
  assert(rot18l(0)	== "123456789101112131415161718")
  assert(rot18l(1)	== "234567891011121314151617181")
  assert(rot18l(2)	== "345678910111213141516171812")

  assert(rot18r(100)	== "910111213141516171812345678")
  assert(rot18l(100)	== "111213141516171812345678910")
end

do --- rotx
  local function rot9r(n, m)
    local a,b,c,d,e,f,g,h,i=1,2,3,4,5,6,7,8,9
    local s = ""
    for x=1,n do
      a,b,c,d,e,f,g,h,i=i,a,b,c,d,e,f,g,h
      if x == m then s = table.concat{a,b,c,d,e,f,g,h,i} end
      c,d = d,c
    end
    return table.concat{a,b,c,d,e,f,g,h,i, s}
  end

  assert(rot9r(0,0) == "123456789")
  assert(rot9r(10,0) == "893124567")
  assert(rot9r(105,0) == "913245678")
  assert(rot9r(105,90) == "913245678891324567")
  assert(rot9r(0,0) == "123456789")
  assert(rot9r(1,0) == "913245678")
  assert(rot9r(2,0) == "893124567")
  assert(rot9r(1,1) == "913245678912345678")
  assert(rot9r(2,1) == "893124567912345678")
  assert(rot9r(2,2) == "893124567891324567")
end
