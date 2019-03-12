
local co = coroutine
local create, resume, yield = co.create, co.resume, co.yield

local N = tonumber(arg and arg[1]) or 10
local first, second

-- Meet another creature.
local function meet(me)
  while second do yield() end -- Wait until meeting place clears.
  local other = first
  if other then -- Hey, I found a new friend!
    first = nil
    second = me
  else -- Sniff, nobody here (yet).
    local n = N - 1
    if n < 0 then return end -- Uh oh, the mall is closed.
    N = n
    first = me
    repeat yield(); other = second until other -- Wait for another creature.
    second = nil
    yield() -- Be nice and let others meet up.
  end
  return other
end

-- Create a very social creature.
local function creature(color)
  return create(function()
    local me = color
    for met=0,1000000000 do
      local other = meet(me)
      if not other then return met end
      if me ~= other then
        if me == "blue" then me = other == "red" and "yellow" or "red"
        elseif me == "red" then me = other == "blue" and "yellow" or "blue"
        else me = other == "blue" and "red" or "blue" end
      end
    end
  end)
end

-- Trivial round-robin scheduler.
local function schedule(threads)
  local resume = resume
  local nthreads, meetings = #threads, 0
  repeat
    for i=1,nthreads do
      local thr = threads[i]
      if not thr then return meetings end
      local ok, met = resume(thr)
      if met then
        meetings = meetings + met
        threads[i] = nil
      end
    end
  until false
end

-- A bunch of colorful creatures.
local threads = {
  creature("blue"),
  creature("red"),
  creature("yellow"),
  creature("blue"),
}

io.write(schedule(threads), "\n")
