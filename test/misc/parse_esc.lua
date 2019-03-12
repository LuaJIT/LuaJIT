assert("\79\126" == "O~")
assert("\x4f\x7e" == "O~")
assert(loadstring[[return "\xxx"]] == nil)
assert(loadstring[[return "\xxx"]] == nil)
assert(assert(loadstring[[return "abc   \z

   def"]])() == "abc   def")
