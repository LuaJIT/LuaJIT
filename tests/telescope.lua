--- Telescope is a test library for Lua that allows for flexible, declarative
-- tests. The documentation produced here is intended largely for developers
-- working on Telescope.  For information on using Telescope, please visit the
-- project homepage at: <a href="http://github.com/norman/telescope">http://github.com/norman/telescope#readme</a>.
-- @release 0.6
-- @class module
-- @module 'telescope'
local _M = {}
local getfenv = _G.getfenv
local setfenv = _G.setfenv


local _VERSION = "0.6.0"

--- The status codes that can be returned by an invoked test. These should not be overidden.
-- @name status_codes
-- @class table
-- @field err - This is returned when an invoked test results in an error
-- rather than a passed or failed assertion.
-- @field fail - This is returned when an invoked test contains one or more failing assertions.
-- @field pass - This is returned when all of a test's assertions pass.
-- @field pending - This is returned when a test does not have a corresponding function.
-- @field unassertive - This is returned when an invoked test does not produce
-- errors, but does not contain any assertions.
local status_codes = {
  err         = 2,
  fail        = 4,
  pass        = 8,
  pending     = 16,
  unassertive = 32
}

--- Labels used to show the various <tt>status_codes</tt> as a single character.
-- These can be overidden if you wish.
-- @name status_labels
-- @class table
-- @see status_codes
-- @field status_codes.err         'E'
-- @field status_codes.fail        'F'
-- @field status_codes.pass        'P'
-- @field status_codes.pending     '?'
-- @field status_codes.unassertive 'U'

local status_labels = {
  [status_codes.err]         = 'E',
  [status_codes.fail]        = 'F',
  [status_codes.pass]        = 'P',
  [status_codes.pending]     = '?',
  [status_codes.unassertive] = 'U'
}

--- The default names for context blocks. It defaults to "context", "spec" and
-- "describe."
-- @name context_aliases
-- @class table
local context_aliases = {"context", "describe", "spec"}
--- The default names for test blocks. It defaults to "test," "it", "expect",
-- "they" and "should."
-- @name test_aliases
-- @class table
local test_aliases    = {"test", "it", "expect", "should", "they"}

--- The default names for "before" blocks. It defaults to "before" and "setup."
-- The function in the before block will be run before each sibling test function
-- or context.
-- @name before_aliases
-- @class table
local before_aliases  = {"before", "setup"}

--- The default names for "after" blocks. It defaults to "after" and "teardown."
-- The function in the after block will be run after each sibling test function
-- or context.
-- @name after_aliases
-- @class table
local after_aliases  = {"after", "teardown"}

-- Prefix to place before all assertion messages. Used by make_assertion().
local assertion_message_prefix  = "Assert failed: expected "

--- The default assertions.
-- These are the assertions built into telescope. You can override them or
-- create your own custom assertions using <tt>make_assertion</tt>.
-- <ul>
-- <tt><li>assert_blank(a)</tt> - true if a is nil, or the empty string</li>
-- <tt><li>assert_empty(a)</tt> - true if a is an empty table</li>
-- <tt><li>assert_equal(a, b)</tt> - true if a == b</li>
-- <tt><li>assert_error(f)</tt> - true if function f produces an error</li>
-- <tt><li>assert_false(a)</tt> - true if a is false</li>
-- <tt><li>assert_greater_than(a, b)</tt> - true if a > b</li>
-- <tt><li>assert_gte(a, b)</tt> - true if a >= b</li>
-- <tt><li>assert_less_than(a, b)</tt> - true if a < b</li>
-- <tt><li>assert_lte(a, b)</tt> - true if a <= b</li>
-- <tt><li>assert_match(a, b)</tt> - true if b is a string that matches pattern a</li>
-- <tt><li>assert_nil(a)</tt> - true if a is nil</li>
-- <tt><li>assert_true(a)</tt> - true if a is true</li>
-- <tt><li>assert_type(a, b)</tt> - true if a is of type b</li>
-- <tt><li>assert_not_blank(a)</tt>  - true if a is not nil and a is not the empty string</li>
-- <tt><li>assert_not_empty(a)</tt> - true if a is a table, and a is not empty</li>
-- <tt><li>assert_not_equal(a, b)</tt> - true if a ~= b</li>
-- <tt><li>assert_not_error(f)</tt> - true if function f does not produce an error</li>
-- <tt><li>assert_not_false(a)</tt> - true if a is not false</li>
-- <tt><li>assert_not_greater_than(a, b)</tt> - true if not (a > b)</li>
-- <tt><li>assert_not_gte(a, b)</tt> - true if not (a >= b)</li>
-- <tt><li>assert_not_less_than(a, b)</tt> - true if not (a < b)</li>
-- <tt><li>assert_not_lte(a, b)</tt> - true if not (a <= b)</li>
-- <tt><li>assert_not_match(a, b)</tt> - true if the string b does not match the pattern a</li>
-- <tt><li>assert_not_nil(a)</tt> - true if a is not nil</li>
-- <tt><li>assert_not_true(a)</tt> - true if a is not true</li>
-- <tt><li>assert_not_type(a, b)</tt> - true if a is not of type b</li>
-- </ul>
-- @see make_assertion
-- @name assertions
-- @class table
local assertions = {}

--- Create a custom assertion.
-- This creates an assertion along with a corresponding negative assertion. It
-- is used internally by telescope to create the default assertions.
-- @param name The base name of the assertion.
-- <p>
-- The name will be used as the basis of the positive and negative assertions;
-- i.e., the name <tt>equal</tt> would be used to create the assertions
-- <tt>assert_equal</tt> and <tt>assert_not_equal</tt>.
-- </p>
-- @param message The base message that will be shown.
-- <p>
-- The assertion message is what is shown when the assertion fails.  It will be
-- prefixed with the string in <tt>telescope.assertion_message_prefix</tt>.
-- The variables passed to <tt>telescope.make_assertion</tt> are interpolated
-- in the message string using <tt>string.format</tt>.  When creating the
-- inverse assertion, the message is reused, with <tt>" to be "</tt> replaced
-- by <tt>" not to be "</tt>. Hence a recommended format is something like:
-- <tt>"%s to be similar to %s"</tt>.
-- </p>
-- @param func The assertion function itself.
-- <p>
-- The assertion function can have any number of arguments.
-- </p>
-- @usage <tt>make_assertion("equal", "%s to be equal to %s", function(a, b)
-- return a == b end)</tt>
-- @function make_assertion
local function make_assertion(name, message, func)
  local num_vars = 0
  -- if the last vararg ends up nil, we'll need to pad the table with nils so
  -- that string.format gets the number of args it expects
  local format_message
  if type(message) == "function" then
    format_message = message
  else
    for _, _ in message:gmatch("%%s") do num_vars = num_vars + 1 end
    format_message = function(message, ...)
      local a = {}
      local args = {...}
      local nargs = select('#', ...)
      if nargs > num_vars then        
        local userErrorMessage = args[num_vars+1]
        if type(userErrorMessage) == "string" then
          return(assertion_message_prefix .. userErrorMessage)
        else
          error(string.format('assert_%s expected %d arguments but got %d', name, num_vars, #args))
        end
      end
      for i = 1, nargs do a[i] = tostring(args[i]) end
      for i = nargs+1, num_vars do a[i] = 'nil' end
      return (assertion_message_prefix .. message):format(unpack(a))
    end
  end

  assertions["assert_" .. name] = function(...)
    if assertion_callback then assertion_callback(...) end
    if not func(...) then
      error({format_message(message, ...), debug.traceback()})
    end
  end
end

--- (local) Return a table with table t's values as keys and keys as values.
-- @param t The table.
local function invert_table(t)
  local t2 = {}
  for k, v in pairs(t) do t2[v] = k end
  return t2
end

-- (local) Truncate a string "s" to length "len", optionally followed by the
-- string given in "after" if truncated; for example, truncate_string("hello
-- world", 3, "...")
-- @param s The string to truncate.
-- @param len The desired length.
-- @param after A string to append to s, if it is truncated.
local function truncate_string(s, len, after)
  if #s <= len then
    return s
  else
    local s = s:sub(1, len):gsub("%s*$", '')
    if after then return s .. after else return s end
  end
end

--- (local) Filter a table's values by function. This function iterates over a
-- table , returning only the table entries that, when passed into function f,
-- yield a truthy value.
-- @param t The table over which to iterate.
-- @param f The filter function.
local function filter(t, f)
  local a, b
  return function()
    repeat a, b = next(t, a)
      if not b then return end
      if f(a, b) then return a, b end
    until not b
  end
end

--- (local) Finds the value in the contexts table indexed with i, and returns a table
-- of i's ancestor contexts.
-- @param i The index in the <tt>contexts</tt> table to get ancestors for.
-- @param contexts The table in which to find the ancestors.
local function ancestors(i, contexts)
  if i == 0 then return end
  local a = {}
  local function func(j)
    if contexts[j].parent == 0 then return nil end
    table.insert(a, contexts[j].parent)
    func(contexts[j].parent)
  end
  func(i)
  return a
end

make_assertion("blank",        "'%s' to be blank",                         function(a) return a == '' or a == nil end)
make_assertion("empty",        "'%s' to be an empty table",                function(a) return not next(a) end)
make_assertion("equal",        "'%s' to be equal to '%s'",                 function(a, b) return a == b end)
make_assertion("error",        "result to be an error",                    function(f) return not pcall(f) end)
make_assertion("false",        "'%s' to be false",                         function(a) return a == false end)
make_assertion("greater_than", "'%s' to be greater than '%s'",             function(a, b) return a > b end)
make_assertion("gte",          "'%s' to be greater than or equal to '%s'", function(a, b) return a >= b end)
make_assertion("less_than",    "'%s' to be less than '%s'",                function(a, b) return a < b end)
make_assertion("lte",          "'%s' to be less than or equal to '%s'",    function(a, b) return a <= b end)
make_assertion("match",        "'%s' to be a match for %s",                function(a, b) return (tostring(b)):match(a) end)
make_assertion("nil",          "'%s' to be nil",                           function(a) return a == nil end)
make_assertion("true",         "'%s' to be true",                          function(a) return a == true end)
make_assertion("type",         "'%s' to be a %s",                          function(a, b) return type(a) == b end)

make_assertion("not_blank",    "'%s' not to be blank",                     function(a) return a ~= '' and a ~= nil end)
make_assertion("not_empty",    "'%s' not to be an empty table",            function(a) return not not next(a) end)
make_assertion("not_equal",    "'%s' not to be equal to '%s'",             function(a, b) return a ~= b end)
make_assertion("not_error",    "result not to be an error",                function(f) return not not pcall(f) end)
make_assertion("not_match",    "'%s' not to be a match for %s",            function(a, b) return not (tostring(b)):match(a) end)
make_assertion("not_nil",      "'%s' not to be nil",                       function(a) return a ~= nil end)
make_assertion("not_type",     "'%s' not to be a %s",                      function(a, b) return type(a) ~= b end)

--- Build a contexts table from the test file or function given in <tt>target</tt>.
-- If the optional <tt>contexts</tt> table argument is provided, then the
-- resulting contexts will be added to it.
-- <p>
-- The resulting contexts table's structure is as follows:
-- </p>
-- <code>
-- {
--   {parent = 0, name = "this is a context", context = true},
--   {parent = 1, name = "this is a nested context", context = true},
--   {parent = 2, name = "this is a test", test = function},
--   {parent = 2, name = "this is another test", test = function},
--   {parent = 0, name = "this is test outside any context", test = function},
-- }
-- </code>
-- @param contexts A optional table in which to collect the resulting contexts
-- and function.
-- @function load_contexts
local function load_contexts(target, contexts)
  local env = {}
  local current_index = 0
  local context_table = contexts or {}

  local function context_block(name, func)
    table.insert(context_table, {parent = current_index, name = name, context = true})
    local previous_index = current_index
    current_index = #context_table
    func()
    current_index = previous_index
  end

  local function test_block(name, func)
    local test_table = {name = name, parent = current_index, test = func or true}
    if current_index ~= 0 then
      test_table.context_name = context_table[current_index].name
    else
      test_table.context_name = 'top level'
    end
    table.insert(context_table, test_table)
  end

  local function before_block(func)
    context_table[current_index].before = func
  end

  local function after_block(func)
    context_table[current_index].after = func
  end

  for _, v in ipairs(after_aliases)   do env[v] = after_block end
  for _, v in ipairs(before_aliases)  do env[v] = before_block end
  for _, v in ipairs(context_aliases) do env[v] = context_block end
  for _, v in ipairs(test_aliases)    do env[v] = test_block end

  -- Set these functions in the module's meta table to allow accessing
  -- telescope's test and context functions without env tricks. This will
  -- however add tests to a context table used inside the module, so multiple
  -- test files will add tests to the same top-level context, which may or may
  -- not be desired.
  setmetatable(_M, {__index = env})

  setmetatable(env, {__index = _G})

  local func, err = type(target) == 'string' and assert(loadfile(target)) or target
  if err then error(err) end
  setfenv(func, env)()
  return context_table
end

-- in-place table reverse.
function table.reverse(t)
     local len = #t+1
     for i=1, (len-1)/2 do
          t[i], t[len-i] = t[len-i], t[i]
     end
end

--- Run all tests.
-- This function will exectute each function in the contexts table.
-- @param contexts The contexts created by <tt>load_contexts</tt>.
-- @param callbacks A table of callback functions to be invoked before or after
-- various test states.
-- <p>
-- There is a callback for each test <tt>status_code</tt>, and callbacks to run
-- before or after each test invocation regardless of outcome.
-- </p>
-- <ul>
-- <li>after - will be invoked after each test</li>
-- <li>before - will be invoked before each test</li>
-- <li>err - will be invoked after each test which results in an error</li>
-- <li>fail - will be invoked after each failing test</li>
-- <li>pass - will be invoked after each passing test</li>
-- <li>pending - will be invoked after each pending test</li>
-- <li>unassertive - will be invoked after each test which doesn't assert
-- anything</li>
-- </ul>
-- <p>
-- Callbacks can be used, for example, to drop into a debugger upon a failed
-- assertion or error, for profiling, or updating a GUI progress meter.
-- </p>
-- @param test_filter A function to filter tests that match only conditions that you specify.
-- <p>
-- For example, the folling would allow you to run only tests whose name matches a pattern:
-- </p>
-- <p>
-- <code>
-- function(t) return t.name:match("%s* lexer") end
-- </code>
-- </p>
-- @return A table of result tables. Each result table has the following
-- fields:
-- <ul>
-- <li>assertions_invoked - the number of assertions the test invoked</li>
-- <li>context            - the name of the context</li>
-- <li>message            - a table with an error message and stack trace</li>
-- <li>name               - the name of the test</li>
-- <li>status_code        - the resulting status code</li>
-- <li>status_label       - the label for the status_code</li>
-- </ul>
-- @see load_contexts
-- @see status_codes
-- @function run
local function run(contexts, callbacks, test_filter)

  local results = {}
  local status_names = invert_table(status_codes)
  local test_filter = test_filter or function(a) return a end

  -- Setup a new environment suitable for running a new test
  local function newEnv()
    local env = {}

    -- Make sure globals are accessible in the new environment
    setmetatable(env, {__index = _G})

    -- Setup all the assert functions in the new environment
    for k, v in pairs(assertions) do
      setfenv(v, env)
      env[k] = v
    end

    return env
  end

  local env = newEnv()

  local function invoke_callback(name, test)
    if not callbacks then return end
    if type(callbacks[name]) == "table" then
      for _, c in ipairs(callbacks[name]) do c(test) end
    elseif callbacks[name] then
      callbacks[name](test)
    end
  end

  local function invoke_test(func)
    local assertions_invoked = 0
    env.assertion_callback = function()
      assertions_invoked = assertions_invoked + 1
    end
    setfenv(func, env)
    local result, message = xpcall(func, debug.traceback)
    if result and assertions_invoked > 0 then
      return status_codes.pass, assertions_invoked, nil
    elseif result then
      return status_codes.unassertive, 0, nil
    elseif type(message) == "table" then
      return status_codes.fail, assertions_invoked, message
    else
      return status_codes.err, assertions_invoked, {message, debug.traceback()}
    end
  end

  for i, v in filter(contexts, function(i, v) return v.test and test_filter(v) end) do
    env = newEnv()    -- Setup a new environment for this test

    local ancestors = ancestors(i, contexts)
    local context_name = 'Top level'
    if contexts[i].parent ~= 0 then
      context_name = contexts[contexts[i].parent].name
    end
    local result = {
      assertions_invoked = 0,
      name               = contexts[i].name,
      context            = context_name,
      test               = i
    }
    table.sort(ancestors)
    -- this "before" is the test callback passed into the runner
    invoke_callback("before", result)
    
    -- run all the "before" blocks/functions
    for _, a in ipairs(ancestors) do
      if contexts[a].before then 
        setfenv(contexts[a].before, env)
        contexts[a].before() 
      end
    end

    -- check if it's a function because pending tests will just have "true"
    if type(v.test) == "function" then
      result.status_code, result.assertions_invoked, result.message = invoke_test(v.test)
      invoke_callback(status_names[result.status_code], result)
    else
      result.status_code = status_codes.pending
      invoke_callback("pending", result)
    end
    result.status_label = status_labels[result.status_code]

    -- Run all the "after" blocks/functions
    table.reverse(ancestors)
    for _, a in ipairs(ancestors) do
      if contexts[a].after then 
        setfenv(contexts[a].after, env)
        contexts[a].after() 
      end
    end

    invoke_callback("after", result)
    results[i] = result
  end

  return results

end

--- Return a detailed report for each context, with the status of each test.
-- @param contexts The contexts returned by <tt>load_contexts</tt>.
-- @param results The results returned by <tt>run</tt>.
-- @function test_report
local function test_report(contexts, results)

  local buffer               = {}
  local leading_space        = "  "
  local level                = 0
  local line_char            = "-"
  local previous_level       = 0
  local status_format_len    = 3
  local status_format        = "[%s]"
  local width                = 72
  local context_name_format  = "%-" .. width - status_format_len .. "s"
  local function_name_format = "%-" .. width - status_format_len .. "s"

  local function space()
    return leading_space:rep(level - 1)
  end

  local function add_divider()
    table.insert(buffer, line_char:rep(width))
  end
  add_divider()
  for i, item in ipairs(contexts) do
    local ancestors = ancestors(i, contexts)
    previous_level = level or 0
    level = #ancestors
    -- the 4 here is the length of "..." plus one space of padding
    local name = truncate_string(item.name, width - status_format_len - 4 - #ancestors, '...')
    if previous_level ~= level and level == 0 then add_divider() end
    if item.context then
      table.insert(buffer, context_name_format:format(space() .. name .. ':'))
    elseif results[i] then
      table.insert(buffer, function_name_format:format(space() .. name) ..
        status_format:format(results[i].status_label))
    end
  end
  add_divider()
  return table.concat(buffer, "\n")

end

--- Return a table of stack traces for tests which produced a failure or an error.
-- @param contexts The contexts returned by <tt>load_contexts</tt>.
-- @param results The results returned by <tt>run</tt>.
-- @function error_report
local function error_report(contexts, results)
  local buffer = {}
  for _, r in filter(results, function(i, r) return r.message end) do
    local name = contexts[r.test].name
    table.insert(buffer, name .. ":\n" .. r.message[1] .. "\n" .. r.message[2])
  end
  if #buffer > 0 then return table.concat(buffer, "\n") end
end

--- Get a one-line report and a summary table with the status counts. The
-- counts given are: total tests, assertions, passed tests, failed tests,
-- pending tests, and tests which didn't assert anything.
-- @return A report that can be printed
-- @return A table with the various counts. Its fields are:
-- <tt>assertions</tt>, <tt>errors</tt>, <tt>failed</tt>, <tt>passed</tt>,
-- <tt>pending</tt>, <tt>tests</tt>, <tt>unassertive</tt>.
-- @param contexts The contexts returned by <tt>load_contexts</tt>.
-- @param results The results returned by <tt>run</tt>.
-- @function summary_report
local function summary_report(contexts, results)
  local r = {
    assertions  = 0,
    errors      = 0,
    failed      = 0,
    passed      = 0,
    pending     = 0,
    tests       = 0,
    unassertive = 0
  }
  for _, v in pairs(results) do
    r.tests = r.tests + 1
    r.assertions = r.assertions + v.assertions_invoked
    if v.status_code == status_codes.err then r.errors = r.errors + 1
    elseif v.status_code == status_codes.fail then r.failed = r.failed + 1
    elseif v.status_code == status_codes.pass then r.passed = r.passed + 1
    elseif v.status_code == status_codes.pending then r.pending = r.pending + 1
    elseif v.status_code == status_codes.unassertive then r.unassertive = r.unassertive + 1
    end
  end
  local buffer = {}
  for _, k in ipairs({"tests", "passed", "assertions", "failed", "errors", "unassertive", "pending"}) do
    local number = r[k]
    local label = k
    if number == 1 then
      label = label:gsub("s$", "")
    end
    table.insert(buffer, ("%d %s"):format(number, label))
  end
  return table.concat(buffer, " "), r
end

_M.after_aliases            = after_aliases
_M.make_assertion           = make_assertion
_M.assertion_message_prefix = assertion_message_prefix
_M.before_aliases           = before_aliases
_M.context_aliases          = context_aliases
_M.error_report             = error_report
_M.load_contexts            = load_contexts
_M.run                      = run
_M.test_report              = test_report
_M.status_codes             = status_codes
_M.status_labels            = status_labels
_M.summary_report           = summary_report
_M.test_aliases             = test_aliases
_M.version                  = _VERSION
_M._VERSION                 = _VERSION

return _M
