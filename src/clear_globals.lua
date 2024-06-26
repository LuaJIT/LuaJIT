local clear =  {}


function table_contains(tbl, x)
    found = false
    for _, v in pairs(tbl) do
        if v == x then 
            found = true 
        end
    end
    return found
end

WHITELIST = {"_G", "WHITELIST", "clearAllGlobals", "print", "k", 
            "v", "pairs", "table_contains", "string", "table", 
            "tonumber", "tostring", "type"}

function clear.clearAllGlobals()
    for k, v in pairs(_G) do
        if not table_contains(WHITELIST, tostring(k)) then _G[k] = nil end;
    end
    _G["k"] = nil
    _G["v"] = nil
    _G["WHITELIST"] = nil
    _G["clearAllGlobals"] = nil
    _G["found"] = nil
    _G["table_contains"] = nil
    _G["pairs"] = nil
    _G["_G"] = nil
end
jit.off(table_contains)
jit.off(clear.clearAllGlobals)


return clear