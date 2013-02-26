local s, t = require 'string', require 'table'
local rep, gsub, find, concat = s.rep, s.gsub, s.find, t.concat
s, t = nil, nil

local colors = {
  black        = "\27[0;30m", -- black
  red          = "\27[0;31m", -- red
  green        = "\27[0;32m", -- green
  boolean      = "\27[0;33m", -- yellow      boolean
  number       = "\27[0;34m", -- blue        number
  userdata     = "\27[0;35m", -- magenta     userdata
  ['function'] = "\27[0;36m", -- cyan        function
  white        = "\27[0;37m", -- white
  B            = "\27[1;m",   -- B
  ['nil']      = "\27[1;30m", -- Bblack      nil
  thread       = "\27[1;31m", -- Bred        thread
  Bgreen       = "\27[1;32m", -- Bgreen
  Byellow      = "\27[1;33m", -- Byeallow
  Bblue        = "\27[1;34m", -- Bblue
  cdata        = "\27[1;35m", -- Bmagenta    cdata
  Bcyan        = "\27[1;36m", -- Bcyan
  Bwhite       = "\27[1;37m", -- Bwhite
  _            = "\27[0m"     -- reset
}

local scapes = {
  ["\\"] = colors.Bgreen .. "\\\\" .. colors.green,
  ["\0"] = colors.Bgreen .. "\\0"  .. colors.green,
  ["\n"] = colors.Bgreen .. "\\n"  .. colors.green,
  ["\r"] = colors.Bgreen .. "\\r"  .. colors.green,
  ["\t"] = colors.Bgreen .. "\\t"  .. colors.green,
  quote  = colors.Bgreen .. '"'    .. colors.green,
  quote2 = colors.Bgreen .. '"'    .. colors._,
  ["["]  = colors.B      .. '['    .. colors._,
  ["]"]  = colors.B      .. ']'    .. colors._
}

local function colorize_escape(scape) 
  return scapes[scape] or scape
end

local function simple_dump(data, depth)
  local t = type(data)
  if t=='string' then
    return scapes.quote .. gsub(data, "%c", colorize_escape) .. scapes.quote2
  elseif t=='table' then 
    local indent, is_array, estimate, i, lines, source = rep(' ', depth), true, 0, 1, {}, ''
    for k,v in pairs(data) do if not (k==i) then is_array=false end i=i+1 end
    i = 1
    for k,v in (is_array and ipairs or pairs)(data) do
      s = is_array and '' or ((type(k)=='string' and find(k, "^[%a_][%a%d_]*$")~=nil) and k..' = ' or '['..simple_dump(k, depth+2)..'] = ')
      s = s..simple_dump(v, depth+2)
      lines[i] = s
      estimate = estimate + #s
      i = i + 1
    end
    return estimate>200 and "{\n"..indent.. concat(lines, ",\n"..indent) .."\n"..rep(' ', depth-2).."}" or "{"..concat(lines, ", ").."}"
  else
    return colors[t] .. tostring(data) .. colors._
  end
end

return {dump = simple_dump}