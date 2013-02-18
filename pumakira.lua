local s, t, m, o = require 'string', require 'table', require 'math', require 'os'

local match, gsub, insert, remove, find, sub, len, lower, mod, rnd_seed, rnd, time
  = s.match, s.gsub, t.insert, t.remove, s.find, s.sub, s.len, s.lower, m.mod, m.randomseed, m.random, o.time

s, t, m, o = nil, nil, nil, nil
    
local pumakira, parsed, filters = {}, {}, {}

-- element mode, see http://www.w3schools.com/dom/dom_nodetype.asp
local mode = {
  text    = 3,
  tag     = 1,
  attr    = 2,
  cdata   = 4,
  comment = 8
}

-- elements with sensitive data
local sensitive_tags = {
  ['!--']      = {starts = '%!%-%-',       ends = '%-%->',     mode=mode.comment},
  ['![cdata['] = {starts = '%!%[cdata%[',  ends = '%]%]>',     mode=mode.cdata},
  script       = {starts = 'script[^>]*>', ends = '</script>', mode=mode.tag},
  style        = {starts = 'style[^>]*>',  ends = '</style>',  mode=mode.tag}
}

-- map empty tags
local empty_tags = {
  area         = true,
  base         = true,
  basefont     = true,
  br           = true,
  col          = true,
  frame        = true,
  hr           = true,
  img          = true,
  input        = true,
  isindex      = true,
  link         = true,
  meta         = true,
  param        = true,
  embed        = true,
  ['?xml']     = true,
  ['!doctype'] = true
}

local function log(...)
  --p(...)
end

local function include(table, value)
  local function each_pairs_match(k, v)
    if v==value then return true end
  end
  return each(table, each_pairs_match)==true
end

local function concat(table, second_table)
  for k,v in pairs(second_table) do 
    if type(k)=='number' then insert(table, v) else table[k] = v end 
  end
  return table
end

local function slice(values, i1, i2)
  local res, n = {}, #values
  -- default values for range
  i1 = i1 or 1
  i2 = i2 or n
  if i2 < 0 then
    i2 = n + i2 + 1
  elseif i2 > n then
    i2 = n
  end
  
  if i1 < 1 or i1 > n then
    return {}
  end
  
  local k = 1
  for i = i1,i2 do
    res[k] = values[i]
    k = k + 1
  end
  return res
end

local function get_test(check_val)
  return function(value) return not value and false or (value==check_val) end
end

local function make_value_checker(operator, value)
  value = type(value)=='string' and value or ''
  local operators = {
      ['=']  = function(test_value) return test_value==value end,
      ['~='] = function(test_value) return test_value and find(value, test_value)~=nil          or false end,
      ['^='] = function(test_value) return test_value and sub(test_value, 1, len(value))==value or false end,
      ['$='] = function(test_value) return test_value and sub(test_value, -1*len(value))==value or false end,
      ['*='] = function(test_value) return test_value and find(value, test_value)~=nil          or false end,
      ['|='] = function(test_value) return test_value and (test_value==value or sub(test_value, 1, len(value)+1)==value..'-') or false end
    }
  return operator and operators[operator] or (function(test_value) return test_value and true or false end)
end
  
filters = {
  iterative = {
    contains = function(index, val, ctx)
      if ctx.type==mode.tag then
        if #ctx.children>0 then
          return (ctx.children[1].type==mode.text and ctx.children[1].data) and (find(ctx.children[1].data, val) and true or false) or false
        end
      end
      return false
    end,
    header   = function(index, val, ctx) return match(ctx.name, "^h%d$")~=nil end,
    empty    = function(index, val, ctx) return ctx.type==mode.tag and #ctx.children==0 or false end,
    eq       = function(index, val)      return index==val end,
    gt       = function(index, val)      return index>val end,
    lt       = function(index, val)      return index<val end,
    even     = function(index)           return mod(index, 2)==1 end,
    odd      = function(index)           return mod(index, 2)==0 end,
    first    = function(index)           return index==1 end
  },

  after = {
    last     = function(_, val, ctx)
      return index==nil and (#ctx==0 and {} or (#ctx>1 and ctx[#ctx-1] or ctx[#ctx])) or true
    end
  }
} 

local function r(x, i)
  local s = ('  '):rep(i)
  for k,v in pairs(x) do
    print(s .. tostring(k) .. ' = ' .. tostring(v))
    if type(v) == "table" then r(v, i+1) end
  end
end

local function parse_attributes(element_fragment)
  local attributes = {}
  local function attributes_iterator(k, _, v)
    if k~='' then
      attributes[lower(k)] = v
    end
  end
  gsub(element_fragment, "(%w+)=?([\"?'?]?)(.-)%2%s?", attributes_iterator)
  return attributes
end

local function parse_selector_nodes(fragment, tester)
  local nodes_iterator = function(id_or_class, node)
    if id_or_class=="" and node=="" then return end
    if id_or_class=="#" then
      tester.id       = node
    elseif id_or_class=="." then
      tester.class    = node
    else 
      tester.tag_name = node
    end
  end
  gsub(fragment, "([#%.]?)([^#?%.?%s?$?]*)", nodes_iterator)
end

local function parse_selector_attributes(fragment, tester)
  local attrs_iterator = function(_, attr, operator, _, value)
    if attr=="" and operator=="" and value=="" then return end
    if operator=="" and value=="" then
      tester.attr = attr
    else 
      tester.test = make_value_checker(operator, value)
    end
  end
  gsub(fragment, "([\"']?)([^~?|?%^?%$?%*?=?\"?'?,?$?]*)([~|%^%$%*=]*)([\"']?)([^%1?%4?,?$?]*)%1%4%s*,?%s*", attrs_iterator)
end

local function parse_selector(selector)
  -- consider using LPEG ...
  local inits, _inits, ends, _ends, relation, _relation, nodes, _nodes, pseudo, _pseudo, content, last_ending, parsed_selector = 
        1, 1, 1, 1, '', '', '', '', '', '', '', 1, {}

  while true do
    -- matches 'p' 'p:first' ...'>div' ...'+div:last'
    _inits, _ends, _relation, _nodes, _pseudo     = find(selector, "%s*([%+~>]?)%s*([#%.%w%d]+):?([^%s?%+?~?>?]*)%s*[%+~>]?%s*", last_ending)
    -- matches 'p:eq(1)' ':contains("the invaluable darkness")' '>div[class="boredoom", alt~="nil"]' '[attr="foo bar"]'
    inits, ends, relation, nodes, pseudo, content = find(selector, "%s*([%+~>]?)%s*([#%.%w%d]*):?([%w]*)[%(%[]+([^%]?%)?]*)[%]%)]?%s*[%+~>]?%s*", last_ending)

    if inits==nil and _inits==nil then
      break
    end

    local tester = {}
    
    if inits==nil then
      relation, nodes, pseudo, ends, content = _relation, _nodes, _pseudo, _ends, ''
    else
      if _inits~=nil then
        if inits>_inits then
          relation, nodes, pseudo, ends, content = _relation, _nodes, _pseudo, _ends, ''
        end
      end
    end

    tester.relation, tester.pseudo, last_ending = relation, pseudo, ends
    parse_selector_nodes(nodes, tester)
    parse_selector_attributes(content, tester)
    insert(parsed_selector, tester)
  end

  return parsed_selector
end

local function gen_unique_id(seed)
  rnd_seed(time() * seed)
  return rnd()
end

pumakira = setmetatable({
  
  dump  = function(self, x)
    r(x, 0)  
  end,

  add_filter = function(self, name, fn, is_iterative)
    is_iterative = is_iterative==nil and true or is_iterative
    filters[(is_iterative and 'iterative' or 'after')][name] = fn
  end,

  parse = function(self, s)
    local stack, top, i, j, unique_id = {}, {children={}}, 1, 1, 0
    local ni, c, label, xarg, empty, text
    insert(stack, top)
    while true do
      ni, j, c, label, xarg, empty = find(s, "<(%/?%??)([%w:%!%-%-%[]+)(.-)(%/?)>", i)    
      label = label and lower(label) or nil
      empty = empty_tags[label] and "/" or empty
      -- detect sensitive tags
      if sensitive_tags[label] then
        ni, j, c  = find(s, sensitive_tags[label].starts.."(.-)"..sensitive_tags[label].ends, i)
        empty, ni = "/", ni-1
      end
      if not ni then break end
      -- generate uniqueness
      unique_id = gen_unique_id(i)
      -- parse text
      text    = sub(s, i, ni-1)
      if not find(text, "^[%s%c]*$") then
        insert(
          top.children, 
          {
            data      = text, 
            type      = mode.text,
            unique_id = unique_id,
            parent    = top.unique_id
          })
      end
      if empty == "/" then  -- empty element tag
        if sensitive_tags[label] then
          insert(
            top.children,
            {
              name       = lower(label),
              type       = sensitive_tags[label].mode,
              attributes = {},
              parent     = top.unique_id,
              unique_id  = unique_id,
              children   = {
                data      = c, 
                type      = mode.text,
                parent    = unique_id,
                unique_id = gen_unique_id(i * 1024)
              }
            })
        else
          insert(
            top.children, 
              {
                name       = lower(label),
                type       = mode.tag,
                attributes = parse_attributes(xarg),
                children   = {},
                parent     = top.unique_id,
                unique_id  = unique_id
              })
        end
      elseif c == "" then   -- start tag
        top = {
          name       = lower(label), 
          attributes = parse_attributes(xarg), 
          type       = mode.tag, 
          parent     = 0,
          unique_id  = unique_id,
          children   = {}
        }
        insert(stack, top)   -- new level
      else  -- end tag
        local toclose = remove(stack)  -- remove top
        top = stack[#stack]
        if #stack < 1 then
          error("nothing to close with "..label)
        end
        if toclose.name ~= label then
          error("trying to close "..toclose.label.." with "..label)
        end
        toclose.parent = top.unique_id or 0
        insert(top.children, toclose)
      end
      i = j+1
    end
    local text = sub(s, i)
    if not find(text, "^[%s$c]*$") then
      insert(
        stack[#stack].children, 
        {
          data      = text, 
          type      = mode.text,
          parent    = stack[#stack].unique_id,
          unique_id = unique_id
        })
    end
    if #stack > 1 then
      error("unclosed "..stack[#stack].name)
    end
    parsed = stack[1].children
    return parsed
  end,

  test_element = function(self, options, element)
    if not element then return false end
    
    if options.test then
      if not options.test(element) then return false end
    end

    if options.tag_name then
      if element.type~=mode.tag                 then return false end
      if not options.tag_name(element.name)     then return false end
    end

    if options.tag_type then
      if not options.tag_type(element.type)     then return false end
    end

    if options.tag_contains then
      if element.type~=mode.text   and
        element.type~=mode.comment and
        element.type~=mode.cdata                then return false end
      if not options.tag_contains(element.data) then return false end
    end

    for key, value in pairs(options) do
      if key~='tag_name' and key~='tag_type' and key~='tag_contains' and key~='test' and key~='pseudo_test' then
        if not element.attributes               then return false end
        if not element.attributes[key]          then return false end
        if not value(element.attributes[key])   then return false end
      end
    end

    if options.pseudo_test then
      if not options.pseudo_test(element)       then return false end
    end

    return true
  end,

  get_elements = function(self, options, current_element, recurse, limit)
    recurse = recurse or true
    limit   = type(limit)=='number' and limit or -1
    if not current_element then return {} end
    local found, element_list = {}, {}   
    for key, value in pairs(options) do
      if type(value)~='function' then
        options[key] = get_test(value)
      end
    end
    if self:test_element(options, current_element) then
      log('found ... inserting', current_element, found)
      insert(found, current_element)
    end
    if limit>-1 and #found>=limit then
      return found
    end
    if recurse and current_element.children then
      element_list = current_element.children
    elseif type(current_element)=='table' then
      element_list = current_element
    else
      return found
    end
    for i=1, #element_list do
      found = concat(found, self:get_elements(options, element_list[i], recurse, limit))
      if limit>-1 and #found>=limit then
        return found
      end
    end
    return found
  end,

  get_element_by_id = function(self, id, current_element, recurse)
    local result = self:get_elements({id = id}, (current_element or parsed), recurse, 1)
    return #result==1 and result[1] or nil
  end,

  get_elements_by_tag_name = function(self, name, current_element, recurse, limit)
    return self:get_elements({tag_name = name}, (current_element or parsed), recurse, limit)
  end,

  get_elements_by_tag_type = function(self, type, current_element, recurse, limit) 
    return self:get_elements({tag_type = type}, (current_element or parsed), recurse, limit)
  end,

  get_elements_by_class_name = function(self, class_name, current_element, recurse, limit)
    return self:get_elements({class = class_name}, (current_element or parsed), recurse, limit)
  end,

  get_parent_node = function(self, node, current_element, recurse)
    local recurse_test = function(element)
      if element.unique_id == node.parent then
        for k=1, #element.children do
          if element.children[k].unique_id==node.unique_id then
            return true
          end
        end
      end
      return false
    end
    local result = self:get_elements({test = recurse_test}, (current_element or parsed), recurse, 1)
    return #result==1 and result[1] or nil
  end,

  query = function(self, selector, current_element)
    self:dump(parse_selector(selector))
  end,

  filters = filters,

  mode = mode

},{__index = pumakira})

return pumakira