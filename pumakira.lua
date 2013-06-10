local s, t, m, o, u = require 'string', require 'table', require 'math', require 'os', require './utils'

local match, gsub, char, format, insert, remove, concat, find, sub, len, lower, rnd_seed, rnd, time, dump
  = s.match, s.gsub, s.char, s.format, t.insert, t.remove, t.concat, s.find, s.sub, s.len, s.lower, m.randomseed, m.random, o.time, u.dump

s, t, m, o, u = nil, nil, nil, nil, nil
    
local pumakira, filters, var_dump = {}, {}, require('utils').dump

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
  local args, i = {...}, 1
  for i=1, #args do
    args[i] = dump(args[i], 2)
  end
  process.stdout:write(concat(args, "\t") .. "\n")
end

local function include(table, value)
  local function each_pairs_match(k, v)
    if v==value then return true end
  end
  return each(table, each_pairs_match)==true
end

local function concatenate(table, second_table)
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

local filters = {
  iterative = {
    contains = "(e.type~=mode.tag and false or (#e.children<1 and (e.children[1].type==mode.text and e.children[1].data)"..
               ' and (find(e.children[1].data, "$val") and true or false) or false))',
    header   = "match(e.name, \"^h%d$\")~=nil",
    empty    = "(e.type==mode.tag and #e.children==0 or false)",
    eq       = "$i==$val",
    gt       = "$i>$val",
    lt       = "$i<$val",
    even     = "($i % 2)==1",
    odd      = "($i % 2)==0",
    first    = "$i==1"
  },
  after = {
    last     = function(_, val, ctx)
      return index==nil and (#ctx==0 and {} or (#ctx>1 and ctx[#ctx-1] or ctx[#ctx])) or true
    end
  }
}

local operations = {
  ['*']  = '',
  ['=']  = '$test=="$val"',
  ['~='] = 'find(" $val ", $test)~=nil',
  ['^='] = 'not (sub($test, 1, len("$val"))=="$val")',
  ['$='] = 'not (sub($test, -1*len("$val"))=="$val")',
  ['*='] = 'find("$val", $test)~=nil',
  ['|='] = 'not ($test=="$val" or sub($test, 1, len("$val")+1)=="$val".."-")'
}

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

local function parse_selector_nodes(fragment, attrs)
  local nodes_iterator = function(id_or_class, node)
    if id_or_class=='' and node=='' then return end
    if id_or_class=='#' then
      insert(attrs, {{'id', '=', node}})
    elseif id_or_class=='.' then
      insert(attrs, {{'class', '*=', node}})
    else 
      insert(attrs, {{'name', '=', node}})
    end
  end
  gsub(fragment, "([#%.]?)([^#?%.?%s?$?]*)", nodes_iterator)
end

local function parse_selector_attributes(fragment, attrs, pseudos, pseudo) -- tester
  local attributes = {}
  local attrs_iterator = function(_, attr, operator, _, value)
    if attr=="" and operator=="" and value=="" then return end
    if operator=="" and value=="" then
      if pseudo=="" then
        insert(attributes, {attr, "*", ''})
      else
        insert(pseudos, {pseudo, attr})
      end
    else
      insert(attributes, {attr, operator, value}) -- make_value_checker
    end
  end
  if fragment=='' then
    if pseudo~='' then
      insert(pseudos, {pseudo, ''})
    end
  else
    gsub(fragment, "([\"']?)([^~?|?%^?%$?%*?=?\"?'?,?$?]*)([~|%^%$%*=]*)([\"']?)([^%1?%4?,?$?]*)%1%4%s*,?%s*", attrs_iterator)
  end
  if #attributes>0 then
    insert(attrs, attributes)
  end
end

local function parse_selector(selector)
  -- consider using LPEG ...
  local inits, _inits, ends, _ends, raw, _raw, relation, _relation, nodes, _nodes, __, pseudo, _pseudo, content, last_ending, parsed_selector, x = 
        1, 1, 1, 1, '', '', '', '', '', '', '', '', '', '', 1, {}, 1
  -- a hundred conditions blocks, i think its fair ... safe way against pattern bug
  for x=1, 100 do
    -- matches 'p' 'p:first' ...'>div' ...'+div:last'
    _inits, _ends, _raw, _relation, __, _nodes, _pseudo = find(selector, "(([%s%+~>]*)(:?)([#%.%w%d]+):?([^%s?%+?~?>?]*))", last_ending)
    -- matches 'p:eq(1)' ':contains("the invaluable darkness")' '>div[class="boredoom", alt~="nil"]' '[attr="foo bar"]'
    inits, ends, raw, relation, nodes, pseudo, content = find(selector, "(([%s%+~>]*)([#%.%w%d]*):?([%w]*)[%(%[]+([^%]?%)?]*)[%]%)]?)", last_ending)
    -- end loop when theres nothing to parse anymore or high number of loops, wich indicates error :(
    if inits==nil and _inits==nil then
      break
    end
    -- fix node+attr+pseudo occurences
    if __==":" then 
      _pseudo = _nodes 
      _nodes  = '' 
    end
    -- check which pattern achieved result or best result
    if inits==nil then
      raw, relation, nodes, pseudo, ends, content = _raw, _relation, _nodes, _pseudo, _ends, ''
    else
      if _inits~=nil then
        if inits>_inits then
          raw, relation, nodes, pseudo, ends, content = _raw, _relation, _nodes, _pseudo, _ends, ''
        end
      end
    end
    -- join conditions/pseudos when possible
    if relation=='' and x>1 then
      parsed_selector[#parsed_selector].raw = parsed_selector[#parsed_selector].raw .. raw
      parse_selector_nodes(nodes, parsed_selector[#parsed_selector].nodes_attr)
      parse_selector_attributes(content, parsed_selector[#parsed_selector].attrs, parsed_selector[#parsed_selector].pseudos, pseudo)
    else 
      local test    = {relation='', attrs={}, nodes_attr={}, pseudos={}, raw=gsub(raw, "[%s%+~>]*(.)%s*", "%1")}
      test.relation = gsub(relation, "%s*(.)%s*", "%1")
      parse_selector_nodes(nodes, test.nodes_attr)
      parse_selector_attributes(content, test.attrs, test.pseudos, pseudo)
      insert(parsed_selector, test)
    end
    last_ending = ends+1
  end
  return parsed_selector
end

-- make value checker for pseusos
local function make_pseudo_checker(pseudo, value, index_var_name)
  if filters.iterative[pseudo] then
    return gsub(gsub(filters.iterative[pseudo], "%$i", index_var_name), "%$val", value)
  end
end

-- make value checker for attributes
local function make_value_checker(tester, operator, value)
  return operations[operator] and gsub(gsub(operations[operator], "%$val", value), "%$test", tester) or tester
end

-- generate code for pseudos
local function generate_code_for_pseudos(pseudos, first_code_block, main_code_block)
  local counter_var, counter, i = '', 'current_parent, ', 0 
  for i=1, #pseudos do
    counter_var      = '_'..char(i+96)
    counter          = counter.. (i==1 and '' or ', ')..counter_var
    first_code_block = first_code_block .. (i==1 and '0' or ', 0')
    main_code_block  = main_code_block .. format(
        "\n    %s = %s+1\n    if not(%s) then return false end \n",
        counter_var,
        counter_var,
        make_pseudo_checker(pseudos[i][1], pseudos[i][2], counter_var)
      )
  end
  return format("local %s = 0.0, %s", counter, first_code_block), 
         format("\n    if e.parent~=current_parent then %s = e.parent, %s end\n%s", counter, first_code_block, main_code_block)
end

-- generate code for attributes
local function generate_code_for_attributes(attrs, generated_code)
  local i, x, child_length, is_or_condional = 0, 0, 0, false
  generated_code = generated_code .. '\n    if '
  for i=1, #attrs do
    child_length    = #attrs[i]
    is_or_condional = child_length > 1
    generated_code  = generated_code .. (is_or_condional and '(' or '')
    for x=1, child_length do
      generated_code = generated_code .. format(
          (attrs[i][x][1]=='name' and '(e.%s and %s)%s' or 
            (attrs[i][x][2]=='*' and '(e.attributes["%s"]%s)%s' or '(e.attributes["%s"] and %s)%s')
          ), 
          attrs[i][x][1],
          make_value_checker(
            format((attrs[i][x][1]=='name' and 'e.%s' or 'e.attributes["%s"]'), attrs[i][x][1]),
            attrs[i][x][2],
            attrs[i][x][3]
          ),
          (is_or_condional and (child_length==x and (#attrs==i and ')' or ') and ') or ' or ') or (#attrs==i and '' or ' and ') )
        )
    end
  end
  return generated_code .. " then else return false end\n"
end

-- generate code and evaluate to comparator closure
local function generate_and_evaluate_code(parsed_section)
  local first_block, middle_block, last_block, method, env = '', '', '', nil, {mode=mode, find=find}

  if parsed_section.nodes_attr and #parsed_section.nodes_attr>0 then
    middle_block = generate_code_for_attributes(parsed_section.nodes_attr, middle_block)
  end

  if parsed_section.attrs and #parsed_section.attrs>0 then
    middle_block = generate_code_for_attributes(parsed_section.attrs, middle_block)
  end

  if parsed_section.pseudos and #parsed_section.pseudos>0 then
    first_block, last_block = generate_code_for_pseudos(parsed_section.pseudos, first_block, last_block)
  end

  method = loadstring(
            format(
              "local function fn_cmp()\n  %s\n  local function cmp(e)" ..
              "\n%s%s\n  return true\n  end\n  return cmp\nend\nreturn fn_cmp()",
              first_block,
              middle_block,
              last_block
            ))
  return method and setfenv(method(), env) or nil
end

local function gen_unique_id(seed)
  rnd_seed(time() * seed)
  return rnd()
end

function pumakira.new()

  local parsed, cache, relations, options, this = 
        '', {},  {['>']=1, ['~']=2, ['+']=3}, {uniqueness={}, found=false, relation=0}, {}

  local function traverse_elements(fn, current_element, result, opts, limit)
    
    -- (0)   all children, grandchildren and so on ...
    -- (1) > children
    -- (2) ~ all next siblings
    -- (3) + next sibling

    if fn(current_element) and not opts.uniqueness[current_element.unique_id] then
      if opts.relation<2 then
        insert(result, current_element)
      else
        opts.found = true
      end
      opts.uniqueness[current_element.unique_id] = true
    end

    if (not current_element.children and #current_element==0) or limit==0 then
      return
    end

    local traversable_elements = current_element.children and current_element.children or current_element

    for i=1, #traversable_elements do
      --
      if opts.found and not opts.uniqueness[traversable_elements[i]] then
        insert(result, traversable_elements[i])
        if opts.relation==3 then
          opts.found=false
        end
      else
        traverse_elements(fn, traversable_elements[i], result, opts, limit-1)
      end
    end

    if opts.found and opts.relation>1 then
      opts.found = false
    end
  end

  this = {
    
    dump  = function(self, x)
      log(x)
    end,

    parse = function(self, s)
      local stack, top, i, j, unique_id, ni, c, label, xarg, empty, text = 
            {}, {children={}}, 1, 1, 0, nil, nil, nil, nil, nil, nil
      --
      insert(stack, top)
      --
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
              index     = #top.children+1,
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
                index      = #top.children+1,
                name       = lower(label),
                type       = sensitive_tags[label].mode,
                attributes = {},
                parent     = top.unique_id,
                unique_id  = unique_id,
                children   = {
                  index     = 1,
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
                  index      = #top.children+1,
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
            index      = 1,
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
          top           = stack[#stack]
          if #stack < 1 then
            error("nothing to close with "..label)
          end
          if toclose.name ~= label then
            error("trying to close "..toclose.label.." with "..label)
          end
          toclose.parent = top.unique_id or 0
          toclose.index  = #top.children + 1
          insert(top.children, toclose)
        end
        i = j+1
      end
      local text = sub(s, i)
      if not find(text, "^[%s$c]*$") then
        insert(
          stack[#stack].children, 
          {
            index     = #stack[#stack].children+1,
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
        found = concatenate(found, self:get_elements(options, element_list[i], recurse, limit))
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
      local function test_class(element)
        if not element.attributes       then return false end
        if not element.attributes.class then return false end
        if find(element.attributes.class, class_name)~=nil then
          return true
        end
      end
      return self:get_elements({test = test_class}, (current_element or parsed), recurse, limit)
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
      local ps, result, old_result, limit = parse_selector(selector), {}, {}, -1

      self:dump(ps)
      --
      options.relation = ps[2] and (relations[ps[2].relation] or 0) or 0
      cache[ps[1].raw] = cache[ps[1].raw] or generate_and_evaluate_code(ps[1])
      limit            = options.relation>1 and 2 or -1
      traverse_elements(
          cache[ps[1].raw],
          (current_element or parsed),
          result,
          options,
          limit
        )
      --
      remove(ps, 1)
      --
      for i=1, #ps do
        --
        old_result         = result
        result             = {}
        options.uniqueness = {}
        options.relation   = ps[i+1] and (relations[ps[i+1].relation] or 0) or 0
        cache[ps[i].raw]   = cache[ps[i].raw] or generate_and_evaluate_code(ps[i])
        limit              = options.relation>1 and 2 or -1
        --
        if ps[i].raw=="blockquote[class]" then
          self:dump(old_result)
          p(ps[i].raw, limit)
        end
        --
        traverse_elements(
            cache[ps[i].raw],
            old_result,
            result,
            options,
            limit
          )
        --
      end
      -- reset
      options.uniqueness = {}
      options.found      = false
      options.relation   = 0
      --
      p(result)
    end,

    dump_cache = function(self)
      self:dump(cache)
    end
  }
  setmetatable(this, pumakira)
  return this
end

return pumakira