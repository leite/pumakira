--
-- Pumakira, html parser and css selector engine
--
-- @author    leite (xico@simbio.se)
-- @license   MIT
-- @copyright Simbiose 2015

local string, table, utils = require 'string', require 'table', require './utils'

local char, find, format, gmatch, gsub, len, lower, match, sub, concat, insert, remove, log, assert,
  loadstring, pairs, rawequal, setfenv, tonumber, type =
  string.char, string.find, string.format, string.gmatch, string.gsub, string.len, string.lower,
  string.match, string.sub, table.concat, table.insert, table.remove, utils.log, assert, loadstring,
  pairs, rawequal, setfenv, tonumber, type

string, table, utils = nil, nil, nil

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

-- module variables
--   ID and NAME tokens must begin with a letter ([A-Za-z]) and may be followed by any number of
--   letters, digits ([0-9]), hyphens ("-"), underscores ("_"), colons (":"), and periods (".").

local EMPTY, cache, relations, skipped_options, should_escape, should_unescape =
  '', {}, {['>']=1, ['~']=2, ['+']=3},
  {tag_name=true, tag_type=true, tag_contains=true, test=true, pseudo_test=true},
  {['.']=46, [':']=58, ['[']=91, [']']=93, ['\\']=92, ['+']=43, ["'"]=39, ['"']=34, [')']=41, ['(']=40},
  {[46]='.', [58]=':', [91]='[', [93]=']', [92]='\\', [43]='+', [39]="'", [34]='"', [41]=')', [40]='('}

-- pseudo filters

local filters = {
  iterative = {
    contains = 'e.type==mode.tag and contains(e, "$val")',
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

-- css query operands

local operations = {
  ['*']  = '',
  ['=']  = '$test=="$val"',
  ['~='] = 'find(" $val ", $test)~=nil',
  ['^='] = 'not (sub($test, 1, len("$val"))=="$val")',
  ['$='] = 'not (sub($test, -1*len("$val"))=="$val")',
  ['*='] = 'find("$val", $test)~=nil',
  ['|='] = 'not ($test=="$val" or sub($test, 1, len("$val")+1)=="$val".."-")'
}

-- concatenate tables
--
-- @table  table
-- @table  second_table
-- @return table

local function concatenate (table, second_table)
  for k,v in pairs(second_table) do
    if type(k)=='number' then insert(table, v) else table[k] = v end
  end
  return table
end

-- check if node contains a text value
--
-- @table  e
-- @string val
-- @return boolean

local function contains (e, val)
  for i=1, #e.children do
    if e.children[i].type==mode.text and e.children[i].data and find(e.children[i].data, val) then
      return true
    end
  end
  return false
end

-- unescape sensitive data
--
-- @string string
-- @return string

local function unescape (string)
  return gsub (string, '&(%d%d)', function (character)
    return should_unescape[tonumber(character)] or char(tonumber(character))
  end)
end

-- escape sensitive data
--
-- @string string
-- @return string

local function escape (string)
  return gsub (string, '%\\([\34\39\40\41\43\46\58\91\92])', function (character)
    return '&'.. should_escape[character]
  end)
end

-- insertation sort with set as reference
--
-- @table list
-- @table set

local function sort (list, set)
  local l, j, k = #list, 0, nil
  for i = 2, l do
    k, j = list[i], i - 1
    while j > 0 and set[list[j]] > set[k] do
      list[j + 1], j = list[j], j - 1
    end
    list[j + 1] = k
  end
end

-- generates a test closure
--
-- @any    check_val
-- @return function

local function get_test (check_val)
  return function(value) return not value and false or (value==check_val) end
end

-- parse node attributes
--
-- @string element_fragment
-- @return table

local function parse_attrs (element_fragment)
  local attrs = {}
  for key, _, value in gmatch(element_fragment, "(%w+)=?([\"?'?]?)(.-)%2%s?") do
    if key ~= EMPTY then
      attrs[lower(key)] = value
    end
  end
  return attrs
end

-- parse selector nodes
--
-- @string fragment
-- @table  attrs

local function parse_selector_nodes (fragment, attrs)
  for classifier, value in gmatch(fragment, "([#%.]?)([^#?%.?%s?$?]*)") do
    if classifier == EMPTY and value ~= EMPTY then
      insert(attrs, {{'name', '=', value}})
    elseif classifier == '#' then
      insert(attrs, {{'id', '=', value}})
    elseif classifier == '.' then
      insert(attrs, {{'class', '*=', value}})
    end
  end
end

-- parse selector attributes
--
-- @string fragment
-- @table  attrs
-- @table  pseudos
-- @string pseudo

local function parse_selector_attributes (fragment, attrs, pseudos, pseudo)
  if fragment == EMPTY then
    if pseudo ~= EMPTY then
      insert(pseudos, {pseudo, EMPTY})
    end
  else
    local attributes = {}
    for _, attr, op, __, value in gmatch(
      fragment, "([\"']?)([^~?|?%^?%$?%*?=?\"?'?,?$?]*)([~|%^%$%*=]*)([\"']?)([^%1?%4?,?$?]*)%1%4%s*,?%s*"
    ) do
      if not(attr == EMPTY and op == EMPTY and value == EMPTY) then
        if op == EMPTY and value == EMPTY then
          if pseudo == EMPTY then
            insert(attributes, {attr, '*', EMPTY})
          else
            insert(pseudos, {pseudo, attr})
          end
        else
          insert(attributes, {attr, op, value})
        end
      end
    end
    if #attributes > 0 then
      insert(attrs, attributes)
    end
  end
end

-- parse css selector
--
-- @string selector

local function parse_selector (selector)
  --local escaped = false
  -- trim and unscape selector, compares with cache and go ahead
  selector = escape(selector)

  -- consider using LPEG ...
  local inits, _inits, ends, _ends, raw, _raw, relation, _relation, nodes, _nodes, separator, pseudo,
    _pseudo, content, last_ending, parsed_selector =
    1, 1, 1, 1, '', '', '', '', '', '', '', '', '', '', 1, {}
  -- a hundred conditions blocks, i think its fair ... safe way against pattern bug
  for x = 1, 33 do
    -- matches 'p' 'p:first' ...'>div' ...'+div:last'
    _inits, _ends, _raw, _relation, separator, _nodes, _pseudo =
      find(selector, "[%c]*(([%s%+~>]?)[%c%s]*(:?)([#%.%w%d]+):?([^%s?%+?~?>?]*))", last_ending)
    -- matches 'p:eq(1)' ':contains("the invaluable darkness")' '>div[class="boredoom", alt~="nil"]' '[attr="foo bar"]'
    inits, ends, raw, relation, nodes, pseudo, content =
      find(selector, "[%c]*(([%s%+~>]?)[%c%s]*([#%.%w%d]*):?([%w]*)[%(%[]+([^%]?%)?]*)[%]%)]?)", last_ending)
    -- end loop when theres nothing to parse anymore or high number of loops, wich indicates error :(

    if not inits and not _inits then
      break
    end

    -- fix node+attr+pseudo occurences
    if separator == ":" then
      _pseudo, _nodes = _nodes, ''
    end

    -- check which pattern achieved result or best result
    if not inits then --if inits==nil then
      raw, relation, nodes, pseudo, ends, content =
        _raw, _relation, unescape(_nodes), unescape(_pseudo), _ends, ''
    else
      if _inits and inits > _inits then
        raw, relation, nodes, pseudo, ends, content =
          _raw, _relation, unescape(_nodes), unescape(_pseudo), _ends, ''
      end
      content = unescape(content)
    end
    --
    --print ()
    --print( '<'..selector..'>' )
    --print ('['..raw..'] ['..relation..'] ['..nodes..'] ['..pseudo..'] ['..content..'] ')
    --print ()

    -- join conditions/pseudos when possible
    if relation == '' and x > 1 then
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

-- make value checker for pseudos
--
-- @string pseudo
-- @any    value
-- @any    index_var_name
-- @return string

local function make_pseudo_checker (pseudo, value, index_var_name)
  if filters.iterative[pseudo] then
    return gsub(gsub(filters.iterative[pseudo], "%$i", index_var_name), "%$val", value)
  end
end

-- make value checker for attributes
--
-- @string tester
-- @string operator
-- @string value

local function make_value_checker (tester, operator, value)
  return operations[operator] and
    gsub(gsub(operations[operator], "%$val", value), "%$test", tester) or tester
end

-- generate code for pseudos
--
-- @table  pseudos
-- @string first_code_block
-- @string main_code_block
-- @return string

local function generate_code_for_pseudos (pseudos, first_code_block, main_code_block)
  local counter_var, counter, i = '', 'current_parent, ', 0
  for i=1, #pseudos do
    counter_var      = '_'..char(i+96)
    counter          = counter.. (i==1 and '' or ', ')..counter_var
    first_code_block = first_code_block .. (i==1 and '0' or ', 0')
    main_code_block  = main_code_block .. format(
        "\n    %s = %s+1\n\n    if not(%s) then\n      return false\n    end\n",
        counter_var,
        counter_var,
        make_pseudo_checker(pseudos[i][1], pseudos[i][2], counter_var)
      )
  end
  return format("local %s = {}, %s", counter, first_code_block), format(
      "\n    if e.parent~=current_parent then\n       %s = e.parent, %s\n    end\n    %s",
      counter, first_code_block, main_code_block
    )
end

-- generate code for attributes
--
-- @table  attrs
-- @string generated_code
-- @return string

local function generate_code_for_attributes (attrs, generated_code)
  local i, x, child_length, is_or_condional = 0, 0, 0, false
  generated_code = generated_code .. '\n    if not('
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
  return generated_code .. ') then\n      return false\n    end\n'
end

-- generate code and evaluate for traversing
--
-- @table  parsed_section
-- @return function

local function generate_and_evaluate_code (parsed_section)
  local first_block, middle_block, last_block, method, env =
    '', '', '', nil, {mode=mode, find=find, contains=contains, match=match, sub=sub, len=len}

  if parsed_section.nodes_attr and #parsed_section.nodes_attr>0 then
    middle_block = generate_code_for_attributes(parsed_section.nodes_attr, middle_block)
  end

  if parsed_section.attrs and #parsed_section.attrs>0 then
    middle_block = generate_code_for_attributes(parsed_section.attrs, middle_block)
  end

  if parsed_section.pseudos and #parsed_section.pseudos>0 then
    first_block, last_block = generate_code_for_pseudos(parsed_section.pseudos, first_block, last_block)
  end

  method = loadstring(format(
    "local function fn_cmp()\n  %s\n  local function cmp(e)" ..
    "\n%s%s\n    return true\n  end\n  return cmp\nend\nreturn fn_cmp()",
    first_block,
    middle_block,
    last_block
  ))

  return method and setfenv(method(), env) or nil
end

-- traverse tree of nodes
--
-- @function fn
-- @table    current_element
-- @table    result
-- @table    opts
-- @number   limit

local function traverse_elements (fn, current_element, result, opts, limit)

  if fn(current_element) and not opts.uniqueness[current_element] then
    if opts.relation<2 then
      insert(result, current_element)
    else
      opts.found = true
    end
    opts.uniqueness[current_element] = true
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

-- main closure
--

local function pumakira ()

  local parsed, options, set, this = '', {uniqueness={}, found=false, relation=0}, {}, {}

  this = {

    -- dump/log
    --
    -- @any x

    dump  = function(self, x)
      log(x)
    end,

    -- parse html/xhtml string
    --
    -- @string s
    -- @return table

    parse = function(self, s)
      local stack, top, element, text, index, i, j, ni, c, label, xarg =
        {}, {children={}}, {}, '', 1, 1, 1, nil, nil, nil, nil

      insert(stack, top)
      --
      while true do
        ni, j, c, label, xarg, empty = find(s, "<(%/?%??)([%w:%!%-%-%[]+)(.-)(%/?)>", i)

        label = label and lower(label) or nil
        empty = empty_tags[label] and '/' or empty
        -- detect sensitive tags
        if sensitive_tags[label] then
          ni, j, c  = find(s, sensitive_tags[label].starts.."(.-)"..sensitive_tags[label].ends, i)
          empty, ni = '/', ni-1
        end
        if not ni then
          break
        end
        -- parse text
        text = sub(s, i, ni-1)
        if not find(text, "^[%s%c]*$") then
          element      = {data=text, type=mode.text, parent=top}
          set[element] = index
          index        = index + 1
          insert(top.children, element)
        end
        if empty == '/' then  -- empty element tag
        --if empty then
          if sensitive_tags[label] then
            element                  = {name=label, attributes={}, type=sensitive_tags[label].mode, parent=top}
            element.children         = {{data=c, type=mode.text, parent=element}}
            set[element]             = index
            index                    = index + 1
            set[element.children[1]] = index
            insert(top.children, element)
          else
            element      = {name=label, type=mode.tag, attributes=parse_attrs(xarg), children={}, parent=top}
            set[element] = index
            insert(top.children, element)
          end
        elseif c == "" then   -- start tag
          top      = {name=label, attributes=parse_attrs(xarg), type=mode.tag, parent=nil, children={}}
          set[top] = index
          insert(stack, top)   -- new level
        else  -- end tag
          local toclose = remove(stack)  -- remove top
          top           = stack[#stack]

          assert(#stack > 0, format('nothing to close with `%s`', label))
          assert(
            toclose.name == label, format('trying to close `%s` with `%s`', toclose.label, label)
          )
          toclose.parent = top
          insert(top.children, toclose)
        end
        i     = j + 1
        index = index + 1
      end
      --local text = sub(s, i)
      text = sub(s, i)
      if not find(text, "^[%s%c]*$") then
        element      = {data=text, type=mode.text, parent=stack[#stack]}
        set[element] = index + 1
        insert(stack[#stack].children, element)
      end
      assert(#stack == 1, format('unclosed `%s`', stack[#stack].name))
      --if #stack > 1 then
      --  error("unclosed "..stack[#stack].name)
      --end
      parsed = stack[1].children
      return parsed
    end,

    -- test element agains name, type and other pseudos tests
    --
    -- @table options
    -- @table element
    -- @return boolean

    test_element = function(self, options, element)
      if
        not element or (options.test and not options.test(element)) or
        (options.tag_name and (element.type ~= mode.tag or not options.tag_name(element.name))) or
        (options.tag_type and not options.tag_type(element.type)) or
        (options.tag_contains and not (element.type == mode.text or element.type == mode.comment or
          element.type == mode.cdata) and not options.tag_contains(element.data))
      then
        return false
      end

      for key, value in pairs(options) do
        if not skipped_options[key] then
          if not element.attributes or not element.attributes[key] or not value(element.attributes[key]) then
            return false
          end
        end
      end

      if options.pseudo_test and not options.pseudo_test(element) then
        return false
      end

      return true
    end,

    -- apply tests and grabs elements in a node tree recursively
    --
    -- @table   options
    -- @table   current_element
    -- @boolean recurse
    -- @number  limit
    -- @return  table

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

    -- get elements by id
    --
    -- @string  id
    -- @table   current_element
    -- @boolean recurse
    -- @return  table

    get_element_by_id = function(self, id, current_element, recurse)
      local result = self:get_elements({id = id}, (current_element or parsed), recurse, 1)
      return #result==1 and result[1] or nil
    end,

    -- get elements by tag name
    --
    -- @string  name
    -- @table   current_element
    -- @boolean recurse
    -- @return  table

    get_elements_by_tag_name = function(self, name, current_element, recurse, limit)
      return self:get_elements({tag_name = name}, (current_element or parsed), recurse, limit)
    end,

    -- get elements by tag type
    --
    -- @string  type
    -- @table   current_element
    -- @boolean recurse
    -- @return  table

    get_elements_by_tag_type = function(self, type, current_element, recurse, limit)
      return self:get_elements({tag_type = type}, (current_element or parsed), recurse, limit)
    end,

    -- get elements by class name
    --
    -- @string  class_name
    -- @table   current_element
    -- @boolean recurse
    -- @return  table

    get_elements_by_class_name = function(self, class_name, current_element, recurse, limit)
      local function test_class(element)
        if not element.attributes or not element.attributes.class then
          return false
        end
        --if not element.attributes.class then return false end
        if find(element.attributes.class, class_name)~=nil then
          return true
        end
      end
      return self:get_elements({test = test_class}, (current_element or parsed), recurse, limit)
    end,

    -- get element parent node
    --
    -- @table   node
    -- @table   current_element
    -- @boolean recurse
    -- @return  table

    get_parent_node = function(self, node, current_element, recurse)
      local recurse_test = function(element)
        if rawequal(element, node.parent) then
          for k=1, #element.children do
            if rawequal(element.children[k], node) then
              return true
            end
          end
        end
        return false
      end
      local result = self:get_elements({test = recurse_test}, (current_element or parsed), recurse, 1)
      return #result==1 and result[1] or nil
    end,

    -- search element by css selector
    --
    -- @string     selector
    -- @table[opt] current_element
    -- @return     table

    query = function(self, selector, current_element)
      local ps, result, old_result, limit = parse_selector(selector), {}, {}, -1

      --self:dump(ps)
      --
      options.relation = ps[2] and (relations[ps[2].relation] or 0) or 0
      cache[ps[1].raw] = cache[ps[1].raw] or generate_and_evaluate_code(ps[1])
      limit            = options.relation > 1 and 2 or -1
      traverse_elements(cache[ps[1].raw], (current_element or parsed), result, options, limit)
      --
      remove(ps, 1)
      --
      for i=1, #ps do
        old_result         = result
        result             = {}
        options.uniqueness = {}
        options.relation   = ps[i+1] and (relations[ps[i+1].relation] or 0) or 0
        cache[ps[i].raw]   = cache[ps[i].raw] or generate_and_evaluate_code(ps[i])
        limit              = options.relation>1 and 2 or -1
        traverse_elements(cache[ps[i].raw], old_result, result, options, limit)
      end
      --
      if #ps > 1 then
        sort(result, set)
      end
      -- reset
      options.uniqueness, options.found, options.relation = {}, false, 0
      --
      return result
    end,

    dump_cache = function(self)
      self:dump(cache)
    end
  }
  this.__index = this
  return setmetatable({}, this)
end

return pumakira