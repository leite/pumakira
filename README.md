# Summary

Pumakira is a html parser and [css selector](http://www.w3.org/TR/selectors/) engine in pure [Lua](http://www.lua.org/).

# WTF pumakira means?

Pumakira is a combination of two words in [nheengatu](http://en.wikipedia.org/wiki/Nheengatu_language): *puu* (harvest) and *makira* (web)

# Help and Support

Please fill a issue or help making a clone and the a push request

# License

[BEER-WARE](http://en.wikipedia.org/wiki/Beerware), see source
  
# Basic usage

```lua

    local pumakira = require 'pumakira'  -- require module

    local instance = pumakira.new()      -- create instance

    -- parse html string
    instance:parse([[<html>
                  <head>
                    <title>foo</title>
                  </head>
                  <body>
                    <h1 class='main'>main title</h1>
                    <div class='main'>
                      <p>foo text</p> <p>foo continue...</p> <blockquote id='blocks'>Prof. Lorem Ipsum</blockquote>
                      <ul class='main'>
                        <li>fool</li>
                      </ul>
                    </div>
                    <ul class='main'>
                      <li>one</li> <li>two</li> <li>three</li> <li>four</li>
                    </ul>
                    <!-- foo comment -->
                  </body>
                </html>]])

    instance:get_element_by_id('blocks')                            -- get node by id

    instance:get_elements_by_tag_name('p')                          -- get nodes by name

    instance:get_elements_by_tag_type(instance.mode.comment)        -- get nodes by type

    instance:get_elements_by_class_name('main')                     -- get nodes by class
    
    instance:get_parent_node(instance:get_element_by_id('blocks'))  -- get node's parent

    -- add "pseudo" filter, requires name and filters method and mode - true for iterative or false for after ... default true
    instance:add_filter(                                            
      'ipsum', 
      function(index, value, context_element)
        -- code here
      end,
      true
    )

    instance:query("body>ul[class='main'] li:odd")                  -- query example

``` 

# Test

... in progress

# TODO

+ improve orphaned pseudos after selector parsing
+ improve tree traversing with coroutines?
+ support luvit module style
+ create a test suite
+ create a wiki?

% February 17th, 2013 -03 GMT