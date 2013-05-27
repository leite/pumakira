local pumakira = require './pumakira'
local utils    = require './utils'

local instance, instance2 = pumakira.new(), pumakira.new()

local stream = [[<!-- before --><!-- until it goes -->
                <html>
                  <head>
                    <title>Foo bar</title>
                  </head>
                  <body>
                    <div class='foo bar' id='foo'>hello</div>
                    <!-- comment <h3>hey!</h3> -->
                  </body>
                </html>]]
local stream2 = [[
                  <!doctype html>
                  <html>
                    <head>
                      <title>NNN</title>
                      <script type="text/javascript">
                        var n=''; document.erer=1+1;
                      </script>
                      <style type='text/css'>
                        /* foo bla ble */ 
                        html,body{} 
                      </style>
                    </head>
                    <body>
                      <div class='ninroot'>first div</div>
                      <h2>titulo</h2>
                      <div class='ninroot'>
                        <h2>foo master</h2>
                        <br /><br /><br> 
                        <div>ton solo div</div>
                        <div id='barclays'>
                          <blockquote class='check peligro out' id="blocks">my name is peligro</blockquote>
                          <ul class='senses'>
                            <li>lorem</li>
                            <li>two</li>
                            <li>lorem ipsum</li>
                            <li>four</li>
                            <li>lorem ipsum dolor</li>
                          </ul>
                        </div>
                      </div>
                      <p class='peligro'>whatchout man!</p>
                    </body>
                  </html>]]

-- parse 
instance:parse(stream)
instance2:parse(stream2)

local test = {}
test = instance:get_element_by_id('foo')

print(test.name=='div')
print(test.children[1].data=='hello')

test = instance2:get_elements_by_class_name('peligro')
print(test[1].name=='blockquote')
print(test[1].attributes.id=='blocks')
print(test[2].name=='p')
print(test[2].children[1].data=='whatchout man!')



p(' >> ')
instance2:query('div div[id="barclays"]')
p(' >>> ')
instance2:query('div[id="barclays"] > ul[class]')
p(' >>>> ')
instance2:query("div > div[id='barclays']~ul[class]+li:even")
p(' >>>>> ')
instance2:query("ul[class='senses'] li:contains('lorem'):first")
p(' >>>>>> ')
instance:query("h2#joe[class='show man', rev='foll', alt*='not at all'][width=200]:contains('bla bla bla'):even")

