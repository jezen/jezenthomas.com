---
title: Ruby Day Two
date: 2013-10-31
description: Floating Down from the Sky
tags: ruby
---

After reading Day Two of Ruby, Bruce provides another set of challenges that
are much more demanding than Day One. The upswing though is that we’re given
much more opportunity to see how flexible Ruby’s methods are. It didn’t take
long for me to be convinced that I should be doing this instead of PHP.

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [Accessing files](#accessing-files)
  2. [Translating hashes and arrays](#translating-hashes-and-arrays)
  3. [Ruby data structures](#ruby-data-structures)
2. [Things to do](#things-to-do)
  1. [Iterating over chunks](#iterating-over-chunks)
  2. [A tree class](#a-tree-class)
  3. [A simple grep](#a-simple-grep)
3. [Thoughts](#thoughts)

### Things to find

#### Accessing files

> Find out how to access files with and without code blocks. What is the
> benefit of the code block?

A quick search on Stack Overflow showed that accessing a file with a code block
is safer because the file is closed automatically when we’re done working with
it.

```ruby
File.open('file.txt','w') do |file|
  file << 'A spoonful of sugar makes the medicine go down.'
end

file = File.open('file.txt','w')
file << 'A spoonful of sugar makes the medicine go down.'
file.close
```

#### Translating hashes and arrays

> How would you translate a hash to an array? Can you translate arrays to hashes?

Half way through writing my `map` function in `irb`, the penny dropped. Why not
just use `.to_a`? We can then just use the `flatten` method to unpack the
nested arrays.

```ruby
drivers = {
  "Räikkönen" => "Finland",
  "Hamilton" => "UK",
  "Alonso" => "Spain"
}
drivers.to_a.flatten
# ["Räikkönen", "Finland", "Hamilton", "UK", "Alonso", "Spain"]
```

> Can you iterate through a hash?

You sure can. It’s similar to doing something like `foreach ($hash as $key =>
$value):` in PHP.

```ruby
manufacturers = {
  "Porsche" => "Germany",
  "Maserati" => "Italy",
  "Aston Martin" => "United Kingdom"
}
manufacturers.each { |key, value| puts key; puts value }
```

#### Ruby data structures

> You can use Ruby arrays as stacks. What other common data structures do
> arrays support?

I couldn’t tell you all of them, but after briefly browsing Wikipedia’s [list
of data structures](http://en.wikipedia.org/wiki/List_of_data_structures) and
comparing it to the [Ruby Array
documentation](http://www.ruby-doc.org/core-2.0.0/Array.html), I came up with a
few ideas.

```irb
# Sorted array
>> [3, 5, 1, 2, 4].sort
 => [1, 2, 3, 4, 5]

# Set
>> [1, 2, 2, 3, 4].uniq
 => [1, 2, 3, 4]

# Queue
>> [1, 2, 3].push 4
 => [1, 2, 3, 4]
>> ['a', 'b', 'c'].shift
 => "a"
```

You can create matrices with arrays in Ruby too, but you probably wouldn’t
since Ruby provides a [Matrix
class](http://www.ruby-doc.org/stdlib-2.0.0/libdoc/matrix/rdoc/Matrix.html).

### Things to do

#### Iterating over chunks

> Print the contents of an array of sixteen numbers, four numbers at a time,
> using just `each`. Now, do the same with `each_slice` in Enumerable.

```ruby
# I’m storing the numbers as a string, so
# I can control line breaks.
str = ''

# With the numbers 1 through 16, create an array
# and iterate over it. Ruby is awesome.
(1..16).to_a.each do |n|

  # Convert the number to a string, and append it
  # to our external string.
  str << n.to_s

  # After every fourth iteration, output the contents
  # of our external string, and reset it.
  if n % 4 < 1
    puts str
    str = ''
  end

end
```

That wasn’t so bad. I was originally confused about making an array from a
range because I imagined it would look like `[1..16]` or maybe `[(1..16)]`. I
got there in the end. Now onto `each_slice`. I looked the method up in Ruby Doc
and it seems to do exactly what Bruce’s challenge is looking for.

```ruby
(1..16).to_a.each_slice(4) { |n| p n }
```

So, the output is slightly different, but it’s close enough. Bruce didn’t
specify that he specifically wanted either a string or an array output. My
first attempt gave me four strings (since I constructed them myself), and
`each_slice` gave me four arrays. Also, I learned from Ruby Doc that you can
just do `p` instead of `puts`. I doubt that’s the kind of thing I would do in
production code though.

#### A tree class

> The Tree class was interesting, but it did not allow you to specify a new
> tree with a clean interface. Let the initializer accept a nested structure
> with hashes and arrays.

I found this problem particularly challenging, because I wasn’t sure if it was
a good idea to create a new instance of `Tree` every time I wanted go a level
deeper in the hash. I’m using duck-typing to check if `name` can be iterated
over; if it can, then we assume it’s a hash. I have a feeling this code is
quite brittle and won’t work if you just pass in a string and an array, since
I’m checking if children responds to `each`, whereas I should probably check
that it responds to something specific to a hash, like `keys`. Maybe I’ll come
back to this one.

```ruby
class Tree
  attr_accessor :children, :node_name

  def initialize(name, children=[])
    if name.respond_to?(:each)
      root = name.first
      name = root[0]
      children = root[1]
    end

    if children.respond_to?(:each)
      children = children.collect {|child, grandchild|
        Tree.new(child, grandchild)
      }
    end

    @children = children
    @node_name = name
  end

  def visit_all(&block)
    visit &block
    children.each {|child| child.visit_all &block}
  end

  def visit(&block)
    block.call self
  end
end
```

#### A simple grep

> Write a simple grep that will print the lines of a file having any occurences
> of a phrase anywhere in that line. If you want, include line numbers.

This exercise was simple and enjoyable. I took the liberty of turning it into a
super-simple shell script. While researching how to put this together, I
learned that it’s generally frowned upon to use Ruby’s global variables like
`$*`, so instead I use the constant `ARGV` to access parameters.

**Usage** 

1. Make the script executable with `chmod u+x grep.rb`
2. Run the script with `./grep.rb [filepath] [needle]`

```ruby
#!/usr/bin/env ruby

pattern = Regexp.new(ARGV[0])

IO.foreach(ARGV[1]).with_index(1) do |line, index|
  puts "#{index}: #{line}" if line =~ pattern
end

```

### Thoughts

I’m enjoying Ruby’s terse and flexible syntax. Having the facility to do away
with verbosity means I can fit more stuff onto the screen, which is a huge
bonus — having a broader overview of the code makes it easier to spot
duplication and questionable logic. As the saying goes: A cluttered desk is a
clutter mind. A cluttered codebase is… An aneurysm?

<a class="previous-post" href="/seven-languages/ruby-day-one">« Ruby: <i>Finding a Nanny</i></a>
<a class="next-post" href="/seven-languages/ruby-day-three">Ruby: <i>Serious Change</i> »</a>
