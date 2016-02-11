---
title: Ruby Day Three
date: 2013-11-28
description: Serious Change
tags: ruby
---

On the third day of Ruby, we looked at metaprogramming with open classes,
modules, macros and the `method_missing` method.

<div id="toc"></div>

1. [An improved CSV parser](#an-improved-csv-parser)
  1. [The original code](#the-original-code)
  2. [Specification](#specification)
  3. [Implementation](#implementation)
2. [Thoughts](#thoughts)

### An improved CSV parser

> Mody the CSV application to support an `each` method to return a CsvRow
> object. Use `method_missing` on that `CsvRow` to return the value for the
> column for a given heading.

#### The original code

For the sake of completeness, I’ve included the original application code that
we’re given to use as a starting point.

```ruby
module ActsAsCsv
  def self.included(base)
    base.extend ClassMethods
  end

  module ClassMethods
    def acts_as_csv
      include InstanceMethods
    end
  end

  module InstanceMethods
    def read
      @csv_contents = []
      filename = self.class.to_s.downcase + '.txt'
      file = File.new(filename)
      @headers = file.gets.chomp.split(', ')

      file.each do |row|
        @csv_contents << row.chomp.split(', ')
      end
    end

    attr_accessor :headers, :csv_contents
    def initialize
      read
    end
end

class RubyCsv
  include ActsAsCsv
  acts_as_csv
end
```

#### Specification

We’re given a specification of how the API should work; the API should allow
you to fetch data with `row.one`, `one` being the name of the column to
retrieve each row’s value from.

```ruby
csv = RubyCsv.new
csv.each {|row| puts row.one}
```

#### Implementation

In the wild, I imagine a module you include in a class would be written by
someone else. It’s a library, it’s a collection of helpful things, it’s
modular. For that reason, I added my implementation to the `RubyCsv` class and
left the `ActsAsCsv` module untouched.

The first step was to define an `each` method, which creates new instances of
`CsvRow`. Of course following that, I would have to write a `CsvRow` class.
This class accepts the CSV column headers, and each row. The `method_missing`
method takes a `name`, finds the index of that `name` in the array of column
headers, and returns the value at that index for each row.

Since I only changed the `RubyCsv` class, I’ll only include that code.

```ruby
class RubyCsv
  include ActsAsCsv
  acts_as_csv

  class CsvRow
    def initialize headers, rows
      @headers = headers
      @rows = rows
    end

    def method_missing name
      index = @headers.index name.to_s
      @rows[index]
    end
  end

  def each &block
    @csv_contents.each do |row|
      block.call CsvRow.new @headers, row
    end
  end
end

```

### Thoughts

Metaprogramming is interesting, but I think we only touched very lightly on the
topic and some more challenges would have been welcome. After completing this
challenge, I had a better understanding of how ActiveRecord works in Ruby on
Rails.

Working with Ruby is painless. I like the terse syntax and the flexible list
comprehension. It feels as though I’m worrying less about micromanagement, and
focusing more on writing a simple script that does something.

<a class="previous-post" href="/seven-languages/ruby-day-two">« Ruby: <i>Floating Down from the Sky</i></a>
<a class="next-post" href="/seven-languages/io-day-one">Io: <i>Skipping School, Hanging Out</i> »</a>
