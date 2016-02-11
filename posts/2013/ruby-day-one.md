---
title: Ruby Day One
date: 2013-10-24
description: Finding a Nanny
tags: ruby
---

So I worked my through the first set of challenges, and it was mostly a
pleasant experience. Ruby was a convenient language to start with, since of all
seven ‘foreign’ languages, this was most familiar.

Ruby is object-oriented, just like PHP. The syntax is just like Coffeescript
(or rather, Coffeescript is just like Ruby), so no real surprises there either.
Saying that, it’s difficult to judge the character of a language before delving
far deeper than the ubiquitous ‘Hello, World!’ example. By tradition, that’s
where we begin.

> Print the string “Hello, world.”

The keyword for outputting stuff in Ruby is `puts`. We can either output the
string directly, or store it in a variable and use string interpolation, which
works just like in Coffeescript or PHP. Nothing strange here.

```irb
>> puts 'Hello, world.'
Hello, world.
 => nil

>> message = 'Hello, world.'
 => "Hello, world."

>> puts "#{message}"
Hello, world.
 => nil
```

> For the string “Hello, Ruby”, find the index of the word “Ruby”.

A quick search shows that I can just use the `.index` method, which is nice.
The equivalent method in PHP is `strrpos()`, which is a lot less
human-readable. I guess this is why Ruby is said to make programmers happy.
I’ll omit punctuation where possible, as Ruby allows it.

```irb
>> 'Hello, Ruby'.index 'Ruby'
 => 7
```

> Print your name ten times

Ok, so now we’re doing some simple iteration. I searched for looping in Ruby,
and came across [The Bastards Book of
Ruby](http://ruby.bastardsbook.com/chapters/loops/). The loop variation that
instantly caught my eye was `.times` since it’s so easy to read. It’s so
conversational, it seems like pseudo-code.

```irb
>> 10.times {puts 'Jezen Thomas'}
Jezen Thomas
Jezen Thomas
Jezen Thomas
Jezen Thomas
Jezen Thomas
Jezen Thomas
Jezen Thomas
Jezen Thomas
Jezen Thomas
Jezen Thomas
 => 10
```

> Print the string “This is sentence number 1”, where the number 1 changes from
> 1 to 10.

Still looking at the same page, I noticed that to reference the current
iteration number we can usually just make a variable between a pair of pipes. I
could do this with `.times`, but the previous example would start from `0`, and
I don’t want “This is sentence number 0”. Instead, I went with a range and
`.each`, because apparently this is the preferred way of doing things in Ruby.

```irb
>> (1..10).each { |n| puts "This is sentence number #{n}" }
This is sentence number 1
This is sentence number 2
This is sentence number 3
This is sentence number 4
This is sentence number 5
This is sentence number 6
This is sentence number 7
This is sentence number 8
This is sentence number 9
This is sentence number 10
 => 1..10
```

> Run a Ruby program from a file.

Up to this point, I had been using the interactive Ruby shell (`irb`) for these
examples. The answer to this wasn’t so hard to guess; I use vim for editing,
and to open a file directly in vim you do `vim filename.ext`. Similarly, you
run a Ruby script from a file with `ruby filename.rb`. The hint is also there
if you do `ruby --help`. I know; RTFM, right?

```bash
➜ ~ ruby --help
Usage: ruby [switches] [--] [programfile] [arguments]
```

> Bonus: If you’re feeling the need for a little more, write a program that
> picks a random number. Let a player guess the number, telling the player
> whether the guess is too low or too high.

Ok, so Bruce’s hints made this problem fairly easy too. On my first attempt, I
had a working script that would generate a random number and have the user try
to guess it, and then tell the user the number was too low, too high, or
correct. Then it would exit, so to continue playing you would have to start the
script again, which of course generates a new random number. It wasn’t too long
before the penny dropped and I threw a loop in.

```ruby
# Generate a random number between 1 and 10. We add the +1
# because rand starts from zero, which we don’t want.
target = rand(10)+1

guess = 0

puts "Pick a number between 1 and 10."

while guess != target
  # This caught me out at first. No type-conversion is done on user
  # input from gets, so it’s always a string. We convert it to an
  # integer so we can compare it to the integer produced by rand.
  guess = gets.to_i

  if guess < target
    puts "Higher"
  # I think the `elsif` keyword is a bit stupid. It’s not
  # human-readable; it’s just hipster crap.
  elsif guess > target
    puts "Lower"
  end
end

puts "You win!"
```

And that concludes the first day of Ruby. I’m quite excited to dig in to Ruby
some more, and give Ruby on Rails a try. Thus far, most of my web projects have
used PHP on the back end and I personally prefer code without all the brackets,
semicolons, and awkward method names.

<a class="previous-post" href="/seven-languages">« Seven Languages: <i>Project Overview</i></a>
<a class="next-post" href="/seven-languages/ruby-day-two">Ruby: <i>Floating Down from the Sky</i> »</a>
