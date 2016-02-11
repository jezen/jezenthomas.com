---
title: Clearing Process Logs
date: 2014-10-29
excerpt: How I hacked unix’s tail to start fresh whenever I restart a process.
tags: code, ruby
---

I’m writing a big hairy database import from a legacy system and not all of the
records contain valid data. It’s important to me to see when any of the rows
from the old datastore fail model validations so I can either fix my model
logic, or insert corrected data later.

My workflow is basically to run the import, view logged exceptions in a tmux
split, fix them, and repeat. I found it painful to have to clear the log/screen
every time I wanted to rerun the import, so I automated it.

I’ve structured my import into a collection of Rake tasks so each table can be
imported individually (after importing its respective dependencies). I have an
additional task for running *all* of the import tasks, and this additional task
depends on a task I use for truncating the log file.

~~~ruby
task import: [:clear_import_log] + setup do
  Rake.application.tasks.each do |task|
    task.invoke if task.name.starts_with?("import:")
  end
end

task :clear_import_log do
  path = "#{File.dirname(__FILE__)}/../import.log"
  log = File.open(path, "w+")
  log.puts "file truncated"
  log.close
end
~~~

When I open the log file in the `clear_import_log` task, I use the `w+` which
truncates the existing file to zero length, or creates a new file if one
doesn’t exist.

To actually clear the screen when I tail the log, I need to employ a touch more
trickery. I wrote a bash script which clears the screen when it encounters the
line "file truncated", or prints the line to the screen in any other case.

~~~bash
#!/bin/bash

set -e

while read LINE; do
  case "$LINE" in
    "file truncated")
      clear
      ;;
    *)
      echo "$LINE"
      ;;
  esac
done
~~~

In my tmux split, I tail the log file and pipe the output to my bash script, so
now whenever I restart the database import, the screen clears and I see fresh
information.

~~~bash
tail -F -n0 log/import/import.log | script/report.sh
~~~
