# PlatinumPitanga

A command line time tracking application with some influence from `git`?

It saves the time entries to `log.json` file in the current directory. For my own workflow, I have each "project" in a directory, and within each directory I have a helper `tasks.txt` file.

For example `tasks.txt` might have the contents of:

```
work on abc
investigate xyz
etcxyz
```

And then using `fzf` I usually do: `pitangaStartTask "$(cat tasks.txt | fzf)" ""` (so I can just 'select' a task to start it).

Here is what a log output looks like:

```
--------------------------------------------
Today: 06:37:33 █ █ █ █ █ █ █ █ █ █ █ █ █  ▂ 
--------------------------------------------
Week:  08:39:16 █  
--------------------------------------------

Blog form edit post nioform                  01:30:42 █ █ █  
Blog                                         02:47:01 █ █ █ █ █  ▄  
Blog - Set dynamic projects                  04:21:32 █ █ █ █ █ █ █ █  ▄  

--------------------------------------------
19:32:07 - 21:56:35
10/03/2019
02:24:27 █ █ █ █  ▆  
Blog - 

18:06:44 - 18:29:18
10/03/2019
00:22:33 ▆  
Blog - 

18:04:43 - 18:04:50
10/03/2019
00:00:07 
Blog form edit post nioform - 
```

## What problems does this solve?

It adds effeciency to logging time - minimal keystrokes. 

## What are the limitations?

It saves the data in a single log.json file - so could be deleted by accident? Make frequent backups!
No timezone support (everything is done in UTC).

## Instructions

Build the Haskell project.

These aliases are highly recommended:
```
alias ptl="pitangaLog | less -R"
pts () { pitangaStartTask "$(cat tasks.txt | fzf)" '' };
alias ptsp="pitangaStopTask"
alias ptr="pitangaResumeTask"
```

Start a new task:
```
pitangaStartTask "description" "second description"
```


Stop a new task:
```
pitangaStopTask
```

See the log
```
pitangaLog
```

Export the log (this is still a bit ugly in output - Work in progress)

```
pitangeExport 
```
