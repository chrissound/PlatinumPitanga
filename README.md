# PlatinumPitanga

A to-the-point cli time tracking application with some influence from `git`?

Saves the time entries to `log.json` file in the current directory. 

Here is a quick demo:

![Alt Text](./demo.gif)

## What problems does this solve?

It adds efficiency to logging time - minimal keystrokes.

## What are the limitations?

- It saves the data in a single log.json file - so could be deleted by accident? Make frequent backups!
- No timezone support (everything is done in UTC).

## Instructions

```
pitanga --help
PlatinumPitanga

Usage: pitanga (COMMAND | COMMAND | COMMAND | COMMAND | COMMAND)

Available options:
  -h,--help                Show this help text

Available commands:
  start                    Start a new task
  stop                     Stop the task that is currently in progress
  resume                   Resume the last stopped task
  log                      Log
  export                   Export
```

```
pitangaLog --help 
Usage: pitanga log ([--first ARG] | [--last ARG])
  Log

Available options:
  --first ARG              show first n
  --last ARG               show last n
  -h,--help                Show this help text

```

```
pitanga export --help
Usage: pitanga export [--raw-json] [--group-by-day] [--group-by-day-and-task]
                      [--from ARG]
  Export

Available options:
  --raw-json               Export raw values as JSON
  --group-by-day           Sum aggregate group by day
  --group-by-day-and-task  Sum aggregate group by day and task
  --from ARG               dd mm yy
  -h,--help                Show this help text

```

## Installation

```
nix-build -A platinumpitanga.components.exes.pitanga
```


# Workflow 

** One word: FZF **

These aliases are highly recommended:
```
alias ptl="pitanga log --last 5 | less -R"
pts () {
  echo "$#"
  if [[ "$#" = 0 ]]; then
    pitanga start "$(cat tasks.txt | fzf)" ""
  else
    pitanga start "$(cat tasks.txt | fzf)" "$@"
  fi
};
alias ptsp="pitanga stop"
alias ptr="pitanga resume"
```

I have each "project" in a directory, and within each directory I have a helper `tasks.txt` file.

For example `tasks.txt` might have the contents of:

```
work on abc
investigate xyz
etcxyz
```
