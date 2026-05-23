---
name: nushell-cookbook
description: Idiomatic Nushell patterns and "Thinking in Nu" principles. Use this to write robust, structured data-driven scripts and pipelines.
---

# Nushell Cookbook

You are an expert in Nushell (`nu`). You prioritize structured data, pipelines, and the "Thinking in Nu" philosophy over traditional string-based shell manipulation.

## Core Principles: "Thinking in Nu"
- **Everything is Data**: Outputs are not strings; they are Tables, Records, Lists, or Primitives.
- **Don't Grep, Where**: Use `where` to filter rows and `get` or `select` to pick columns.
- **Parse Early**: If an external command returns strings, use `parse`, `detect columns`, or `from json` immediately.

## Data Transformation Patterns

### 1. Table Manipulation
```nushell
# Add a calculated column
ls | insert age { |it| (date now) - $it.modified }

# Multi-level sort
ls | sort-by type size

# Grouping and aggregation
open log.json | group-by level | transpose level count | update count { get count | length }
```

### 2. Handling Formats
```nushell
# Convert between formats
open data.yaml | save data.json

# Extract from nested JSON
open package.json | get dependencies.typescript
```

## HTTP & API Patterns

### 1. Modern API Interaction
**NEVER use cURL.** Use `http get`, `http post`, etc.

```nushell
# GET and filter immediately
http get https://api.github.com/repos/nushell/nushell | select name stars forks

# POST with a Nushell record (automatically converted to JSON)
http post -c json https://api.example.com/api {
    user: "jon",
    action: "login"
}
```

### 2. Handling Headers
```nushell
let headers = [Authorization $"Bearer ($env.TOKEN)" Accept "application/json"]
http get --headers $headers https://api.example.com/private
```

## System Automation Patterns

### 1. Batch File Operations
```nushell
# Delete all .tmp files recursively
ls **/*.tmp | each { |it| rm $it.name }

# Bulk rename (replacing spaces with underscores)
ls | where name =~ " " | each { |it| 
    mv $it.name ($it.name | str replace --all " " "_") 
}
```

### 2. Safe Path Handling
```nushell
# Use path join instead of string concatenation
let backup_dir = ([$env.HOME "backups" (date now | format date "%Y-%m-%d")] | path join)
mkdir $backup_dir
```

## Tips for the Agent
- Use `$in` to access the value passed into a closure or command.
- Use `try { ... } catch { ... }` for resilient system scripts.
- Use `table -e` (expand) or `explore` when working interactively to understand data shape.
- Use `debug info` to troubleshoot complex pipelines.
