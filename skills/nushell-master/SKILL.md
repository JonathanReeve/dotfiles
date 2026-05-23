---
name: nushell-master
description: Expert-level Nushell usage for advanced data manipulation, system automation, and complex pipelines. Use when you need to perform sophisticated data transformations, handle nested structures, or use Nushell as a powerful scripting language.
---

# Nushell Master

You are a master of Nushell (`nu`). You leverage its structured data model to perform complex tasks with precision and elegance.

## Advanced Data Manipulation

### 1. Working with Tables
Nushell's strength is its table manipulation.

```nushell
# Combine, filter, and sort
ls | append (ls /tmp) | where type == file | sort-by size -r | first 10
```

### 2. Nested Structures
Handle JSON or YAML with deeply nested data.

```nushell
# Accessing nested fields
open config.json | get database.production.host

# Updating nested fields
open config.json | upsert database.production.port 5432 | save config.json --force
```

### 3. Custom Columns and Transformations
Create new data from existing fields.

```nushell
# Calculate file age in days
ls | insert age { |it| (date now) - $it.modified | into int | / 1day }
```

## System Automation

### 1. Robust File Operations
Use Nushell's `path` commands for cross-platform safety.

```nushell
# Safely join paths
let target = ([$env.HOME "Downloads" "backup.tar.gz"] | path join)
```

### 2. Parsing Complex Output
Use `parse` to turn unstructured string output into tables.

```nushell
# Parse custom command output
some-legacy-cmd | parse "{date} {level} {message}" | where level == ERROR
```

## Tips for the Agent

1. **Context Efficiency**: Use `select`, `take`, or `first` to keep your output small. Don't dump huge tables into the context.
2. **Type Safety**: Use `into int`, `into string`, `into datetime` to ensure you're working with the right types.
3. **Error Handling**: Use `try { ... } catch { ... }` for resilient scripts.
4. **Shell Integration**: Remember that you can run external commands by prefixing them with `^` if there's a name collision with a Nushell built-in.
5. **Data Formats**: Nushell can natively `open` and `save` many formats: `json`, `yaml`, `toml`, `csv`, `tsv`, `sqlite`.

## Example: Batch Refactoring
```nushell
ls **/*.js | each { |it| 
    let content = (open $it.name | str replace --all 'old_func' 'new_func')
    $content | save $it.name --force
}
```
