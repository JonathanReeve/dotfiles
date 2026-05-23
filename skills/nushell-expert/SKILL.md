---
name: nushell-expert
description: Use Nushell for structured data processing, HTTP requests, and complex text manipulation. Use when tasks involve JSON, CSV, YAML, or API interactions where Nushell's pipelines are more elegant than curl, jq, and sed.
---

# Nushell Expert

Use Nushell (`nu`) to handle structured data and complex pipelines. Nushell treats data as tables and records, making it significantly more robust than traditional string-based processing.

## Core Workflows

### 1. HTTP and JSON Processing
Instead of `curl | jq`, use `http get` and Nushell's built-in filtering.

```bash
nu -c 'http get https://api.github.com/repos/nushell/nushell | select name description stars'
```

### 2. File Format Conversion
Easily convert between JSON, YAML, CSV, and TOML.

```bash
nu -c 'open data.json | save data.yaml'
```

### 3. Data Extraction and Filtering
Filter and transform lists or tables with clean syntax.

```bash
nu -c 'ls | where size > 10mb | sort-by size'
```

### 4. Bulk Operations
Use `each` for iterating over data.

```bash
nu -c 'ls *.jpg | each { |it| print $"Processing ($it.name)..." }'
```

## Tips for Gemini CLI
- Always use `nu -c '<command>'` to execute Nushell commands from the standard shell.
- Use `from json`, `from csv`, etc., when reading raw string output from other tools.
- Nushell's `path` commands (like `path join`, `path exists`) are safer than manual string concatenation.
- When processing high-volume output, use `take` or `first` to keep the context window lean.
