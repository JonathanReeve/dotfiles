---
name: mu-email
description: Search and manage emails from a local maildir using the 'mu' command-line tool. Supports searching, viewing, archiving, deleting, and integrating with Emacs/Org-mode for TODOs and calendar entries.
---

# mu-email

This skill allows you to interface with a local maildir indexed by `mu`.

## Workflows

### 0. Search Recent Emails
Use the `mu.nu search-recent` command to see the latest non-archived messages from your primary inboxes.
```bash
nu ~/Agordoj/scripts/mu.nu search-recent 10
```

### 1. Search for Emails
Use the `mu.nu search` command to find emails matching a query.
```bash
nu ~/Agordoj/scripts/mu.nu search "from:criterion date:2026.."
```

### 2. Read an Email
Use the `mu.nu view` command to read the content of a specific email.
```bash
nu ~/Agordoj/scripts/mu.nu view "/path/to/email/file"
```

### 3. Management Actions
- **Archive:** `nu ~/Agordoj/scripts/mu.nu archive "/path/to/email"` (Moves to /Archive)
- **Delete:** `nu ~/Agordoj/scripts/mu.nu delete "/path/to/email"` (Moves to /Trash)
- **Move:** `nu ~/Agordoj/scripts/mu.nu move "/path/to/email" "/TargetFolder"`

### 4. Emacs/Org-mode Integration
- **Task:** `nu ~/Agordoj/scripts/mu.nu todo "/path/to/email"` (Creates Org Task via org-protocol)
- **Calendar:** `nu ~/Agordoj/scripts/mu.nu calendar "/path/to/email"` (Schedules event and archives email)

## Useful Queries
- `flag:unread` - Find unread messages
- `date:1w..` - Messages from the last week
- `maildir:/Inbox` - Messages in the Inbox
- `flag:attach` - Messages with attachments

## Notes
- Ensure `mu` is installed and the database is indexed (`mu index`).
- Emacs must be running with `server-start` and `org-protocol` configured for Task/Calendar actions.
