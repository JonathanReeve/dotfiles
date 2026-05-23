# Mu Query Language Reference

`mu find` uses a powerful query language. Here are the most common fields and operators.

## Fields

- `from:<pattern>` / `f:` - Matches the sender
- `to:<pattern>` / `t:` - Matches the recipient
- `subject:<pattern>` / `s:` - Matches the subject
- `body:<pattern>` / `b:` - Matches the body
- `maildir:<pattern>` / `m:` - Matches the maildir path
- `date:<range>` / `d:` - Matches the date range
- `flag:<flag>` / `g:` - Matches message flags (unread, seen, replied, flagged, attachment, signed, encrypted)
- `size:<range>` / `z:` - Matches the message size
- `priority:<prio>` / `p:` - Matches priority (low, normal, high)
- `tags:<tag>` / `x:` - Matches labels/tags
- `list:<pattern>` / `v:` - Matches mailing list

## Date Ranges

- `date:20250101..20251231` - Absolute range
- `date:2026..` - From 2026 onwards
- `date:..2025` - Up to 2025
- `date:2d..` - Last 2 days
- `date:1w..` - Last week
- `date:20260501..` - Since May 1st, 2026

## Flags

- `flag:unread` - Unread messages
- `flag:attach` - Messages with attachments
- `flag:flagged` - Starred/flagged messages

## Logical Operators

- `AND` (implicit)
- `OR`
- `NOT` (or `!`)
- `()` for grouping

## Examples

- `from:amazon subject:order`
- `flag:unread maildir:/Inbox`
- `date:1w.. NOT from:newsletter`
- `(to:me OR cc:me) flag:attach`
