import os
import re
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from datetime import datetime
from pathlib import Path

# Paths
ORG_DIR = Path.expanduser(Path("~/Dokumentoj/Org"))
DAILY_DIR = ORG_DIR / "Roam/Daily"
TODO_FILE = ORG_DIR / "Projects/todo.org"

def parse_daily_journals():
    data = []
    if not DAILY_DIR.exists():
        return pd.DataFrame()
        
    for file in DAILY_DIR.glob("*.org"):
        date_str = file.stem
        try:
            date = datetime.strptime(date_str, "%Y-%m-%d")
        except ValueError:
            continue
            
        with open(file, 'r', encoding='utf-8') as f:
            content = f.read()
            
        # Extract properties
        entry = {"date": date}
        properties = ["DRINKS", "MOOD", "EXERCISE", "KETO"]
        for prop in properties:
            match = re.search(f'^:{prop}:\\s*(.*)$', content, re.MULTILINE | re.IGNORECASE)
            if match:
                val = match.group(1).strip()
                # Simple cleaning
                if prop in ["DRINKS", "MOOD", "EXERCISE"]:
                    try:
                        # Extract first number
                        entry[prop.lower()] = float(re.search(r'(\d+\.?\d*)', val).group(1))
                    except (ValueError, AttributeError):
                        entry[prop.lower()] = None
                else:
                    entry[prop.lower()] = val if val else None
        
        if any(v is not None for k, v in entry.items() if k != "date"):
            data.append(entry)
            
    return pd.DataFrame(data).sort_values("date")

def parse_habits():
    # This is a simplified parser for Org habits
    # It looks for DONE states with timestamps in the LOGBOOK or as plain entries
    data = []
    if not TODO_FILE.exists():
        return pd.DataFrame()
        
    with open(TODO_FILE, 'r', encoding='utf-8') as f:
        content = f.read()
        
    # Find habit subtrees
    habits = re.split(r'^\*{3}\s+', content, flags=re.MULTILINE)[1:]
    
    for habit_block in habits:
        lines = habit_block.splitlines()
        title_line = lines[0]
        if ":STYLE: habit" not in habit_block:
            continue
            
        title = re.sub(r'^(TODO|DONE|WAITING)\s+', '', title_line).strip()
        
        # Look for completion dates: [YYYY-MM-DD ...]
        # This matches standard Org state change timestamps
        dates = re.findall(r'- State "DONE".*?\[(\d{4}-\d{2}-\d{2})', habit_block)
        # Also look for clock entries as proxies for completion
        clock_dates = re.findall(r'CLOCK: \[(\d{4}-\d{2}-\d{2})', habit_block)
        
        all_dates = set(dates + clock_dates)
        for d in all_dates:
            data.append({"date": datetime.strptime(d, "%Y-%m-%d"), "habit": title, "completed": 1})
            
    return pd.DataFrame(data)

def visualize():
    print("Parsing Org files...")
    journal_df = parse_daily_journals()
    habits_df = parse_habits()
    
    # 1. Journal Stats (Line Chart)
    if not journal_df.empty:
        print("Generating Journal Stats plot...")
        fig_journal = px.line(journal_df, x="date", y=["drinks", "mood"], 
                             title="Daily Stats Over Time",
                             labels={"value": "Level", "date": "Date"},
                             template="plotly_dark")
        fig_journal.write_html("journal_stats.html")
        print("Saved journal_stats.html")

    # 2. Habit Heatmap
    if not habits_df.empty:
        print("Generating Habit Heatmap...")
        # Create a pivot table for the heatmap
        pivot = habits_df.pivot_table(index="habit", columns="date", values="completed", fill_value=0)
        
        fig_habits = go.Figure(data=go.Heatmap(
            z=pivot.values,
            x=pivot.columns,
            y=pivot.index,
            colorscale='Viridis',
            showscale=False
        ))
        fig_habits.update_layout(
            title="Habit Consistency Matrix",
            xaxis_title="Date",
            yaxis_title="Habit",
            template="plotly_dark"
        )
        fig_habits.write_html("habit_heatmap.html")
        print("Saved habit_heatmap.html")

if __name__ == "__main__":
    visualize()
