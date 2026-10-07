#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p "python3.withPackages (ps: [ (ps.callPackage ../../scripts/busylib.nix {}) ])" pass

"""Org-Clock BUSY Bar App: Org-mode clocking integration with live timer and hardware button toggle.

    python app.py                        # BUSY Bar over USB (always 10.0.4.20)
    python app.py --host 127.0.0.1:8080  # emulator or a Wi-Fi bar
"""

import sys
import os
import json
import subprocess
import time
import urllib.request
import asyncio

try:
    from busylib import BusyBar, AsyncBusyBar
    from busylib.types import DisplayElements, TextElement, RectangleElement, CountdownElement, BusySnapshot, BusySnapshotNotStarted, BusyBarSettings
    BUSYLIB_AVAILABLE = True
except ImportError as e:
    BUSYLIB_AVAILABLE = False
    sys.stderr.write(f"Warning: busylib import failed: {e}\n")

APP = "org-clock"
CONFIG_PATH = os.path.expanduser("~/.config/busybar.json")
EMACSCLIENT = "/etc/profiles/per-user/jon/bin/emacsclient"


def get_password_from_pass():
    """Retrieves password from pass store via `pass busybar`."""
    env = os.environ.copy()
    if "PASSWORD_STORE_DIR" not in env:
        env["PASSWORD_STORE_DIR"] = os.path.expanduser("~/Dokumentoj/Personal/.password-store")
    try:
        res = subprocess.run(
            ["pass", "busybar"],
            capture_output=True,
            text=True,
            timeout=2,
            env=env
        )
        if res.returncode == 0 and res.stdout.strip():
            return res.stdout.strip()
    except Exception:
        pass
    return ""


def load_config():
    config = {
        "ip": os.environ.get("BUSYBAR_IP", "10.0.4.20"),
        "password": os.environ.get("BUSYBAR_PASSWORD", os.environ.get("BUSYBAR_API_TOKEN", "")),
        "enabled": True
    }
    if "--host" in sys.argv:
        idx = sys.argv.index("--host")
        if idx + 1 < len(sys.argv):
            config["ip"] = sys.argv[idx + 1]

    if os.path.exists(CONFIG_PATH):
        try:
            with open(CONFIG_PATH, "r") as f:
                saved = json.load(f)
                config.update(saved)
        except Exception as e:
            sys.stderr.write(f"Warning loading config {CONFIG_PATH}: {e}\n")
    else:
        try:
            os.makedirs(os.path.dirname(CONFIG_PATH), exist_ok=True)
            with open(CONFIG_PATH, "w") as f:
                json.dump(config, f, indent=2)
        except Exception:
            pass

    if not config.get("password"):
        config["password"] = get_password_from_pass()

    return config


def get_org_clock_info():
    """
    Queries Emacsclient for org clock info via json-encode.
    Returns (heading, start_iso_utc_timestamp) if active, or None if inactive.
    """
    elisp = (
        '(json-encode '
        '(if (org-clock-is-active) '
        '(list :active t '
        ':heading (or org-clock-heading "Task") '
        ':start (if (bound-and-true-p org-clock-start-time) '
        '(format-time-string "%Y-%m-%dT%H:%M:%SZ" org-clock-start-time t) "")) '
        '(list :active nil)))'
    )
    try:
        res = subprocess.run(
            [EMACSCLIENT, "--eval", elisp],
            capture_output=True,
            text=True,
            timeout=3
        )
        if res.returncode == 0:
            out = res.stdout.strip()
            if out.startswith('"') and out.endswith('"'):
                out = json.loads(out)
            data = json.loads(out)
            if isinstance(data, dict) and data.get("active"):
                heading = data.get("heading", "Task")
                start_iso = data.get("start", "")
                return (heading, start_iso)
    except Exception:
        pass
    return None


def get_org_clock_status():
    """Gets formatted status string for CLI / bar widget."""
    info = get_org_clock_info()
    if info:
        return f"{info[0]}"
    return "Protocolu!"


def format_elapsed_time(start_iso):
    """Calculates formatted elapsed count-up time HH:MM:SS / MM:SS from start ISO timestamp."""
    if not start_iso or len(start_iso) < 19:
        return "00:00"
    try:
        iso_clean = start_iso.replace("Z", "+00:00")
        st = datetime.datetime.fromisoformat(iso_clean)
        if st.tzinfo is None:
            st = st.replace(tzinfo=datetime.timezone.utc)
        now = datetime.datetime.now(datetime.timezone.utc)
        elapsed_sec = max(0, int((now - st).total_seconds()))
        hours = elapsed_sec // 3600
        mins = (elapsed_sec % 3600) // 60
        secs = elapsed_sec % 60
        if hours > 0:
            return f"{hours}:{mins:02d}:{secs:02d}"
        else:
            return f"{mins:02d}:{secs:02d}"
    except Exception:
        return "00:00"


def send_to_busybar(clock_info, config):
    """
    Pushes styled display update to Busybar over LAN/USB (72x16 grid).
    - Clocked in: Centered task heading (y=1) + Centered live HH:MM:SS timer (y=10).
    - Not clocked in: Centered "PROTOCOLU!" (y=4) + Red border.
    """
    if not config.get("enabled", True):
        return False, "Disabled"

    ip = config.get("ip", "10.0.4.20")
    password = config.get("password") or config.get("api_key") or None

    if not BUSYLIB_AVAILABLE:
        sys.stderr.write("busylib library is not available.\n")
        return False, "busylib unavailable"

    is_active = clock_info is not None and clock_info != "Protocolu!"

    if not is_active:
        # Inactive state: Red rounded border + Red "PROTOCOLU!" text centered at x=36, y=4
        display_data = DisplayElements(
            application_name="org-clock",
            priority=100,
            elements=[
                RectangleElement(
                    id="t_border",
                    x=0,
                    y=0,
                    width=72,
                    height=16,
                    radius=2,
                    fill="none",
                    border_width=1,
                    border_color="#FF3344FF"
                ),
                TextElement(
                    id="t1",
                    type="text",
                    text="PROTOCOLU!",
                    x=36,
                    y=4,
                    align="center",
                    font="small",
                    color="#FF4455FF"
                )
            ]
        )
    else:
        if isinstance(clock_info, (tuple, list)):
            heading, start_iso = clock_info[0], clock_info[1]
        else:
            heading = str(clock_info)
            start_iso = ""

        # Truncate long task titles with ASCII dots to avoid font glyph box []
        if len(heading) > 18:
            display_heading = heading[:15] + "..."
        else:
            display_heading = heading

        elapsed_str = format_elapsed_time(start_iso)
        time_element = TextElement(
            id="t2",
            type="text",
            text=elapsed_str,
            x=36,
            y=10,
            font="small",
            color="#00FF88FF",
            align="center"
        )

        display_data = DisplayElements(
            application_name="org-clock",
            priority=100,
            elements=[
                TextElement(
                    id="t1",
                    type="text",
                    text=display_heading,
                    x=36,
                    y=1,
                    align="center",
                    font="tiny",
                    color="#00D2FFFF"
                ),
                time_element
            ]
        )

    req_headers = {"Connection": "close"}
    if password:
        req_headers["X-API-Token"] = str(password).strip()

    candidates = []
    if ip:
        candidates.append(ip)
    if "10.0.4.20" not in candidates:
        candidates.append("10.0.4.20")

    for target in candidates:
        try:
            addr = f"http://{target}" if not target.startswith("http") else target
            with BusyBar(addr=addr, token=password, timeout=3.0) as bb:
                bb.display_draw(display_data, headers=req_headers)
                return True, target
        except Exception as e:
            sys.stderr.write(f"DEBUG EXCEPTION on {target}: {type(e).__name__} - {e}\n")
            continue

    return False, "Unreachable"


def adjust_clock_minutes(delta_minutes):
    """Adjusts current org clock start time by +/- delta_minutes in Emacs."""
    elisp = f'(when (org-clock-is-active) (setq org-clock-start-time (time-subtract org-clock-start-time {delta_minutes * -60})) (org-clock-update-time-maybe))'
    try:
        subprocess.run(
            [EMACSCLIENT, "--eval", elisp],
            capture_output=True,
            text=True,
            timeout=3
        )
    except Exception as e:
        sys.stderr.write(f"Error adjusting clock: {e}\n")

    info = get_org_clock_info()
    config = load_config()
    send_to_busybar(info, config)


def toggle_clock():
    """Toggles org clock (stop active clock or resume last clocked task with safety handler)."""
    elisp = '(if (org-clock-is-active) (org-clock-out) (condition-case nil (org-clock-in-last) (error (message "No previous clock"))))'
    try:
        subprocess.run(
            [EMACSCLIENT, "--eval", elisp],
            capture_output=True,
            text=True,
            timeout=5
        )
    except Exception as e:
        sys.stderr.write(f"Error toggling clock: {e}\n")

    info = get_org_clock_info()
    config = load_config()
    send_to_busybar(info, config)
    status_str = get_org_clock_status()
    print(status_str)
    return status_str


async def listen_ws_events(config):
    """Outbound WebSocket client listener for physical hardware button & dial events."""
    ip = config.get("ip", "10.0.4.20")
    password = config.get("password") or config.get("api_key") or None
    candidates = [ip, "10.0.4.20"]

    for target in candidates:
        try:
            addr = f"http://{target}" if not target.startswith("http") else target
            async with AsyncBusyBar(addr=addr, token=password, timeout=3.0) as client:
                async for event in client.stream_status_ws(enable=True):
                    if isinstance(event, dict):
                        for update in event.get("updates", []):
                            if "input" in update and "button_event" in update["input"]:
                                btn_evt = update["input"]["button_event"]
                                action = str(btn_evt.get("action", "")).upper()
                                if action in ("RELEASE", "1", "RELEASE_ACTION"):
                                    toggle_clock()
                            elif "input" in update and "encoder_event" in update["input"]:
                                enc_evt = update["input"]["encoder_event"]
                                steps = enc_evt.get("steps", 1)
                                adjust_clock_minutes(steps)
                            else:
                                evt_str = str(update).lower()
                                if "button" in evt_str and "release" in evt_str:
                                    toggle_clock()
        except Exception:
            await asyncio.sleep(3)


def run_daemon(config):
    """Pure outbound WebSocket client listener — NO HTTP web server, NO listening sockets!"""
    print(f"{APP} app outbound WebSocket listener running...")
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    try:
        loop.run_until_complete(listen_ws_events(config))
    except Exception:
        pass


def main():
    config = load_config()
    cmd = None
    for arg in sys.argv[1:]:
        if not arg.startswith("--"):
            cmd = arg
            break

    if cmd == "update":
        info = get_org_clock_info()
        text_desc = info[0] if info else "Protocolu!"
        success, target_used = send_to_busybar(info, config)
        if success:
            print(f"Update '{text_desc}' sent to Busybar via busylib ({target_used}): Success")
        else:
            print(f"Update '{text_desc}' sent to Busybar via busylib ({target_used}): Unreachable/Disabled")
    elif cmd == "toggle":
        toggle_clock()
    elif cmd == "daemon":
        run_daemon(config)
    elif cmd == "status":
        info = get_org_clock_info()
        status_desc = info[0] if info else "Protocolu!"
        has_pass = bool(config.get("password"))
        print(f"Current Org Clock: {status_desc}")
        print(f"Busybar Configured IP: {config.get('ip')}")
        print(f"Busybar Password Loaded: {has_pass}")
        print(f"Busybar Enabled: {config.get('enabled')}")
        print(f"busylib Installed: {BUSYLIB_AVAILABLE}")
    else:
        info = get_org_clock_info()
        send_to_busybar(info, config)
        print(f"{APP} app running against {config.get('ip')}... (Ctrl-C to stop)")
        try:
            run_daemon(config)
        except KeyboardInterrupt:
            print("\nStopped.")


if __name__ == "__main__":
    main()
