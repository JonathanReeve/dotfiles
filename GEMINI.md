# Pending Issues

## DMS Widget Dismissal (Popouts/Modals)
The user wants DMS widgets (Control Center, Clipboard, etc.) to:
1. Toggle when the bar icon is clicked (close if already open).
2. Close automatically when clicking anywhere outside the widget.

### Attempts so far:
1. **IPC Workaround**: Created a script `dms-close-all.sh` to force-close all widgets via IPC and bound it to `Super+Escape`. User rejected this as a hack.
2. **Settings**: Tried setting `dismissOnClickOutside = true` and `clickThrough = false` in `home.nix`. These options either don't exist in the current DMS schema or don't work as expected on Sway/Hyprland.
3. **Nix Patching (Layer Change)**: Set `DMS_POPOUT_LAYER = "overlay"` in session variables to see if focus handling improved. No change.
4. **Source Code Patching**: 
   - Identified that `quickshell/Widgets/DankPopout.qml` emits `backgroundClicked` but has no listener. 
   - Applied a patch in `flake.nix` via `substituteInPlace` to add `onBackgroundClicked: close()`.
   - Patched `contentWindow` anchors in `DankPopout.qml` to ensure it has full-screen dimensions to catch clicks.
   - Despite successful builds, the patched DMS shell still does not dismiss widgets on outside clicks or toggle correctly.

### Status: 
Unsolved. The logic seems to be deeper in how `PopoutManager.requestPopout` handles state or how the Wayland layers are interacting with the MouseArea.
