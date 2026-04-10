import QtQuick
import Quickshell
import Quickshell.Wayland
import qs.Widgets

Item {
    id: root
    
    // This is a dummy component to inject logic into the global state
    Component.onCompleted: {
        // We can't easily override the base DankPopout.qml without replacing the file
        // but we can try to find a global service to hook into.
    }
}
