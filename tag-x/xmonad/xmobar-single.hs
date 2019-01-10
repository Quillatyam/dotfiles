Config {
    position = TopP 0 0,
    font = "xft:IBMPlexMono-Bold:size=10:bold:antialias=true",
    additionalFonts = ["xft:FontAwesome-11"],
    bgColor = "#111111",
    fgColor = "#f2f2f2",
    lowerOnStart = False,
    overrideRedirect = False,
    allDesktops = True,
    persistent = True,
    commands = [
        Run Date "%a %b %d %H:%M" "date" 10,
	    Run Com "sysctl" ["-n", "hw.acpi.thermal.tz0.temperature"] "temp" 10,
	    --Run Com "sysctl" ["-n", "hw.acpi.battery.life"] "battery" 20,
	    Run Com "sysctl" ["-n", "vm.loadavg"] "load" 10,
	    Run Com "sysctl" ["-n", "hw.acpi.video.lcd0.brightness"] "brightness" 20,
	    --Run Com "mixer" ["-S", "vol"] "vol" 20,
        Run Com "wireless" [] "ssid" 20,
        Run Com "battery" [] "battery" 20,
        Run StdinReader
    ],
    sepChar = "*",
    alignSep = "}{",
    template = "*StdinReader* }{ *ssid* | *brightness* | *load* | *temp* | *battery* | *date* "
}
