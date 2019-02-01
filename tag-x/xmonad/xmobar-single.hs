-- Xmobar config is for a single screen setup
Config {
    position = TopP 0 0,
    font = "xft:IBMPlexMono-Bold:size=9:bold:antialias=true",
    additionalFonts = ["xft:FontAwesome-11"],
    bgColor = "#000000",
    fgColor = "#cccccc",
    lowerOnStart = False,
    overrideRedirect = False,
    allDesktops = True,
    persistent = True,
    pickBroadest = True,
    commands = [
        --Run Weather "KPAO" ["-t","<tempF>F <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000,
        Run Date "%a %b %d %H:%M" "date" 10,
	    --Run Com "sysctl" ["-n", "hw.acpi.thermal.tz0.temperature"] "temp" 10,
	    --Run Com "sysctl" ["-n", "vm.loadavg"] "load" 10,
	    --Run Com "sysctl" ["-n", "hw.acpi.video.lcd0.brightness"] "brightness" 20,
	    --Run Com "mixer" ["-S", "vol"] "vol" 20,
        --Run Com "wireless" [] "ssid" 20,
        Run Com "cpu_freq.sh" [] "cpu" 10,
        Run Com "if_ip.sh" [] "ip" 20,
        Run Com "battery" [] "battery" 20,
        Run StdinReader
    ],
    sepChar = "*",
    alignSep = "}{",
    template = "*StdinReader* }{ *cpu* :: *ip* :: *battery* :: *date* "
}
