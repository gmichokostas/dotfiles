hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall.use_syncinstall = true
Install=spoon.SpoonInstall

Install:andUse("Caffeine", { start = true })
Install:andUse("ClipboardTool", { start = true })
Install:andUse("TimeMachineProgress", { start = true })
Install:andUse("WindowHalfsAndThirds", { hotkeys = 'default' })
