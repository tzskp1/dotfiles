Config { -- appearance
    font = "xft:SourceHanCodeJP:size=10:antialias=true"
   , bgColor = "#2E3440"
   , fgColor = "#8FBCBB"
   , position = TopSize C 100 20
   , border = NoBorder
   , borderColor = "#26a69a"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   -- , template = " %StdinReader% }{ %multicpu%%dynnetwork%%memory%%battery%<fc=#ece391>%RJTT%%date%</fc> " -- note PC
   , template = " %StdinReader% }{ %multicpu% %coretemp% %dynnetwork% %memory% <fc=#ece391>%RJTT% %date%</fc> "
   -- , template = "%battery% | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{ %RJTT% | %date% || %kbd% "

   -- general behavior
   , lowerOnStart =     False   -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = False -- set the Override Redirect flag (Xlib)
   , pickBroadest =     True -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands = 
       -- weather monitor
        [ Run Weather "RJTT" [ "--template", "<skyCondition> <fc=#4682B4><tempC></fc>°C <fc=#4682B4><rh></fc>% <fc=#4682B4><pressure></fc>hPa"
                             ] 36000


        -- cpu core temperature monitor
        , Run CoreTemp       [ "--template" , "<icon=/home/tk/.icons/sm4tik-icon-pack/xbm/temp.xbm/> <core0>.<core1>.<core2>"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 50

        -- network activity monitor (dynamic interface resolution)
        , Run DynNetwork     [ "--template" , "<dev> <icon=/home/tk/.icons/sm4tik-icon-pack/xbm/net_down_03.xbm/><rx>  <icon=/home/tk/.icons/sm4tik-icon-pack/xbm/net_up_03.xbm/><tx>   "
                             , "--Low"      , "1000"       -- units: kB/s
                             , "--High"     , "5000"       -- units: kB/s
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 50
        -- cpu activity monitor
        , Run MultiCpu        [ "-t"       , "<icon=/home/tk/.icons/sm4tik-icon-pack/xbm/cpu.xbm/> <total0>.<total1>.<total2>.<total3>.<total4>.<total5>"
		  	  				  , "-L"       , "40"
							  , "-H"       , "85"
							  , "-m"       , "2"
							  , "--normal" , "#ffffff"
							  , "--high"   , "#f44336"
							  ] 10

		, Run Memory          [ "-t"       , "<icon=/home/tk/.icons/sm4tik-icon-pack/xbm/mem.xbm/> <usedratio>%   "
							  , "-L"       , "40"
							  , "-H"       , "90"
							  , "-m"       , "2"
							  , "--normal" , "#ffffff"
							  , "--high"   , "#f44336"
							  ] 10
		, Run BatteryP        ["CMB1"]
							  [ "-t"       , "<icon=/home/tk/.icons/sm4tik-icon-pack/xbm/bat_full_02.xbm/> <acstatus>  "
							  , "-L"       , "20"
							  , "-H"       , "80"
							  , "--low"    , "#f44336"
							  , "--normal" , "#ffffff"
							  , "--"
							  , "-o" , "<left>% (<timeleft>)"
							  , "-O" , "Charging <left>%"
							  , "-i" , "<left>%"
							  ] 50
	    , Run Date "%a %m/%d %H:%M" "date" 10
		, Run StdinReader
		]
       }
