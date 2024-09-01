local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local gfs = require("gears.filesystem")
local themes_path = "/home/samsepi0l/.config/awesome"
local theme = {}

theme.font = "Courier Prime 15"

theme.bg_normal = "#28282866"
theme.bg_focus = "#7d8618"
theme.bg_urgent = "#ff0000"
theme.bg_minimize = "#b8bb26"
theme.bg_systray = theme.bg_normal

theme.fg_normal = "#fbf1c7"
theme.fg_focus = "#fbf1c7"
theme.fg_urgent = "#fbf1c7"
theme.fg_minimize = "#fbf1c7"

theme.gap_single_client = true
theme.useless_gap = dpi(8)
theme.border_width = dpi(1)
theme.border_normal = "#00000000"
theme.border_marked = "#7d8618"
theme.border_focus = theme.border_marked
--snap
theme.snap_bg = "#7d8618"
--notifications
naughty.config = {
	defaults = {
		ontop = true,
		font = theme.font,
		timeout = 10,
		margin = 20,
		border_width = 1.5,
		font = theme.font,
		fg = beautiful.fg_normal,
		bg = beautiful.bg_normal,
		position = "top_middle",
	},
	padding = 60,
	spacing = 4,
	--height = 200
	-- width = 200
}
theme.wallpaper = "~/.config/wallpaper.png"
return theme
-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
