-- Lain extension
local lain = require("lain")
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Theme handling library
beautiful = require("beautiful")
-- Notification library
naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
 naughty.notify({
  preset = naughty.config.presets.critical,
  title = "Oops, there were errors during startup!",
  text = awesome.startup_errors,
 })
end
-- Handle runtime errors after startup
do
 local in_error = false
 awesome.connect_signal("debug::error", function (err)
  -- Make sure we don't go into an endless error loop
  if in_error then return end
  in_error = true
  naughty.notify({ preset = naughty.config.presets.critical,
  title = "Oops, an error happened!",
  text = tostring(err) })
  in_error = false
 end)
end

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("/home/samsepi0l/.config/awesome/theme.lua")
terminal = "alacritty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor
-- Dropdown terminal
-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
-- awful.layout.suit.floating,
 awful.layout.suit.tile,
-- awful.layout.suit.tile.left,
-- awful.layout.suit.tile.bottom,
-- awful.layout.suit.tile.top,
-- awful.layout.suit.fair,
-- awful.layout.suit.fair.horizontal,
-- awful.layout.suit.spiral,
-- awful.layout.suit.spiral.dwindle,
-- awful.layout.suit.max,
-- awful.layout.suit.max.fullscreen,
-- awful.layout.suit.magnifier,
-- awful.layout.suit.corner.nw,
-- awful.layout.suit.corner.ne,
-- awful.layout.suit.corner.sw,
-- awful.layout.suit.corner.se,
}


local function set_wallpaper(s)
 -- Wallpaper
 if beautiful.wallpaper then
  local wallpaper = beautiful.wallpaper
  -- If wallpaper is a function, call it with the screen
  if type(wallpaper) == "function" then
   wallpaper = wallpaper(s)
  end
  gears.wallpaper.maximized(wallpaper, s, true)
 end
end

-- re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
-- wallpaper
 set_wallpaper(s)
 -- each screen has its own tag table.
awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])end)

local quake = lain.util.quake {
 app = "alacritty",
 name = "Dropterm",
 argname = "--class Dropterm",
 border = 1,
 width = 0.4,
 height = 0.4,
 horiz = "center",
 vert = "center",
 followscreen = true,
}

globalkeys = gears.table.join(
-- start of custom binds
awful.key({ modkey }, "Delete", function ()
 local focused_client = client.focus
 if focused_client then
  awful.spawn.easy_async("kill --signal SIGKILL " .. focused_client.pid)
 end
end, { description = "Hard kill focused client", group = "client" }),

awful.key({ modkey, }, "z", function () quake:toggle() end,
{description = "dropdown terminal", group = "launcher"}),

awful.key({ modkey,           }, "p",
 function ()
  awful.util.spawn("keyboard-switcher.sh")
 end,
 {description = "Change keyboard layout", group = "launcher"}
),

awful.key({ modkey,           }, "b",
 function ()
  awful.util.spawn("librewolf")
 end,
 {description = "start librewolf", group = "launcher"}
),

awful.key({ modkey,           }, "c",
 function ()
  awful.util.spawn("rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'")
 end,
 {description = "clipboard history", group = "launcher"}
),

awful.key({ modkey,           }, "space",
function ()
 awful.util.spawn("rofi -show run")
end,
{description = "start rofi", group = "launcher"}
),

awful.key({ modkey,           }, "y",
 function ()
  awful.util.spawn("invidious.sh")
 end,
 {description = "start rofi youtube search", group = "launcher"}
),

awful.key({ modkey,           }, "Tab",
 function ()
  awful.util.spawn("rofi -show drun")
 end,
 {description = "start rofi in drun", group = "launcher"}
),

awful.key({ "Mod1", },"Tab",
 function ()
  awful.util.spawn("rofi -show window")
 end,
 {description = "start rofi", group = "launcher"}
),

awful.key({ modkey,           }, "Print",
 function ()
  awful.util.spawn("flameshot_wrapper_ocr_trans")
 end,
 {description = "custom border ocr translate screenshot", group = "launcher"}
),

awful.key({ "Mod1", },"Print",
 function ()
  awful.util.spawn("flameshot_wrapper_ocr")
 end,
 {description = "custom border ocr clipboard screenshot", group = "launcher"}
),

awful.key({}, "Print",
 function ()
  awful.util.spawn("flameshot_wrapper")
 end,
 {description = "custom border screenshot", group = "launcher"}
),

awful.key({modkey,           }, "-",
 function ()
  awful.util.spawn("wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%-")
 end,
 {description = "decrease volume by 5%", group = "launcher"}
),

awful.key({modkey,           }, "=",
 function ()
  awful.util.spawn("wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+")
 end,
 {description = "increase volume by 5%", group = "launcher"}
),

awful.key({ modkey,           }, "a",
function (c) c.sticky = not c.sticky  end,
{description = "toggle sticky", group = "client"}
),
--end of custom binds
-- Non-empty tag browsing
awful.key({ modkey }, "Left", function () lain.util.tag_view_nonempty(-1) end,
{description="view previous non empty", group="tag"}),
awful.key({ modkey }, "Right", function () lain.util.tag_view_nonempty(1) end,
{description="view next non empty", group="tag"}),
awful.key({ modkey, "Control" }, "h", function () lain.util.tag_view_nonempty(-1) end,
{description="view previous non empty", group="tag"}),
awful.key({ modkey, "Control" }, "l", function () lain.util.tag_view_nonempty(1) end,
{description="view next non empty", group="tag"}),
awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
{description="show help", group="awesome"}),
--awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
--{description = "view previous", group = "tag"}),
--awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
--{description = "view next", group = "tag"}),
awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
{description = "go back", group = "tag"}),

awful.key({ modkey,           }, "j",
function ()
 awful.client.focus.byidx( 1)
end,
{description = "focus next by index", group = "client"}
),
awful.key({ modkey,           }, "k",
function ()
 awful.client.focus.byidx(-1)
end,
{description = "focus previous by index", group = "client"}
),
-- Layout manipulation
awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
{description = "swap with next client by index", group = "client"}),
awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
{description = "swap with previous client by index", group = "client"}),
awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
{description = "focus the next screen", group = "screen"}),
awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
{description = "focus the previous screen", group = "screen"}),
awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
{description = "jump to urgent client", group = "client"}),
-- Standard program
awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
{description = "open a terminal", group = "launcher"}),
awful.key({ modkey, "Control" }, "r", awesome.restart,
{description = "reload awesome", group = "awesome"}),

awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
{description = "increase master width factor", group = "layout"}),
awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
{description = "decrease master width factor", group = "layout"}),
awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
{description = "increase the number of master clients", group = "layout"}),
awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
{description = "decrease the number of master clients", group = "layout"}),
awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
{description = "increase the number of columns", group = "layout"}),
awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
{description = "decrease the number of columns", group = "layout"}),
awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
{description = "select next", group = "layout"}),
awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
{description = "select previous", group = "layout"}),

awful.key({ modkey, "Control" }, "n",
function ()
 local c = awful.client.restore()
 -- Focus restored client
 if c then
  c:emit_signal(
  "request::activate", "key.unminimize", {raise = true}
  )
 end
end,
{description = "restore minimized", group = "client"})
)
clientkeys = gears.table.join(
awful.key({ modkey,           }, "f",
function (c)
 c.fullscreen = not c.fullscreen
 c:raise()
end,
{description = "toggle fullscreen", group = "client"}),
awful.key({ modkey }, "q",      function (c) c:kill()                         end,
{description = "close", group = "client"}),
awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
{description = "toggle floating", group = "client"}),
awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
{description = "move to master", group = "client"}),
awful.key({ modkey,           }, "t",
function (c) c.ontop = not c.ontop            end,
{description = "toggle keep on top", group = "client"}),
awful.key({ modkey,           }, "n",
function (c)
 -- The client currently has the input focus, so it cannot be
 -- minimized, since minimized clients can't have the focus.
 c.minimized = true
end ,
{description = "minimize", group = "client"}),
awful.key({ modkey,           }, "m",
function (c)
 c.maximized = not c.maximized
 c:raise()
end ,
{description = "(un)maximize", group = "client"}),
awful.key({ modkey, "Control" }, "m",
function (c)
 c.maximized_vertical = not c.maximized_vertical
 c:raise()
end ,
{description = "(un)maximize vertically", group = "client"}),
awful.key({ modkey, "Shift"   }, "m",
function (c)
 c.maximized_horizontal = not c.maximized_horizontal
 c:raise()
end ,
{description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
 globalkeys = gears.table.join(globalkeys,
 -- View tag only.
 awful.key({ modkey }, "#" .. i + 9,
 function ()
  local screen = awful.screen.focused()
  local tag = screen.tags[i]
  if tag then
   tag:view_only()
  end
 end,
 {description = "view tag #"..i, group = "tag"}),
 -- Toggle tag display.
 awful.key({ modkey, "Control" }, "#" .. i + 9,
 function ()
  local screen = awful.screen.focused()
  local tag = screen.tags[i]
  if tag then
   awful.tag.viewtoggle(tag)
  end
 end,
 {description = "toggle tag #" .. i, group = "tag"}),
 -- Move client to tag.
 awful.key({ modkey, "Shift" }, "#" .. i + 9,
 function ()
  if client.focus then
   local tag = client.focus.screen.tags[i]
   if tag then
    client.focus:move_to_tag(tag)
   end
  end
 end,
 {description = "move focused client to tag #"..i, group = "tag"}),
 -- Toggle tag on focused client.
 awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
 function ()
  if client.focus then
   local tag = client.focus.screen.tags[i]
   if tag then
    client.focus:toggle_tag(tag)
   end
  end
 end,
 {description = "toggle focused client on tag #" .. i, group = "tag"})
 )
end

clientbuttons = gears.table.join(
awful.button({ }, 1, function (c)
 c:emit_signal("request::activate", "mouse_click", {raise = true})
end),
awful.button({ modkey }, 1, function (c)
 c:emit_signal("request::activate", "mouse_click", {raise = true})
 awful.mouse.client.move(c)
end),
awful.button({ modkey }, 3, function (c)
 c:emit_signal("request::activate", "mouse_click", {raise = true})
 awful.mouse.client.resize(c)
end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
 -- All clients will match this rule.
 { rule = { },
  properties = {
   border_width = beautiful.border_width,
   border_color = beautiful.border_normal,
   useless_gap = beautiful.useless_gap,
   focus = awful.client.focus.filter,
   raise = true,
   border_focus = beautiful.border_focus,
   keys = clientkeys, buttons = clientbuttons,
   screen = awful.screen.preferred,
   placement = awful.placement.no_overlap+awful.placement.no_offscreen,
   maximized_vertical   = false,
   maximized_horizontal = false,
   floating = false,
   maximized = false
  }
 },
-- {-- always on top
--  rule_any = {
--   class = {
--    "mpv"
--   },
--  },
--  properties = {
--   floating = true,
--   ontop = true,
--   sticky = true
--  },
--  callback = function(c) c:connect_signal("property::fullscreen", function() if not c.fullscreen then c.ontop = true end end) end
-- },
 {-- Floating clients.
  rule_any = {
   instance = {
    "DTA",-- Firefox addon DownThemAll.
    "copyq",-- Includes session name in class.
    "pinentry",
   },
   class = {
    "Arandr",
    "Blueman-manager",
    "Gpick",
    "Kruler",
    "Krita",
    "MessageWin",-- kalarm.
    "Sxiv",
    "Tor Browser",-- Needs a fixed window size to avoid fingerprinting by screen size.
    "Wpa_gui",
    "veromix",
    "xtightvncviewer"
   },
   -- Note that the name property shown in xprop might be set slightly after creation of the client
   -- and the name shown there might not match defined rules here.
   name = {
    "Event Tester",-- xev.
   },
   role = {
    "AlarmWindow",  -- Thunderbird's calendar.
    "ConfigManager",  -- Thunderbird's about:config.
    "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
   },
   type = {
   "dialog",
   }
  },
  properties = { floating = true }
 },

 { rule = { class = "mpv" },
  properties = { screen = 1, tag = "4" }
 },
 { rule = { class = "KeePassXC" },
  properties = { screen = 1, tag = "8" }
 },
{ rule = { class = "LibreWolf" },
  properties = { screen = 1, tag = "2" }
 },
  { rule = { name = "MarkdownPreview — LibreWolf" },
  properties = { screen = 1, tag = "1" }
 },
 { rule = { class = "qBittorrent" },
  properties = { screen = 1, tag = "9" }
 },
  { rule = { class = "leagueclientux.exe" },
  properties = { screen = 1, tag = "7" }
 },
   { rule = { class = "Anki" },
  properties = { screen = 1, tag = "6" }
 },
    { rule = { class = "league of legends.exe" },
  properties = { screen = 1, tag = "7" }
 },
    { rule = { class = "explorer.exe" },
  properties = { screen = 1, tag = "8", minimized = true }
 },
    { rule = { class = "leagueclient.exe" },
  properties = { screen = 1, tag = "7" }
 },
    { rule = { class = "steam_app_2357570" },
  properties = { screen = 1, tag = "8" }
 },
    { rule = { class = "steam" },
  properties = { screen = 1, tag = "8" }
 },
    { rule = { class = "Steam" },
  properties = { screen = 1, tag = "8" }
 },
}
-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
 -- Set the windows at the slave,
 -- i.e. put it at the end of others instead of setting it master.
  if not awesome.startup then awful.client.setslave(c) end
  if awesome.startup
   and not c.size_hints.user_position
   and not c.size_hints.program_position then
   -- Prevent clients from being unreachable after screen count changes.
   awful.placement.no_offscreen(c)
  end
end)

-- Autostart things (should put most in .xinitrc unless it dont work :D.
-- this straight up does not fucking work if its not both "with_shell" and the util one and idont know why im sick of it, it looks so retartded
awful.spawn.with_shell("remaps")
awful.util.spawn("remaps")

-- change border color on focus
 client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
 client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
--window swallowing
 dont_swallow_classname_list = { "LibreWolf", "librewolf", "nheko", "krita" }
 table_minimize_parent = { "mpv", "ranger" }
 table_cannot_swallow = { "krita", "nheko", "godot", "LibreWolf", "librewolf" }

function is_in_Table(table, element)
 for _, value in pairs(table) do
  if element:match(value) then
   return true
  end
 end
 return false
end

function cannot_be_swallowed(class)
 return not is_in_Table(dont_swallow_classname_list, class)
end

function can_swallow(class)
 return not is_in_Table(table_cannot_swallow, class)
end

function is_parent_minimized(class)
 return is_in_Table(table_minimize_parent, class)
end

function copy_size(c, parent_client)
 if (not c or not parent_client) then
  return
 end
 if (not c.valid or not parent_client.valid) then
  return
 end
 c.x=parent_client.x
 c.y=parent_client.y
 c.width=parent_client.width
 c.height=parent_client.height
end
function check_resize_client(c)
 if(c.child_resize) then
  copy_size(c.child_resize, c)
 end
end

function get_parent_pid(child_ppid, callback)
 local ppid_cmd = string.format("pstree -ps %s", child_ppid)
 awful.spawn.easy_async(ppid_cmd, function(stdout, stderr, reason, exit_code)
  -- primitive error checking
  if stderr and stderr ~= "" then
   callback(stderr)
   return
  end
  local ppid = stdout
  callback(nil, ppid)
 end)
end

client.connect_signal("property::size", check_resize_client)
client.connect_signal("property::position", check_resize_client)
client.connect_signal("manage", function(c)
 local parent_client=awful.client.focus.history.get(c.screen, 1)
 get_parent_pid(c.pid, function(err, ppid)
  if err then
   error(err)
   return
  end
  parent_pid = ppid
  if parent_client and (parent_pid:find("("..parent_client.pid..")")) and can_swallow(c.class) and cannot_be_swallowed(parent_client.class) then
   if is_parent_minimized(c.class) then
    parent_client.child_resize=c
    parent_client.minimized = true
    parent_client.skip_taskbar = not parent_client.skip_taskbar
    c:connect_signal("unmanage", function()
     parent_client.minimized = false
     parent_client.skip_taskbar = not parent_client.skip_taskbar
    end)
    copy_size(c, parent_client)
   else
    parent_client.child_resize=c
    c.floating=true
    copy_size(c, parent_client)
   end
  end
 end)
end)
