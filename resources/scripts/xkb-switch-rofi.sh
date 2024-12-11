# Array of menu options
options=$(xkb-switch --list)

# Show rofi menu and store the selected option
choice=$(echo "${options}" | rofi -dmenu -i -p "Switch keyboard layout to")

# Switch layout
xkb-switch --set "${choice}"
notify-send "switched layout to ${choice}"
