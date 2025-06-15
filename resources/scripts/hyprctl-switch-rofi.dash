OPTIONS=$(hyprctl devices -j | jq -r '.keyboards[] | select(.main) | .layout')

# Show rofi menu and store the selected option
CHOICE=$(echo "${OPTIONS}" | rofi -dmenu -i -format i -sep ',' -p "Switch keyboard layout to")

LAYOUT_NAME=$(echo "$OPTIONS" | tr ',' '\n' | sed -n "$((CHOICE + 1))p")

hyprctl switchxkblayout current "$CHOICE"
notify-send "switched layout to $LAYOUT_NAME"
