# Array of menu options
options=("Tal Rasha speed" "Disable")

# Show rofi menu and store the selected option
choice=$(printf '%s\n' "${options[@]}" | rofi -dmenu -i -p "Build or disable:")

case $choice in
"Disable")
  d3-autocast --exit
  ;;
"Tal Rasha speed")
  # Frozen nova
  # Autoattack (elemental stacks)
  d3-autocast "Diablo III" q:0.1 r:8
  ;;
esac
