import os

from aqt import mw

from . import answer_buttons, settings_window
from .answer_effects import *

addon_path = os.path.dirname(__file__)
user_files = os.path.join(addon_path, "user_files")
mw.addonManager.setConfigAction(__name__, settings_window.DuoMedSettings)
config = mw.addonManager.getConfig(__name__)
print(__name__)
is_rumble = config["rumble"]
is_popups = config["popups"]
is_buttons = config["buttons"]
is_audio = config["audio"]

if is_buttons:
  answer_buttons.enable_bottom_buttons()
else:
  answer_buttons.disable_bottom_buttons()
