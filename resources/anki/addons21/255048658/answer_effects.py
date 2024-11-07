import ctypes
import os
from threading import Timer

from aqt import gui_hooks, mw
from aqt.sound import AVPlayer, clearAudioQueue, play

streak = 0
effect_queue = 0
addon_path = os.path.dirname(__file__)
user_files = os.path.join(addon_path, "user_files")
config = mw.addonManager.getConfig(__name__)
is_rumble = config["rumble"]
is_popups = config["popups"]
is_buttons = config["buttons"]
is_audio = config["audio"]
# TODO: how to update these variables when the user changes config settings
audio_wrong = os.path.join(user_files, "wrong.mp3")
audio_correct = os.path.join(user_files, "correct.mp3")

# Define necessary structures
class XINPUT_VIBRATION(ctypes.Structure):
  _fields_ = [("wLeftMotorSpeed", ctypes.c_ushort), ("wRightMotorSpeed", ctypes.c_ushort)]

if os.name == "nt": # windows
  try:
    xinput = ctypes.windll.xinput1_1 # Load Xinput.dll
    # Set up function argument types and return type
    XInputSetState = xinput.XInputSetState
    XInputSetState.argtypes = [ctypes.c_uint, ctypes.POINTER(XINPUT_VIBRATION)]
    XInputSetState.restype = ctypes.c_uint
    rumble_enabled = True
  except FileNotFoundError:
    rumble_enabled = False

else:
  rumble_enabled = False
if is_rumble == 0:
  rumble_enabled = False

def set_vibration(controller, left_motor, right_motor):
  vibration = XINPUT_VIBRATION(int(left_motor*65535), int(right_motor*65535))
  XInputSetState(controller, ctypes.byref(vibration))

def play_sound(sound):
  global is_audio
  if not is_audio:
    return
  clearAudioQueue()
  mw.progress.single_shot(1, lambda: play(sound), False)

def _play_tags(self, tags):
  self._enqueued = tags[:]
  if self.interrupt_current_audio and False:
    self._stop_if_playing()
  self._play_next_if_idle()

AVPlayer.play_tags = _play_tags

def rumble(duration, intensity):
  """duration in seconds, intensity in 0-1"""
  if not rumble_enabled: # windows
    return
  set_vibration(0, intensity, intensity)

  def unfreeze():
    set_vibration(0, 0, 0)

  t = Timer(duration, unfreeze)
  t.start()

def prepare(html, card, context):
  global effect_queue
  if context != "reviewQuestion" or effect_queue == 0:
    return html
  # use jquery to find the body, append to the end of it
  ease = effect_queue
  if ease == 1:
    text = "Next time!"
  elif ease == 2:
    text = "Tough one!"
  elif ease == 3:
    text = "Nice!"
  else:
    text = "Wow!"
  effect_queue = 0
  return html + f"""
    <div class="popup cardfinish{ease}">
      {text}
    </div>
    <style>
@import url(https://db.onlinewebfonts.com/c/14936bb7a4b6575fd2eee80a3ab52cc2?family=Feather+Bold);

.popup {{
  font-family: "Feather Bold", "Varela Round" !important;
  padding: 20px 40px 20px 40px;
  width: 150px;
  border-radius: 16px;
  font-size: 24px;
  position: fixed;
  left: 50%;
  top: 10%;
  animation-name: popup_fadein;
  animation-duration: 900ms;
  animation-fill-mode: forwards;
}}

.cardfinish1 {{
    color: #2e2d2d !important;
    border-bottom: 5px solid #ea2b2b;
    background-color: #ff4b4b;
}}

.cardfinish2 {{
    color: #2e2d2d !important;
    border-bottom: 5px solid #ff9600;
    background-color: #ffb100;
}}

.cardfinish3 {{
    color: #162324b;
    background-color: #58cc02;
    border-bottom: 8px solid #58a700;
}}

.cardfinish4 {{
    color: #2e2d2d !important;
    border-bottom: 5px solid #1899d6;
    background-color: #1cb0f6;
}}

@keyframes popup_fadein {{
  0% {{
    opacity: 0;
    transform: scale(0.7) translateX(-50%) translateY(-5%);
  }}

  10% {{
    opacity: 1;
    transform: scale(1)  translateX(-50%) translateY(0%);
  }}

  30% {{
    opacity: 1;
    transform: scale(0.95)  translateX(-50%) translateY(0%);
  }}

  60% {{
    opacity: 1;
    transform: scale(0.95)  translateX(-50%) translateY(0%);
  }}

  100% {{
    opacity: 0;
    transform: scale(0.95) translateX(-50%) translateY(0%);

  }}
}}
</script>"""

gui_hooks.card_will_show.append(prepare)

def on_reviewer_did_answer_card(reviewer, card, ease):
  global streak, effect_queue, is_popups
  if ease != 1: # if card not rated 'again'
    streak += 1
    print(f"current streak of {streak} cards!")
  else:
    streak = 0
    print("streak lost!")

def on_review_haptics(reviewer, card, ease):
  if ease != 1: # if card not rated 'again'
    rumble(0.350, 0.2)
  else:
    rumble(0.500, 0.6)

def on_review_audio(reviewer, card, ease):
  if ease != 1: # if card not rated 'again'
    play_sound(audio_correct)
  else:
    play_sound(audio_wrong)

def on_review_visuals(reviewer, card, ease):
  global effect_queue
  if is_popups:
    effect_queue = ease

def reset_hooks():
  global config, is_rumble, is_popups, is_buttons, is_audio
  config = mw.addonManager.getConfig(__name__)
  is_rumble = config["rumble"]
  is_popups = config["popups"]
  is_buttons = config["buttons"]
  is_audio = config["audio"]

  gui_hooks.reviewer_did_answer_card.remove(on_review_haptics)
  if is_rumble:
    gui_hooks.reviewer_did_answer_card.append(on_review_haptics)

  gui_hooks.reviewer_did_answer_card.remove(on_review_visuals)
  if is_popups:
    gui_hooks.reviewer_did_answer_card.append(on_review_visuals)

  gui_hooks.reviewer_did_answer_card.remove(on_review_audio)
  if is_audio:
    gui_hooks.reviewer_did_answer_card.append(on_review_audio)

  gui_hooks.reviewer_did_answer_card.append(on_reviewer_did_answer_card)
  print("reset!")

reset_hooks()
