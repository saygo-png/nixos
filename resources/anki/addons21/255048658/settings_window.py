import aqt
from aqt.qt import *
from aqt.utils import showInfo

from .answer_effects import reset_hooks

class Ui_Dialog(object):
  def setupUi(self, Dialog):
    Dialog.setObjectName("DuoMed Settings")
    # Dialog.resize(650, 405)
    Dialog.setWindowTitle("DuoMed Settings")
    Dialog.setModal(False)

    config = aqt.mw.addonManager.getConfig(__name__)
    is_rumble = config["rumble"]
    is_popups = config["popups"]
    is_buttons = config["buttons"]
    is_audio = config["audio"]

    self.masterGridLayout = aqt.QGridLayout(Dialog)
    self.masterGridLayout.setObjectName("masterGridLayout")
    self.masterGridLayout.setColumnMinimumWidth(2, 20)

    # RUMBLE
    self.label_rumble = aqt.QLabel(Dialog)
    self.label_rumble.setText("Enable haptics/rumble")
    self.masterGridLayout.addWidget(self.label_rumble, 0, 0)

    self.check_rumble = aqt.QCheckBox(Dialog)
    if is_rumble:
      self.check_rumble.setChecked(True) # load config and update based on that
    self.masterGridLayout.addWidget(self.check_rumble, 0, 2)

    # AUDIO
    self.label_audio = aqt.QLabel(Dialog)
    self.label_audio.setText("Enable audio feedback")
    self.masterGridLayout.addWidget(self.label_audio, 1, 0)

    self.check_audio = aqt.QCheckBox(Dialog)
    if is_audio:
      self.check_audio.setChecked(True) # load config and update based on that
    self.masterGridLayout.addWidget(self.check_audio, 1, 2)

    self.label_visuals = aqt.QLabel(Dialog)
    self.label_visuals.setText("Enable visual feedback")
    self.masterGridLayout.addWidget(self.label_visuals, 2, 0)

    self.check_visuals = aqt.QCheckBox(Dialog)
    if is_popups:
      self.check_visuals.setChecked(True) # load config and update based on that
    self.masterGridLayout.addWidget(self.check_visuals, 2, 2)

    # BOTTOMBAR BUTTONS
    self.label_buttons = aqt.QLabel(Dialog)
    self.label_buttons.setText("Enable button styling")
    self.masterGridLayout.addWidget(self.label_buttons, 3, 0)

    self.check_buttons = aqt.QCheckBox(Dialog)
    if is_buttons:
      self.check_buttons.setChecked(True) # load config and update based on that
    self.masterGridLayout.addWidget(self.check_buttons, 3, 2)

    # SAVE CONFIG
    self.save_config = aqt.QPushButton(Dialog)
    self.save_config.setText("Save and close")
    self.save_config.clicked.connect(lambda: self.saveConfig(Dialog))
    self.masterGridLayout.addWidget(self.save_config, 4, 0)

  def saveConfig(self, Dialog):
    config = aqt.mw.addonManager.getConfig(__name__)
    audio = self.check_audio.isChecked()
    buttons = self.check_buttons.isChecked()
    visuals = self.check_visuals.isChecked()
    haptic = self.check_rumble.isChecked()
    config["audio"] = audio
    config["buttons"] = buttons
    config["popups"] = visuals
    config["rumble"] = haptic
    aqt.mw.addonManager.writeConfig(__name__, config)
    reset_hooks()
    Dialog.close()

class MyUIDialog(QDialog):
  def __init__(self, parent=None):
    self.parent = parent
    QDialog.__init__(self, parent, aqt.Qt.Window)
    self.dialog = Ui_Dialog()
    self.dialog.setupUi(self)

def DuoMedSettings():
  d = MyUIDialog()
  if d.exec():
    showInfo('dialog closed')
