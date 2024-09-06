from typing import List
from pathlib import Path

from aqt.qt import QPushButton
from aqt.utils import openFolder

from .ankiaddonconfig import ConfigManager, ConfigWindow

THEMES_DIR = Path(__file__).parent / "user_files" / "themes"


def open_theme_dir() -> None:
    theme_dir = THEMES_DIR / conf["theme"]
    openFolder(theme_dir)


def get_themes() -> List[str]:
    themes = []
    for child in THEMES_DIR.iterdir():
        if child.is_dir():
            themes.append(child.name)
    return themes


def general_tab(conf_window: ConfigWindow) -> None:
    conf_window.resize(400, 200)

    tab = conf_window.add_tab("General")

    btn_lay = tab.hlayout()
    themes = get_themes()
    btn_lay.dropdown("theme", themes, themes, "Theme:", "Choose a gamification theme")
    btn_lay.stretch()

    btn = QPushButton("Open Theme Folder")
    btn.clicked.connect(lambda _: open_theme_dir())
    btn.setToolTip(
        "You can customize themes by modifying files in the theme directory."
    )
    btn_lay.addWidget(btn)

    tab.hseparator()

    tab.checkbox(
        "sound_effect",
        "Play sound feedback ",
    )
    tab.space(8)
    tab.checkbox("start_effect", "Play feedback on review start ")
    tab.checkbox("review_effect", "Play feedback during review ")
    tab.checkbox("congrats_effect", "Play feedback on completing deck ")

    tab.stretch()


conf = ConfigManager()
conf.use_custom_window()
conf.add_config_tab(general_tab)
