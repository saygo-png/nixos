import json

from aqt import appVersion, gui_hooks, mw
from aqt.reviewer import ReviewerBottomBar

def bottom_bar_styles():
  bottom_bar_css = """
    @import url('https://fonts.googleapis.com/css2?family=Varela+Round&display=swap');

    button {
        padding: 10px !important;
        border-radius: 10px !important;
        width: 100px !important;
        text-align: center !important;
        font-family: "Varela Round" !important;
        font-weight: bold !important;

        cursor: pointer !important;
        font-size: 14px !important;
        transition: all 250ms ease-out !important;
    }

    button:hover {
        background: none;
        border: none;
    }

    .nobold {
    color: white;
    }

    #defease {
        color: #2e2d2d !important;
        border-bottom: 5px solid #58a700 !important;
        background-color: #58cc02 !important;
    }

    #defease:hover {
        color: #2e2d2d !important;
        background-color: #61e002 !important;
    }

    /* the "Again" button */
    button[onclick*="ease1"]:not(#defease) {
        color: #2e2d2d !important;
        border-bottom: 5px solid #ea2b2b;
        background-color: #ff4b4b;
    }

    button[onclick*="ease1"]:not(#defease):hover {
        color: #2e2d2d !important;
        background-color: #f5a4a4;
    }

    /* the "Hard" button */
    button[onclick*="ease2"]:not(#defease) {
        color: #2e2d2d !important;
        border-bottom: 5px solid #ff9600;
        background-color: #ffb100;
    }

    button[onclick*="ease2"]:not(#defease):hover {
        color: #2e2d2d !important;
        background-color: #ffc800;
    }

    /* the "Easy" button */
    button[onclick*="ease3"]:not(#defease),
    button[onclick*="ease4"]:not(#defease) {
        color: #2e2d2d !important;
        border-bottom: 5px solid #1899d6;
        background-color: #1cb0f6;
    }

    button[onclick*="ease3"]:not(#defease):hover,
    button[onclick*="ease4"]:not(#defease):hover {
        color: #2e2d2d !important;
        background-color: #84d8ff;
}
    """
  return bottom_bar_css

def on_webview_will_set_content(web_content, context):
  if not isinstance(context, ReviewerBottomBar):
    return
  web_content.head += f"<style id='duo-styles'>{bottom_bar_styles()}</style>"

def on_theme_did_change():
  mw.bottomWeb.eval(
    "document.querySelector('#styles').textContent = %s;" % json.dumps(bottom_bar_styles())
  )

def enable_bottom_buttons():
  gui_hooks.webview_will_set_content.append(on_webview_will_set_content)
  gui_hooks.theme_did_change.append(on_theme_did_change)

def disable_bottom_buttons():
  gui_hooks.webview_will_set_content.remove(on_webview_will_set_content)
  gui_hooks.theme_did_change.remove(on_theme_did_change)
