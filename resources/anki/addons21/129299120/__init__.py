# -*- coding: utf-8 -*-
# Version: 3.1.1
#
# This is an Anki add-on for creating notes by importing media files from a
# user-selected folder. The user is able to map properties of the imported
# file to fields in a note type. For example, a user can map the media file
# to the 'Front' field and the file name to the 'Back' field and generate new
# cards from a folder of media files following this pattern. It can create
# decks recursively and with a hierarchical tag structure.
#
# See GitHub page to report issues or to contribute:
# https://github.com/Iksas/media-import-2

import time

from aqt import editor, mw
from aqt.utils import tooltip
from aqt.qt import *
from anki import notes

try:
    from PyQt6 import QtCore
except ImportError:
    from PyQt5 import QtCore

from . import dialog

# Support the same media types as the Editor
AUDIO = editor.audio
IMAGE = editor.pics

# Possible field mappings
ACTIONS = [
    "",
    "Media",
    "File Name",
    "File Name (full)",
    "Extension",
    "Extension (case-sensitive)",
    "Sequence",
    "Subfolder tags (individual)",
    "Subfolder tag (hierarchical)"
]

# Tooltips for the dropdown menu
# TODO: Don't use string literals as keys
ACTION_TOOLTIPS = {
    "": "Nothing",
    "Media": "The media file\n(image / audio etc.)",
    "File Name": 'The file name without extension\n(e.g. "image.JPG" -> "image")',
    "File Name (full)": 'The file name with extension\n(e.g. "image.JPG" -> "image.JPG")',
    "Extension": 'The lower-case file extension\n(e.g. "image.JPG" -> "jpg")',
    "Extension (case-sensitive)": 'The file extension\n(e.g. "image.JPG" -> "JPG")',
    "Sequence": 'An increasing number\n("0", "1", "2", ...)',
    "Subfolder tags (individual)": 'Creates one tag for each subfolder\n(e.g. "./f1/f2/f3/image.JPG" -> [f1] [f2] [f3])',
    "Subfolder tag (hierarchical)": 'Creates a single tag from the subfolder path\n(e.g. "./f1/f2/f3/image.JPG" -> [f1::f2::f3])',
}

# Note items that we can import into that are not note fields
SPECIAL_FIELDS = ["Tags"]

# Stores the recent path for the file picker
MEDIA_IMPORT_2_RECENT_PATH = os.path.expanduser("~")

def doMediaImport():
    # Raise the main dialog for the add-on and retrieve its result when closed.
    (path, recursive, model, fieldList, ok) = ImportSettingsDialog().getDialogResult()
    if not ok:
        return

    start_time = time.monotonic()
    # Get the MediaImport deck id (auto-created if it doesn't exist)
    did = mw.col.decks.id("MediaImport")

    # Walk through the entire directory tree
    newCount = 0
    failure = False
    fileCount = sum([len(files) for _, _, files in os.walk(path)])
    mw.progress.start(max=fileCount, parent=mw, immediate=True)

    for root, dirs, files in os.walk(path):
        # Don't import subfolders if the user disabled them
        if not recursive:
            dirs[:] = []

        for i, fileName in enumerate(files):
            note = notes.Note(mw.col, model)
            note.note_type()["did"] = did
            mediaName, ext = os.path.splitext(fileName)
            ext = ext[1:].lower()
            filePath = os.path.join(root, fileName)
            if ext is None or ext not in AUDIO + IMAGE:
                # Skip files with no extension and non-media files
                continue
            # Add the file to the media collection and get its name
            internalFileName = mw.col.media.add_file(filePath)
            # Now we populate each field according to the mapping selected
            for field, actionText, special in fieldList:
                if actionText == "":
                    continue
                elif actionText == "Media":
                    if ext in AUDIO:
                        data = "[sound:%s]" % internalFileName
                    elif ext in IMAGE:
                        data = '<img src="%s">' % internalFileName
                    else:
                        continue
                elif actionText == "File Name":
                    data = mediaName
                elif actionText == "File Name (full)":
                    data = fileName
                elif actionText == "Extension":
                    data = ext
                elif actionText == "Extension (case-sensitive)":
                    data = os.path.splitext(mediaName)[1][1:]
                elif actionText == "Sequence":
                    data = str(i)
                elif actionText == "Subfolder tags (individual)":
                    relative_path = os.path.relpath(root, path)
                    data = relative_path.split(os.sep)
                    if "." in data:
                        data.remove(".")
                elif actionText == "Subfolder tag (hierarchical)":
                    relative_path = os.path.relpath(root, path)
                    data = relative_path.split(os.sep)
                    if "." in data:
                        data.remove(".")
                    data = "::".join(data)
                else:
                    continue

                if special and field == "Tags":
                    if type(data) is not list:
                        data = [data]
                    for tag in data:
                        note.tags.append(tag.replace(" ", "_"))
                else:
                    if type(data) is list:
                        data = " ".join(data)
                    note[field] = data

            if not mw.col.addNote(note):
                # No cards were generated - probably bad template.
                # No point trying to import anymore.
                failure = True
                break
            newCount += 1
            mw.progress.update(value=newCount)
        if failure:
            break

    mw.progress.finish()
    end_time = time.monotonic()
    tooltip(f"Created {newCount} cards in {end_time - start_time:.2f} seconds.")
    mw.deckBrowser.refresh()
    if failure:
        showFailureDialog()
    else:
        showCompletionDialog(newCount)


class ImportSettingsDialog(QDialog):
    def __init__(self):
        QDialog.__init__(self, mw)
        self.form = dialog.Ui_Form()
        self.form.setupUi(self)
        self.form.buttonBox.accepted.connect(self.accept)
        self.form.buttonBox.rejected.connect(self.reject)
        self.form.browse.clicked.connect(self.onBrowse)
        self.form.recursiveCheckbox.clicked.connect(self.recursiveCheckboxClicked)
        # The path to the media folder chosen by user
        self.mediaDir = None
        self.recursive = True
        # The number of fields in the note type we are using
        self.fieldCount = 0
        self.populateModelList()
        try:
            self.exec_()
        except AttributeError:
            self.exec()

    def populateModelList(self):
        """Fill in the list of available note types to select from."""
        models = mw.col.models.all()
        for m in models:
            item = QListWidgetItem(m["name"])
            # Put the model in the widget to conveniently fetch later
            item.model = m
            self.form.modelList.addItem(item)
        self.form.modelList.sortItems()
        self.form.modelList.currentRowChanged.connect(self.populateFieldGrid)
        # Triggers a selection so the fields will be populated
        self.form.modelList.setCurrentRow(0)

    def populateFieldGrid(self):
        """Fill in the fieldMapGrid QGridLayout.

        Each row in the grid contains two columns:
        Column 0 = QLabel with name of field
        Column 1 = QComboBox with selection of mappings ("actions")
        The first two fields will default to Media and File Name, so we have
        special cases for rows 0 and 1. The final row is a spacer."""

        self.clearLayout(self.form.fieldMapGrid)

        # Add note fields to grid
        row = 0
        for field in self.form.modelList.currentItem().model["flds"]:
            self.createRow(field["name"], row)
            row += 1

        # Add special fields to grid
        for name in SPECIAL_FIELDS:
            self.createRow(name, row, special=True)
            row += 1

        self.fieldCount = row
        try:
            self.form.fieldMapGrid.addItem(
                QSpacerItem(0, 0, QSizePolicy.Minimum, QSizePolicy.Expanding), row, 0
            )
        except AttributeError:
            self.form.fieldMapGrid.addItem(
                QSpacerItem(
                    0, 0, QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Expanding
                ),
                row,
                0,
            )

    def createRow(self, name, idx, special=False):
        lbl = QLabel(name)
        cmb = QComboBox(None)

        # Add the actions to the dropdown menu, and add tooltips
        for i, actionText in enumerate(ACTIONS):
            # Tags cannot store media
            if name == "Tags" and actionText == "Media":
                continue
            cmb.addItem(actionText)
            if actionText in ACTION_TOOLTIPS:
                cmb.setItemData(i, ACTION_TOOLTIPS[actionText], QtCore.Qt.ItemDataRole.ToolTipRole)

        # piggyback the special flag on QLabel
        lbl.special = special
        self.form.fieldMapGrid.addWidget(lbl, idx, 0)
        self.form.fieldMapGrid.addWidget(cmb, idx, 1)
        if idx == 0:
            cmb.setCurrentIndex(1)
        if idx == 1:
            cmb.setCurrentIndex(2)

    def getDialogResult(self):
        """Return a tuple containing the user-defined settings to follow
        for an import. The tuple contains four items (in order):
         - Path to chosen media folder
         - The model (note type) to use for new notes
         - A dictionary that maps each of the fields in the model to an
           integer index from the ACTIONS list
         - True/False indicating whether the user clicked OK/Cancel"""

        try:
            if self.result() == QDialog.Rejected:
                return None, False, None, None, False
        except AttributeError:
            if self.result() == QDialog.DialogCode.Rejected:
                return None, False, None, None, False

        model = self.form.modelList.currentItem().model
        # Iterate the grid rows to populate the field map
        fieldList = []
        grid = self.form.fieldMapGrid
        for row in range(self.fieldCount):
            # QLabel with field name
            field = grid.itemAtPosition(row, 0).widget().text()
            # Piggybacked special flag
            special = grid.itemAtPosition(row, 0).widget().special
            # QComboBox with currently displayed text
            actionText = grid.itemAtPosition(row, 1).widget().currentText()
            fieldList.append((field, actionText, special))
        return self.mediaDir, self.recursive, model, fieldList, True

    def onBrowse(self):
        """Show the file picker."""
        global MEDIA_IMPORT_2_RECENT_PATH

        path = QFileDialog.getExistingDirectory(mw, caption="Import Folder", directory=MEDIA_IMPORT_2_RECENT_PATH)
        if not path:
            return
        self.mediaDir = path
        # store the parent directory of the recently selected folder
        MEDIA_IMPORT_2_RECENT_PATH = os.path.abspath(os.path.join(path, os.pardir))
        self.form.mediaDir.setText(self.mediaDir)
        self.form.mediaDir.setStyleSheet("")

    def recursiveCheckboxClicked(self, value: bool):
        self.recursive = value

    def accept(self):
        # Show a red warning box if the user tries to import without selecting
        # a media folder.
        if not self.mediaDir:
            self.form.mediaDir.setStyleSheet("border: 1px solid red")
            return
        QDialog.accept(self)

    def clearLayout(self, layout):
        """Convenience method to remove child widgets from a layout."""
        while layout.count():
            child = layout.takeAt(0)
            if child.widget() is not None:
                child.widget().deleteLater()
            elif child.layout() is not None:
                self.clearLayout(child.layout())


def showCompletionDialog(newCount):
    QMessageBox.about(
        mw,
        "Media Import Complete",
        """
<p>
Media import is complete and %s new notes were created.
All generated cards are placed in the <b>MediaImport</b> deck.
<br><br>
Please refer to the introductory videos for instructions on
<a href="https://www.youtube.com/watch?v=DnbKwHEQ1mA">flipping card content</a> or
<a href="https://www.youtube.com/watch?v=F1j1Zx0mXME">modifying the appearance of cards.</a>
</p>"""
        % newCount,
    )


def showFailureDialog():
    QMessageBox.about(
        mw,
        "Media Import Failure",
        """
<p>
Failed to generate cards and no media files were imported. Please ensure the
note type you selected is able to generate cards by using a valid
<a href="https://docs.ankiweb.net/templates/intro.html">card template</a>.
</p>
""",
    )


action = QAction("Media Import 2...", mw)
action.triggered.connect(doMediaImport)  # noqa
mw.form.menuTools.addAction(action)
