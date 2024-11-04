# -*- coding: utf-8 -*-
# Version: 3.1.1
#
# This is an Anki add-on for creating notes by importing media files from a
# user-selected directory. The user is able to map properties of the imported
# file to fields in a note type. For example, a user can map the media file
# to the 'Front' field and the file name to the 'Back' field and generate new
# cards from a folder of media files following this pattern. It can create
# decks recursively and with a hierarchical tag structure.
#
# See GitHub page to report issues or to contribute:
# https://github.com/Iksas/media-import-2

try:
    from PyQt6 import QtCore, QtGui, QtWidgets
except ImportError:
    from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_Form(object):
    def setupUi(self, Form):
        Form.setObjectName("Form")
        Form.resize(604, 353)
        self.verticalLayout = QtWidgets.QVBoxLayout(Form)
        self.verticalLayout.setObjectName("verticalLayout")
        self.topGridLayout = QtWidgets.QGridLayout()
        self.topGridLayout.setObjectName("topGridLayout")
        self.label_3 = QtWidgets.QLabel(Form)
        self.label_3.setObjectName("label_3")
        self.topGridLayout.addWidget(self.label_3, 0, 0)
        self.mediaDir = QtWidgets.QLineEdit(Form)
        self.mediaDir.setEnabled(False)
        self.mediaDir.setObjectName("mediaDir")
        self.topGridLayout.addWidget(self.mediaDir, 0, 1)
        self.browse = QtWidgets.QPushButton(Form)
        self.browse.setObjectName("browse")
        self.topGridLayout.addWidget(self.browse, 0, 2)
        self.subDirLabel = QtWidgets.QLabel(Form)
        self.subDirLabel.setToolTip("Include media files located in subfolders")
        self.topGridLayout.addWidget(self.subDirLabel, 1, 0)
        self.recursiveCheckbox = QtWidgets.QCheckBox(Form)
        self.recursiveCheckbox.setChecked(True)
        self.recursiveCheckbox.setObjectName("recursive")
        self.recursiveCheckbox.setToolTip("Include media files located in subfolders")
        self.topGridLayout.addWidget(self.recursiveCheckbox, 1, 1)
        self.verticalLayout.addLayout(self.topGridLayout)

        try:
            spacerItem = QtWidgets.QSpacerItem(0, 10, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Fixed)
        except AttributeError:
            spacerItem = QtWidgets.QSpacerItem(0, 10, QtWidgets.QSizePolicy.Policy.Minimum, QtWidgets.QSizePolicy.Policy.Fixed)

        self.verticalLayout.addItem(spacerItem)
        self.gridLayout = QtWidgets.QGridLayout()
        self.gridLayout.setObjectName("gridLayout")
        self.modelList = QtWidgets.QListWidget(Form)
        self.modelList.setObjectName("modelList")
        self.gridLayout.addWidget(self.modelList, 2, 0, 1, 1)
        self.label_2 = QtWidgets.QLabel(Form)
        self.label_2.setObjectName("label_2")
        self.gridLayout.addWidget(self.label_2, 0, 3, 1, 1)
        self.buttonBox = QtWidgets.QDialogButtonBox(Form)

        try:
            self.buttonBox.setStandardButtons(QtWidgets.QDialogButtonBox.Cancel|QtWidgets.QDialogButtonBox.Ok)
        except AttributeError:
            self.buttonBox.setStandardButtons(QtWidgets.QDialogButtonBox.StandardButton.Cancel|QtWidgets.QDialogButtonBox.StandardButton.Ok)

        self.buttonBox.setObjectName("buttonBox")
        self.gridLayout.addWidget(self.buttonBox, 5, 3, 1, 1)
        self.fieldMapGrid = QtWidgets.QGridLayout()
        self.fieldMapGrid.setObjectName("fieldMapGrid")
        self.gridLayout.addLayout(self.fieldMapGrid, 2, 3, 1, 1)
        self.label = QtWidgets.QLabel(Form)
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 0, 0, 1, 1)
        self.verticalLayout.addLayout(self.gridLayout)

        self.retranslateUi(Form)
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", "Media Import 2"))
        self.label_3.setText(_translate("Form", "Media folder: "))
        self.browse.setText(_translate("Form", "Browse"))
        self.label_2.setText(_translate("Form", "Map fields"))
        self.label.setText(_translate("Form", "Select note type"))
        self.subDirLabel.setText(_translate("Form", "Include subfolders:"))

