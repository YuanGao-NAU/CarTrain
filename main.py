import sys
from PyQt5.QtWidgets import QMainWindow, QApplication
from untitled import Ui_MainWindow
from PyQt5 import QtCore, QtGui, QtWidgets

if __name__ == '__main__':
    app = QApplication(sys.argv)
    MainWindow = QMainWindow()
    ui = Ui_MainWindow()
    ui.setupUi(MainWindow)
    MainWindow.show()
    sys.exit(app.exec_())
    pass
