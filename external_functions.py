from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import *
from matrix_teco_u4 import matrix_teco_u4
from matrix_teco_u3 import matrix_teco_u3
import os
import shutil
import sys
import subprocess
from nn_clm_cen import nn_clm_cen
from practice_unit5_1 import practice_unit5_1
from copyfiles2dir import copyfiles2dir
from practice_unit5_3 import practice_unit5_3
import time
from plot_unit7 import plot_unit7_e1, plot_unit7_e2
from linecache import getline


    def register(self):
        self.da_indir = ""
        self.da_outdir = ""
        self.simulation_outdir = ""
        self.da_indir = ""
        self.forecasting_outdir = ""
        self.proda_task2_outdir = ""
        self.proda_task4_input1 = ""
        self.proda_task4_input2 = ""
        self.proda_task4_outdir = ""
        self.proda_task1_outdir = ""
        self.proda_task3_outdir = ""
        self.trace_bench_outdir = ""
        self.unit_6_outdir = ""
        self.matrix_teco_outdir = ""
        self.comboBox.currentIndexChanged.connect(self.set_tasks_visible)
        self.matrix.currentIndexChanged.connect(self.set_tab_visible)
        self.pushButton_19.clicked.connect(self.config_trace_bench_outdir)
        self.pushButton_2.clicked.connect(self.config_teco_simulation_parameters)
        self.pushButton_4.clicked.connect(self.config_teco_simulation_namelist)
        self.pushButton_5.clicked.connect(self.config_teco_da_parameters)
        self.pushButton_6.clicked.connect(self.config_teco_da_namelist)
        self.pushButton_3.clicked.connect(self.run_model)
        self.pushButton_7.clicked.connect(self.set_da_folder)
        self.pushButton_11.clicked.connect(self.config_teco_simulation_outdir)
        self.pushButton_12.clicked.connect(self.config_teco_da_outdir)
        self.pushButton_14.clicked.connect(self.config_teco_forecasting_outdir)
        self.pushButton_10.clicked.connect(self.config_proda_task2_outdir)
        self.pushButton_13.clicked.connect(self.config_proda_task4_outdir)
        self.pushButton_8.clicked.connect(self.config_proda_task4_input_nn_task)
        self.pushButton_9.clicked.connect(self.config_proda_task4_input_one_batch_task)
        self.pushButton_15.clicked.connect(self.config_teco_select_da_pars)
        self.pushButton_16.clicked.connect(self.config_proda_task1_outdir)
        self.pushButton_17.clicked.connect(self.config_proda_task3_outdir)
        self.pushButton_18.clicked.connect(self.config_matrix_outdir)
        self.pushButton_20.clicked.connect(self.config_unit_6_outdir)
        self.pushButton_21.clicked.connect(self.open_unit_6_source_code_folder)
        self.pushButton.clicked.connect(self.open_unit_3_4_source_code_folder)
        self.pushButton_23.clicked.connect(self.config_unit_2_outdir)
        self.get_platform()


    def open_unit_6_source_code_folder(self):
        if self.platform == "win32":
            subprocess.Popen(['explorer', ".\\Source_code\\unit_6"])
        else:
            subprocess.Popen(["open", "./Source_code/unit_6/"])


    def open_unit_3_4_source_code_folder(self):
        if self.platform == "win32":
            subprocess.Popen(['explorer', ".\\Source_code\\TECO_matrix"])
        else:
            subprocess.Popen(["open", "./Source_code/TECO_matrix/"])


    def set_tasks_visible(self):
        if self.comboBox.currentText() == "Select a unit":
            self.matrix.setVisible(True)
            self.matrix.clear()
        
        elif self.comboBox.currentText() == "Unit 2":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "2"])
            self.matrix.setCurrentIndex(0)

        elif self.comboBox.currentText() == "Unit 3":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "1", "2"])
            self.matrix.setCurrentIndex(0)

        elif self.comboBox.currentText() == "Unit 4":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "1", "2"])
            self.matrix.setCurrentIndex(0)

        elif self.comboBox.currentText() == "Unit 5":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "1", "2", "3"])
            self.matrix.setCurrentIndex(0)

        elif self.comboBox.currentText() == "Unit 6":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "1", "2"])
            self.matrix.setCurrentIndex(0)

        elif self.comboBox.currentText() == "Unit 7":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "1", "2"])
            self.matrix.setCurrentIndex(0)

        elif self.comboBox.currentText() == "Unit 8":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "1", "2", "3", "4"])
            self.matrix.setCurrentIndex(0)
        
        elif self.comboBox.currentText() == "Unit 9":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "1", "2", "3"])
            self.matrix.setCurrentIndex(0)
        
        elif self.comboBox.currentText() == "Unit 10":
            self.matrix.setVisible(True)
            self.matrix.clear()
            self.matrix.addItems(["Select an exercise", "1", "2", "3", "4"])
            self.matrix.setCurrentIndex(0)

    def set_tab_visible(self):
        self.times.setVisible(False)
        self.label_3.setVisible(False)
        self.pushButton_5.setVisible(True)
        self.pushButton_6.setVisible(True)
        self.pushButton_15.setVisible(True)

        if self.comboBox.currentText() == "Unit 2":
                self.tabWidget.setCurrentIndex(0)

        if self.comboBox.currentText() == "Unit 3":
            if self.matrix.currentText() == "1":
                self.tabWidget.setCurrentIndex(1)
                self.comboBox_4.setVisible(True)
                self.comboBox_19.setVisible(False)
                self.comboBox_5.setVisible(False)
                self.comboBox_14.setVisible(False)
            if self.matrix.currentText() == "2":
                self.tabWidget.setCurrentIndex(1)
                self.comboBox_4.setVisible(False)
                self.comboBox_19.setVisible(True)
                self.comboBox_5.setVisible(False)
                self.comboBox_14.setVisible(False)

        if self.comboBox.currentText() == "Unit 4":
            if self.matrix.currentText() == "1":
                self.tabWidget.setCurrentIndex(1)
                self.comboBox_4.setVisible(False)
                self.comboBox_19.setVisible(False)
                self.comboBox_5.setVisible(True)
                self.comboBox_14.setVisible(False)
            if self.matrix.currentText() == "2":
                self.tabWidget.setCurrentIndex(1)
                self.comboBox_4.setVisible(False)
                self.comboBox_19.setVisible(False)
                self.comboBox_5.setVisible(False)
                self.comboBox_14.setVisible(True)

        elif self.comboBox.currentText() == "Unit 5":
            self.comboBox_16.setVisible(False)
            self.label_30.setVisible(False)
            if self.matrix.currentText() == "1":
                self.tabWidget.setCurrentIndex(2)
            elif self.matrix.currentText() == "2":
                self.tabWidget.setCurrentIndex(2)
                self.comboBox_16.setVisible(True)
                self.label_30.setVisible(True)
            elif self.matrix.currentText() == "3":
                self.tabWidget.setCurrentIndex(2)
            elif self.matrix.currentText() == "4":
                self.tabWidget.setCurrentIndex(2)
        
        elif self.comboBox.currentText() == "Unit 6":
            if self.matrix.currentText() == "1":
                self.tabWidget.setCurrentIndex(3)
                self.comboBox_18.setVisible(False)
                self.comboBox_17.setVisible(True)
            elif self.matrix.currentText() == "2":
                self.tabWidget.setCurrentIndex(3)
                self.comboBox_17.setVisible(False)
                self.comboBox_18.setVisible(True)      

        elif self.comboBox.currentText() == "Unit 7":
            if self.matrix.currentText() == "1":
                self.tabWidget.setCurrentIndex(4)
            elif self.matrix.currentText() == "2":
                self.tabWidget.setCurrentIndex(4)
        
        elif self.comboBox.currentText() == "Unit 8":
            if self.matrix.currentText() == "1":
                self.tabWidget.setCurrentIndex(5)
            self.pushButton_5.setVisible(False)
            self.pushButton_6.setVisible(False)
            self.pushButton_15.setVisible(False)

        elif self.comboBox.currentText() == "Unit 9":
            if self.matrix.currentText() == "1":
                self.tabWidget.setCurrentIndex(5)
            elif self.matrix.currentText() == "2":
                self.times.setVisible(True)
                self.label_3.setVisible(True)
                self.tabWidget.setCurrentIndex(6)
            elif self.matrix.currentText() == "3":
                self.tabWidget.setCurrentIndex(6)

        elif self.comboBox.currentText() == "Unit 10":
            if self.matrix.currentText() == "1":
                self.tabWidget.setCurrentIndex(7)
            if self.matrix.currentText() == "2":
                self.tabWidget.setCurrentIndex(8)
            if self.matrix.currentText() == "3":
                self.tabWidget.setCurrentIndex(9)
            if self.matrix.currentText() == "4":
                self.tabWidget.setCurrentIndex(10)

    def config_matrix_unit_3(self):
        if self.matrix.currentText() == "1":
            return self.comboBox_4.currentText()
        else:
            return self.comboBox_19.currentText()


    def config_matrix_unit_4(self):
        if self.matrix.currentText() == "1":
            return self.comboBox_5.currentText()
        else:
            return self.comboBox_14.currentText()

    def config_unit_2_outdir(self):
        out_dir = QFileDialog.getExistingDirectory()
        if out_dir:
            self.unit_2_outdir = out_dir
            self.lineEdit_14.setText(out_dir)

    def config_matrix_outdir(self):
        out_dir = QFileDialog.getExistingDirectory()
        if out_dir:
            self.matrix_teco_outdir = out_dir
            self.lineEdit_11.setText(out_dir)


    def config_unit_6_outdir(self):
        out_dir = QFileDialog.getExistingDirectory()
        if out_dir:
            self.unit_6_outdir = out_dir
            self.lineEdit_13.setText(out_dir)


    def config_teco_simulation_parameters(self):
        if self.simulation_outdir:
            if self.platform == "win32":
                os.startfile(self.simulation_outdir + "/SPRUCE_pars.txt")
            else:
                subprocess.call(["open", self.simulation_outdir + "/SPRUCE_pars.txt"])
        else:
            msg = QMessageBox()
            msg.setIcon(QMessageBox.Critical)
            msg.setText("Please select a folder!")
            msg.setWindowTitle("Error")
            msg.exec_()


    def config_teco_simulation_namelist(self):
        if self.simulation_outdir:
            if self.platform == "win32":
                os.startfile(self.simulation_outdir + "/teco_workshop_simu.txt")
            else:
                subprocess.call(["open", self.simulation_outdir + "/teco_workshop_simu.txt"])
        else:
            msg = QMessageBox()
            msg.setIcon(QMessageBox.Critical)
            msg.setText("Please select a folder!")
            msg.setWindowTitle("Error")
            msg.exec_()


    def config_teco_da_parameters(self):
        if self.da_outdir:
            if self.platform == "win32":
                os.startfile(self.da_outdir + "/SPRUCE_pars.txt")
            else:
                subprocess.call(["open", self.da_outdir + "/SPRUCE_pars.txt"])
        else:
            msg = QMessageBox()
            msg.setIcon(QMessageBox.Critical)
            msg.setText("Please select a folder!")
            msg.setWindowTitle("Error")
            msg.exec_()


    def config_teco_select_da_pars(self):
        if self.da_outdir:
            if self.platform == "win32":
                os.startfile(self.da_outdir + "/SPRUCE_da_pars.txt")
            else:
                subprocess.call(["open", self.da_outdir + "/SPRUCE_da_pars.txt"])
        else:
            msg = QMessageBox()
            msg.setIcon(QMessageBox.Critical)
            msg.setText("Please select a folder!")
            msg.setWindowTitle("Error")
            msg.exec_()


    def config_teco_da_namelist(self):
        if self.da_outdir:
            os.startfile(self.da_outdir + "/teco_workshop_da_no_obs.txt")
        else:
            msg = QMessageBox()
            msg.setIcon(QMessageBox.Critical)
            msg.setText("Please select a folder!")
            msg.setWindowTitle("Error")
            msg.exec_()


    def config_teco_simulation_outdir(self):
        out_dir = QFileDialog.getExistingDirectory()
        if out_dir:
            self.simulation_outdir = out_dir
            shutil.copyfile("./Source_code/TECO_2.3/input/SPRUCE_pars.txt", self.simulation_outdir + "/SPRUCE_pars.txt")
            shutil.copyfile("./Source_code/TECO_2.3/input/SPRUCE_da_pars.txt",
                            self.simulation_outdir + "/SPRUCE_da_pars.txt")
            shutil.copyfile("./Source_code/TECO_2.3/workshop_nml/teco_workshop_simu.txt",
                            self.simulation_outdir + "/teco_workshop_simu.txt")

            self.lineEdit_4.setText(out_dir)


    def config_teco_da_outdir(self):
        out_dir = QFileDialog.getExistingDirectory()
        if out_dir:
            self.da_outdir = out_dir
            self.lineEdit_5.setText(out_dir)
            if self.comboBox.currentText() != "Unit 8":
                shutil.copyfile("./Source_code/TECO_2.3/input/SPRUCE_pars.txt", self.da_outdir + "/SPRUCE_pars.txt")
                shutil.copyfile("./Source_code/TECO_2.3/input/SPRUCE_da_pars.txt",
                                self.da_outdir + "/SPRUCE_da_pars.txt")
                shutil.copyfile("./Source_code/TECO_2.3/workshop_nml/teco_workshop_da_no_obs.txt",
                                self.da_outdir + "/teco_workshop_da_no_obs.txt")


    def config_teco_forecasting_outdir(self):
        out_dir = QFileDialog.getExistingDirectory()
        if out_dir:
            self.forecasting_outdir = out_dir
            self.lineEdit_8.setText(out_dir)


    def config_proda_task2_outdir(self):
        out_dir = QFileDialog.getExistingDirectory()
        if out_dir:
            self.proda_task2_outdir = out_dir
            self.lineEdit_6.setText(out_dir)


    def config_proda_task4_outdir(self):
        out_dir = QFileDialog.getExistingDirectory()
        if out_dir:
            self.proda_task4_outdir = out_dir
            self.lineEdit_7.setText(out_dir)


    def config_proda_task4_input_nn_task(self):
        in_dir = QFileDialog.getExistingDirectory()
        if in_dir:
            self.proda_task4_input1 = in_dir
            self.lineEdit_2.setText(in_dir)


    def config_proda_task4_input_one_batch_task(self):
        in_dir = QFileDialog.getExistingDirectory()
        if in_dir:
            self.proda_task4_input2 = in_dir
            self.lineEdit_3.setText(in_dir)


    def set_da_folder(self):
        indir = QFileDialog.getExistingDirectory()
        if indir:
            self.lineEdit.setText(indir)
            self.da_indir = indir


    def config_proda_task1_outdir(self):
        outdir = QFileDialog.getExistingDirectory()
        if outdir:
            self.lineEdit_9.setText(outdir)
        self.proda_task1_outdir = outdir


    def config_proda_task3_outdir(self):
        outdir = QFileDialog.getExistingDirectory()
        if outdir:
            self.lineEdit_10.setText(outdir)
        self.proda_task3_outdir = outdir

    def config_trace_bench_outdir(self):
        outdir = QFileDialog.getExistingDirectory()
        if outdir:
            self.lineEdit_12.setText(outdir)
        self.trace_bench_outdir = outdir

    def open_unit_2_ex_2(self):
        if self.platform == "win32":
            subprocess.Popen(['Notepad', ".\\Source_code\\unit_2\\test_p2.py"])
        else:
            subprocess.Popen(["open", "./Source_code/unit_6/test_p2.py"])

    def open_unit_2_ex_2_solution(self):
        if self.platform == "win32":
            subprocess.Popen(['Notepad', ".\\Source_code\\unit_2\\test_p2_key.py"])
        else:
            subprocess.Popen(["open", "./Source_code/unit_6/test_p2_key.py"])

    def run_model(self):
        os.chdir(os.getcwd())
        msg = QMessageBox()
        msg.setIcon(QMessageBox.Information)
        msg.setText("Task submitted!")
        msg.setWindowTitle("Info")
        msg.exec_()
        if self.comboBox.currentText() == "Unit 2":
            res = 1
            pass

        elif self.comboBox.currentText() == "Unit 3":
            res = 1
            if self.matrix_teco_outdir:
                if self.matrix.currentText() == "1":
                    res = matrix_teco_u3(self.config_matrix_unit_3(), self.matrix_teco_outdir)
                elif self.matrix.currentText() == "2":
                    res = matrix_teco_u4(self.config_matrix_unit_4(), self.matrix_teco_outdir)
                if res == 0:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Information)
                    msg.setText("Finished!")
                    msg.setWindowTitle("Info")
                    msg.exec_()
            else:
                msg = QMessageBox()
                msg.setIcon(QMessageBox.Critical)
                msg.setText("Please select a folder!")
                msg.setWindowTitle("Error")
                msg.exec_()

        elif self.comboBox.currentText() == "Unit 4":
            res = 1
            if self.matrix_teco_outdir:
                if self.matrix.currentText() == "1":
                    res = matrix_teco_u3(self.config_matrix_unit_3(), self.matrix_teco_outdir)
                elif self.matrix.currentText() == "2":
                    res = matrix_teco_u4(self.config_matrix_unit_4(), self.matrix_teco_outdir)
                if res == 0:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Information)
                    msg.setText("Finished!")
                    msg.setWindowTitle("Info")
                    msg.exec_()
            else:
                msg = QMessageBox()
                msg.setIcon(QMessageBox.Critical)
                msg.setText("Please select a folder!")
                msg.setWindowTitle("Error")
                msg.exec_()

        elif self.comboBox.currentText() == "Unit 5":
            if self.trace_bench_outdir:
                res = 1
                if self.matrix.currentText() == "1":
                    res = practice_unit5_1(self.trace_bench_outdir)
                elif self.matrix.currentText() == "2":
                    time.sleep(10)
                    if self.comboBox_16.currentText() == "Spatial":
                        source_folder = "./Source_code/traceability/practice_2/results/output/spatial/"
                        res = copyfiles2dir(source_folder, self.trace_bench_outdir)
                    else:
                        source_folder = "./Source_code/traceability/practice_2/results/output/temporal/"
                        res = copyfiles2dir(source_folder, self.trace_bench_outdir)
                elif self.matrix.currentText() == "3":
                    res = practice_unit5_3(self.trace_bench_outdir)
                elif self.matrix.currentText() == "4":
                    res = 1
                    pass

                if res == 0:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Information)
                    msg.setText("Finished!")
                    msg.setWindowTitle("Info")
                    msg.exec_()

                if res == 1:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Information)
                    msg.setText("Not implemented!")
                    msg.setWindowTitle("Info")
                    msg.exec_()
            else:
                msg = QMessageBox()
                msg.setIcon(QMessageBox.Critical)
                msg.setText("Please select a folder!")
                msg.setWindowTitle("Error")
                msg.exec_()

        elif self.comboBox.currentText() == "Unit 6":
            if self.unit_6_outdir != "":
                if os.path.exists(self.unit_6_outdir + "/practice_1_ambient") == False:
                    os.mkdir(self.unit_6_outdir + "/practice_1_ambient/")
                    os.mkdir(self.unit_6_outdir + "/practice_1_ambient/figures/")
                if os.path.exists(self.unit_6_outdir + "/practice_1_elevated") == False:
                    os.mkdir(self.unit_6_outdir + "/practice_1_elevated/")
                    os.mkdir(self.unit_6_outdir + "/practice_1_elevated/figures")
                if os.path.exists(self.unit_6_outdir + "/practice_2_ambient") == False:
                    os.mkdir(self.unit_6_outdir + "/practice_2_ambient/")
                    os.mkdir(self.unit_6_outdir + "/practice_2_ambient/figures/")
                res = 1
                model_folder = "./Source_code/unit_6/"
                cmd = ""
                cmd_r = ""
                if self.matrix.currentText() == "1":
                    if self.comboBox_17.currentText() == "ambient":
                        if self.platform == "win32":
                            cmd = "python Source_code/unit_6/Probabilistic_inversion.py 1 ParamRange.txt {1}{0}{1}".format(
                                self.unit_6_outdir + "/practice_1_ambient/", "\"")
                        else:
                            cmd = "python3 Source_code/unit_6/Probabilistic_inversion.py 1 ParamRange.txt {1}{0}{1}".format(
                                self.unit_6_outdir + "/practice_1_ambient/", "\"")
                        cmd_r = "Rscript Source_code/unit_6/FigurePlot.R 1 ParamRange.txt {1}{0}{1}".format(
                            self.unit_6_outdir + "/practice_1_ambient/", "\"")
                        print(self.unit_6_outdir)
                    else:
                        if self.platform == "win32":
                            cmd = "python Source_code/unit_6/Probabilistic_inversion.py 2 ParamRange.txt {1}{0}{1}".format(
                                self.unit_6_outdir + "/practice_1_elevated/", "\"")
                        else:
                            cmd = "python3 Source_code/unit_6/Probabilistic_inversion.py 2 ParamRange.txt {1}{0}{1}".format(
                                self.unit_6_outdir + "/practice_1_elevated/", "\"")
                        cmd_r = "Rscript Source_code/unit_6/FigurePlot.R 2 ParamRange.txt {1}{0}{1}".format(
                            self.unit_6_outdir + "/practice_1_elevated/", "\"")

                elif self.matrix.currentText() == "2":
                    if self.platform == "win32":
                        cmd = "python Source_code/unit_6/Probabilistic_inversion.py 1 newParamRange.txt {1}{0}{1}".format(
                            self.unit_6_outdir + "/practice_2_ambient/", "\"")
                    else:
                        cmd = "python3 Source_code/unit_6/Probabilistic_inversion.py 1 newParamRange.txt {1}{0}{1}".format(
                            self.unit_6_outdir + "/practice_2_ambient/", "\"")
                    cmd_r = "Rscript Source_code/unit_6/FigurePlot.R 1 newParamRange.txt {1}{0}{1}".format(
                        self.unit_6_outdir + "/practice_2_ambient/", "\"")

                res = os.system(cmd)
                res = os.system(cmd_r)

                if res == 0:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Information)
                    msg.setText("Finished!")
                    msg.setWindowTitle("Info")
                    msg.exec_()
            else:
                msg = QMessageBox()
                msg.setIcon(QMessageBox.Critical)
                msg.setText("Please select a folder!")
                msg.setWindowTitle("Error")
                msg.exec_()

        elif self.comboBox.currentText() == "Unit 7":
            model_folder = "./Source_code/TECO_2.3/"
            if self.matrix.currentText() == "1":
                if self.simulation_outdir:

                    cmd_pars = "{0}/teco_workshop_{1}.txt{7} {0}/SPRUCE_pars.txt{7} {0}/SPRUCE_da_pars.txt{7} " \
                               "{2}/input/SPRUCE_forcing_2011_{3}.txt {0}{7} " \
                               "{2}/input/SPRUCE_cflux.txt {2}/input/SPRUCE_cpool.txt {2}/input/SPRUCE_ch4.txt {4} {5} {6}".format(
                        "\"" + self.simulation_outdir, "simu", model_folder, "2016", "0", "380", "100", "\""
                    )

                    if self.platform == "win32":
                        res = os.system(".\Source_code\TECO_2.3\TECO_2.3.exe {0}".format(cmd_pars))
                    else:
                        res = os.system(model_folder + "TECO_2.3.exe {0}".format(cmd_pars))

                    res = plot_unit7_e1("./Source_code/TECO_2.3/input/", self.simulation_outdir)

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()

                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()
            elif self.matrix.currentText() == "2":
                if self.da_outdir:
                    if os.path.exists(self.da_outdir + "/DA/") == False:
                        os.mkdir(self.da_outdir + "/DA/")
                    if os.path.exists(self.da_outdir + "/forecasting/") == False:
                        os.mkdir(self.da_outdir + "/forecasting/")
                    cmd_pars = "{0}/teco_workshop_{1}.txt{7} {0}/SPRUCE_pars.txt{7} {0}/SPRUCE_da_pars.txt{7} " \
                               "{2}/input/SPRUCE_forcing_2011_{3}.txt {0}/DA/{7} " \
                               "{2}/input/SPRUCE_cflux.txt {2}/input/SPRUCE_cpool.txt {2}/input/SPRUCE_ch4.txt {4} {5} {6}".format(
                        "\"" + self.da_outdir, "da_no_obs", model_folder, "2016", "0", "380", "100", "\""
                    )
                    if self.platform == "win32":
                        res = os.system(".\Source_code\TECO_2.3\TECO_2.3.exe {0}".format(cmd_pars))
                    else:
                        res = os.system(model_folder + "TECO_2.3.exe {0}".format(cmd_pars))

                    shutil.copyfile(self.da_outdir + "/SPRUCE_pars.txt",
                                    self.da_outdir + "/DA/SPRUCE_pars.txt")
                    shutil.copyfile(self.da_outdir + "/SPRUCE_da_pars.txt",
                                    self.da_outdir + "/DA/SPRUCE_da_pars.txt")
                    shutil.copyfile("./Source_code/TECO_2.3/workshop_nml/teco_workshop_forecasting.txt",
                                    self.da_outdir + "/teco_workshop_forecasting.txt")
                    shutil.copyfile(self.da_outdir + "/DA/Paraest.txt", self.da_outdir + "/forecasting/Paraest_example.txt")
                    
                    cmd_pars = "{0}/teco_workshop_{1}.txt{7} {0}/SPRUCE_pars.txt{7} {0}/SPRUCE_da_pars.txt{7} " \
                               "{2}/input/SPRUCE_forcing_2011_{3}.txt {0}/forecasting/{7} " \
                               "{2}/input/SPRUCE_cflux.txt {2}/input/SPRUCE_cpool.txt {2}/input/SPRUCE_ch4.txt {4} {5} {6}".format(
                        "\"" + self.da_outdir, "forecasting", model_folder, "2016",
                        "0",
                        "380",
                        "100", "\""
                    )
                    if self.platform == "win32":
                        res = os.system(".\Source_code\TECO_2.3\TECO_2.3.exe {0}".format(cmd_pars))
                    else:
                        res = os.system(model_folder + "TECO_2.3.exe {0}".format(cmd_pars))

                    year_ticks = [1, 365, 365*2, 365*3, 365*4, 365*5, 365*6]
                    year_names = [2011, 2012, 2013, 2014, 2015, 2016, 2017]
                    
                    res = plot_unit7_e2(model_folder, self.da_outdir, year_ticks, year_names, 1)
                    
                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()
                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()
            # else:
            #     if self.forecasting_outdir and self.da_indir:
            #         shutil.copyfile("./Source_code/TECO_2.3/input/SPRUCE_pars.txt",
            #                         self.forecasting_outdir + "/SPRUCE_pars.txt")
            #         shutil.copyfile("./Source_code/TECO_2.3/input/SPRUCE_da_pars.txt",
            #                         self.forecasting_outdir + "/SPRUCE_da_pars.txt")
            #         shutil.copyfile("./Source_code/TECO_2.3/workshop_nml/teco_workshop_forecasting.txt",
            #                         self.forecasting_outdir + "/teco_workshop_forecasting.txt")
            #         shutil.copyfile(self.da_indir + "/Paraest.txt", self.forecasting_outdir + "/Paraest_example.txt")
            #         # os.mkdir(self.forecasting_outdir + "/output")
            #         cmd_pars = "{0}/teco_workshop_{1}.txt{7} {2}/input/SPRUCE_pars.txt {2}/input/SPRUCE_da_pars.txt " \
            #                    "{2}/input/SPRUCE_forcing_2011_{3}.txt {0}{7} " \
            #                    "{2}/input/SPRUCE_cflux.txt {2}/input/SPRUCE_cpool.txt {2}/input/SPRUCE_ch4.txt {4} {5} {6}".format(
            #             "\"" + self.forecasting_outdir, "forecasting", model_folder, "2024",
            #             self.comboBox_2.currentText(),
            #             self.comboBox_3.currentText(),
            #             self.times.currentText(), "\""
            #         )
            #         if self.platform == "win32":
            #             res = os.system(".\Source_code\TECO_2.3\TECO_2.3.exe {0}".format(cmd_pars))
            #         else:
            #             res = os.system(model_folder + "TECO_2.3.exe {0}".format(cmd_pars))

            #         if res == 0:
            #             msg = QMessageBox()
            #             msg.setIcon(QMessageBox.Information)
            #             msg.setText("Finished!")
            #             msg.setWindowTitle("Info")
            #             msg.exec_()
            #     else:
            #         msg = QMessageBox()
            #         msg.setIcon(QMessageBox.Critical)
            #         msg.setText("Please select a folder!")
            #         msg.setWindowTitle("Error")
            #         msg.exec_()

        elif self.comboBox.currentText() == "Unit 8":
            model_folder = "./Source_code/TECO_2.3/"
            if self.matrix.currentText() == "1":
                res = 1
                if self.da_outdir != "":
                    
                    shutil.copyfile("./Source_code/TECO_2.3/DA_with_no_obs.zip",
                                    self.da_outdir + "/DA_with_no_obs.zip")
                    
                    res = 0
                    
                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()

                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

            elif self.matrix.currentText() == "2":
                res = 1
                if self.da_outdir != "":
                    
                    shutil.copyfile("./Source_code/TECO_2.3/DA_with_pools.zip",
                                    self.da_outdir + "/DA_with_pools.zip")
                    
                    res = 0

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()

                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

            elif self.matrix.currentText() == "3":
                res = 1
                if self.da_outdir != "":
                    
                    shutil.copyfile("./Source_code/TECO_2.3/DA_with_cflux.zip",
                                    self.da_outdir + "/DA_with_cflux.zip")

                    res = 0
                    
                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()

                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

            elif self.matrix.currentText() == "4":
                res = 1
                if self.da_outdir != "":
                    
                    shutil.copyfile("./Source_code/TECO_2.3/DA_with_cpools_and_cfluxes.zip",
                                    self.da_outdir + "/DA_with_cpools_and_cfluxes.zip")
                    
                    res = 0

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()

                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()
                   
        elif self.comboBox.currentText() == "Unit 9":
            model_folder = "./Source_code/TECO_2.3/"
            if self.matrix.currentText() == "1":
                if self.da_outdir:
                    if os.path.exists(self.da_outdir + "/DA/") == False:
                        os.mkdir(self.da_outdir + "/DA/")
                    if os.path.exists(self.da_outdir + "/forecasting/") == False:
                        os.mkdir(self.da_outdir + "/forecasting/")

                    cmd_pars = "{0}/teco_workshop_{1}.txt{7} {0}/SPRUCE_pars.txt{7} {0}/SPRUCE_da_pars.txt{7} " \
                               "{2}/input/SPRUCE_forcing_2011_{3}.txt {0}/DA/{7} " \
                               "{2}/input/SPRUCE_cflux.txt {2}/input/SPRUCE_cpool.txt {2}/input/SPRUCE_ch4.txt {4} {5} {6}".format(
                        "\"" + self.da_outdir, "da_no_obs", model_folder, "2016", "0", "380", "100", "\""
                    )
                    if self.platform == "win32":
                        res = os.system(".\Source_code\TECO_2.3\TECO_2.3.exe {0}".format(cmd_pars))
                    else:
                        res = os.system(model_folder + "TECO_2.3.exe {0}".format(cmd_pars))

                    shutil.copyfile(self.da_outdir + "/SPRUCE_pars.txt",
                                    self.da_outdir + "/DA/SPRUCE_pars.txt")
                    shutil.copyfile(self.da_outdir + "/SPRUCE_da_pars.txt",
                                    self.da_outdir + "/DA/SPRUCE_da_pars.txt")
                    shutil.copyfile("./Source_code/TECO_2.3/workshop_nml/teco_workshop_forecasting.txt",
                                    self.da_outdir + "/teco_workshop_forecasting.txt")
                    shutil.copyfile(self.da_outdir + "/DA/Paraest.txt", self.da_outdir + "/forecasting/Paraest_example.txt")
                    
                    cmd_pars = "{0}/teco_workshop_{1}.txt{7} {0}/SPRUCE_pars.txt{7} {0}/SPRUCE_da_pars.txt{7} " \
                               "{2}/input/SPRUCE_forcing_2011_{3}.txt {0}/forecasting/{7} " \
                               "{2}/input/SPRUCE_cflux.txt {2}/input/SPRUCE_cpool.txt {2}/input/SPRUCE_ch4.txt {4} {5} {6}".format(
                        "\"" + self.da_outdir, "forecasting", model_folder, "2024",
                        "0",
                        "380",
                        "100", "\""
                    )
                    if self.platform == "win32":
                        res = os.system(".\Source_code\TECO_2.3\TECO_2.3.exe {0}".format(cmd_pars))
                    else:
                        res = os.system(model_folder + "TECO_2.3.exe {0}".format(cmd_pars))
                    
                    year_ticks = [1, 365, 365*2, 365*3, 365*4, 365*5, 365*6, 365*7, 365*8, 365*9, 365*10, 365*11, 365*12, 365*13]
                    year_names = [2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024]

                    res = plot_unit7_e2(model_folder, self.da_outdir, year_ticks, year_names, 1)
                    
                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()
                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

            elif self.matrix.currentText() == "2":
                if os.path.exists(self.forecasting_outdir + "/forecasting/") == False:
                        os.mkdir(self.forecasting_outdir + "/forecasting/")
                if self.forecasting_outdir and self.da_indir:
                    shutil.copyfile(self.da_indir + "/SPRUCE_pars.txt",
                                    self.forecasting_outdir + "/SPRUCE_pars.txt")
                    shutil.copyfile(self.da_indir + "/SPRUCE_da_pars.txt",
                                    self.forecasting_outdir + "/SPRUCE_da_pars.txt")
                    shutil.copyfile("./Source_code/TECO_2.3/workshop_nml/teco_workshop_forecasting.txt",
                                    self.forecasting_outdir + "/teco_workshop_forecasting.txt")
                    shutil.copyfile(self.da_indir + "/Paraest.txt", self.forecasting_outdir + "/forecasting/Paraest_example.txt")
                    # os.mkdir(self.forecasting_outdir + "/output")
                    cmd_pars = "{0}/teco_workshop_{1}.txt{7} {0}/SPRUCE_pars.txt{7} {0}/SPRUCE_da_pars.txt{7} " \
                               "{2}/input/SPRUCE_forcing_2011_{3}.txt {0}/forecasting/{7} " \
                               "{2}/input/SPRUCE_cflux.txt {2}/input/SPRUCE_cpool.txt {2}/input/SPRUCE_ch4.txt {4} {5} {6}".format(
                        "\"" + self.forecasting_outdir, "forecasting", model_folder, "2024",
                        "0",
                        "380",
                        self.times.currentText(), "\""
                    )
                    if self.platform == "win32":
                        res = os.system(".\Source_code\TECO_2.3\TECO_2.3.exe {0}".format(cmd_pars))
                    else:
                        res = os.system(model_folder + "TECO_2.3.exe {0}".format(cmd_pars))

                    year_ticks = [1, 365, 365*2, 365*3, 365*4, 365*5, 365*6, 365*7, 365*8, 365*9, 365*10, 365*11, 365*12, 365*13]
                    year_names = [2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024]

                    res = plot_unit7_e2(model_folder, self.forecasting_outdir, year_ticks, year_names, 0, int(self.times.currentText()))

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()
                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

            elif self.matrix.currentText() == "3":
                if os.path.exists(self.da_outdir + "/forecasting/") == False:
                        os.mkdir(self.da_outdir + "/forecasting/")
                if self.forecasting_outdir and self.da_indir:
                    shutil.copyfile(self.da_indir + "/SPRUCE_pars.txt",
                                    self.forecasting_outdir + "/SPRUCE_pars.txt")
                    shutil.copyfile(self.da_indir + "/SPRUCE_da_pars.txt",
                                    self.forecasting_outdir + "/SPRUCE_da_pars.txt")
                    shutil.copyfile("./Source_code/TECO_2.3/workshop_nml/teco_workshop_forecasting.txt",
                                    self.forecasting_outdir + "/teco_workshop_forecasting.txt")
                    shutil.copyfile(self.da_indir + "/Paraest.txt", self.forecasting_outdir + "/forecasting/Paraest_example.txt")
                    # os.mkdir(self.forecasting_outdir + "/output")
                    cmd_pars = "{0}/teco_workshop_{1}.txt{7} {0}/SPRUCE_pars.txt{7} {0}/SPRUCE_da_pars.txt{7} " \
                               "{2}/input/SPRUCE_forcing_2011_{3}.txt {0}/forecasting/{7} " \
                               "{2}/input/SPRUCE_cflux.txt {2}/input/SPRUCE_cpool.txt {2}/input/SPRUCE_ch4.txt {4} {5} {6}".format(
                        "\"" + self.forecasting_outdir, "forecasting", model_folder, "2024",
                        self.comboBox_2.currentText(),
                        self.comboBox_3.currentText(),
                        "100", "\""
                    )
                    if self.platform == "win32":
                        res = os.system(".\Source_code\TECO_2.3\TECO_2.3.exe {0}".format(cmd_pars))
                    else:
                        res = os.system(model_folder + "TECO_2.3.exe {0}".format(cmd_pars))

                    year_ticks = [1, 365, 365*2, 365*3, 365*4, 365*5, 365*6, 365*7, 365*8, 365*9, 365*10, 365*11, 365*12, 365*13]
                    year_names = [2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024]

                    res = plot_unit7_e2(model_folder, self.forecasting_outdir, year_ticks, year_names, 0)

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()
                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

        elif self.comboBox.currentText() == "Unit 10":
            model_folder = "./Source_code/proda/"
            if self.matrix.currentText() == "1":

                if self.proda_task1_outdir:
                    if os.path.exists(self.proda_task1_outdir + "/output_data/") == False:
                        os.mkdir(self.proda_task1_outdir + "/output_data/")
                    
                    nn_clm_cen(model_folder, self.proda_task1_outdir + "/", "1")

                    cmd_pars = "1 {0}/ {1}{2}".format(model_folder, "\"" + self.proda_task1_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/NN_Indi_MAP_Project_CLM_CEN.R {0}".format(cmd_pars))
                    cmd_pars = "{0}/ {1}{2}".format(model_folder, "\"" + self.proda_task1_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/nn_para_map.R {0}".format(cmd_pars))

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()
                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

            elif self.matrix.currentText() == "2":

                if self.proda_task2_outdir:
                    if os.path.exists(self.proda_task2_outdir + "/output_data/") == False:
                        os.mkdir(self.proda_task2_outdir + "/output_data/")
                    
                    nn_clm_cen(model_folder, self.proda_task2_outdir + "/", "0", self.comboBox_10.currentText(),
                               self.comboBox_6.currentText(), self.comboBox_11.currentText(),
                               self.comboBox_7.currentText(),
                               self.comboBox_12.currentText(), self.comboBox_8.currentText(),
                               self.comboBox_13.currentText(),
                               self.comboBox_9.currentText()
                               )

                    cmd_pars = "1 {0}/ {1}{2}".format(model_folder, "\"" + self.proda_task2_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/NN_Indi_MAP_Project_CLM_CEN.R {0}".format(cmd_pars))
                    cmd_pars = "{0}/ {1}{2}".format(model_folder, "\"" + self.proda_task2_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/nn_para_map.R {0}".format(cmd_pars))

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()
                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

            elif self.matrix.currentText() == "3":

                if self.proda_task3_outdir:
                    if os.path.exists(self.proda_task3_outdir + "/output_data/") == False:
                        os.mkdir(self.proda_task3_outdir + "/output_data/")
                    cmd_pars = "{0}/ {1}{2}".format(model_folder, "\"" + self.proda_task3_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/One_Batch_DA.R {0}".format(cmd_pars))

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()
                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()
            else:

                if self.proda_task4_input1 and self.proda_task4_input2 and self.proda_task4_outdir:
                    if os.path.exists(self.proda_task4_outdir + "/output_data/") == False:
                        os.mkdir(self.proda_task4_outdir + "/output_data/")
                    self.copy_task_result(self.proda_task4_input1 + "/output_data",
                                          self.proda_task4_outdir + "/output_data")
                    self.copy_task_result(self.proda_task4_input2 + "/output_data",
                                          self.proda_task4_outdir + "/output_data")

                    cmd_pars = "1 {0}/ {1}{2}".format(model_folder, "\"" + self.proda_task4_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/NN_Indi_MAP_Project_CLM_CEN.R {0}".format(cmd_pars))

                    cmd_pars = "2 {0}/ {1}{2}".format(model_folder, "\"" + self.proda_task4_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/NN_Indi_MAP_Project_CLM_CEN.R {0}".format(cmd_pars))

                    cmd_pars = "0 {0}/ {1}{2}".format(model_folder, "\"" + self.proda_task4_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/Global_Projection_NN_CLM_CEN.R {0}".format(cmd_pars))

                    cmd_pars = "1 {0}/ {1}{2}".format(model_folder, "\"" + self.proda_task4_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/Global_Projection_NN_CLM_CEN.R {0}".format(cmd_pars))

                    cmd_pars = "{0}/ {1}{2}".format(model_folder, "\"" + self.proda_task4_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/Different_Method_obs_vs_mod.R {0}".format(cmd_pars))

                    cmd_pars = "{0}{1}".format("\"" + self.proda_task4_outdir + "/", "\"")
                    res = os.system("Rscript ./Source_code/proda/different_method_soil_map.R {0}".format(cmd_pars))

                    if res == 0:
                        msg = QMessageBox()
                        msg.setIcon(QMessageBox.Information)
                        msg.setText("Finished!")
                        msg.setWindowTitle("Info")
                        msg.exec_()
                else:
                    msg = QMessageBox()
                    msg.setIcon(QMessageBox.Critical)
                    msg.setText("Please select a folder!")
                    msg.setWindowTitle("Error")
                    msg.exec_()

    def get_platform(self):
        self.platform = sys.platform


    def copy_task_result(self, source_path, target_path):
        for root, dirs, files in os.walk(source_path):
            for file in files:
                src_file = os.path.join(root, file)
                shutil.copy(src_file, target_path)