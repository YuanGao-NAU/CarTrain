from Model import Model
from scipy.integrate import solve_ivp
import numpy as np
import pandas as pd
from inspect import isfunction

class GeneralModel(Model):
    def __init__(self, times, B, A, K, iv_list, input_fluxes, xi=1):
        self.times = times
        self.B = B
        self.A = A
        self.K = K
        self.iv_list = iv_list
        self.input_fluxes = input_fluxes
        self.xi = xi
        self.pool_n = self.get_pool_n()

        self.Y = self.ode_solver()
        self.create_headers()
        self.df = self.get_df()
        #self.get_c_input()
        self.get_diagnostic_variables()

    def ode_solver(self):
        t = [self.times[0], self.times[-1]]
        y = solve_ivp(self.right_hand_equation, t, self.iv_list, t_eval = self.times, vectorized = True)
        return y

    def get_input(self, t, y):
        idx = (np.abs(self.times - t)).argmin()
        if self.times[idx] > t:
            idx = idx - 1

        if isfunction(self.input_fluxes):
            self.tmp_input_fluxes = self.input_fluxes(t, y)
        else:
            if type(self.input_fluxes) == np.ndarray:
                self.tmp_input_fluxes = self.input_fluxes[idx]
            else:
                self.tmp_input_fluxes = self.input_fluxes

        if isfunction(self.B):
            self.tmp_B = self.B(t, y)
        else:
            if type(self.B) == np.ndarray:
                if self.B.shape[1] != 1 and self.B.shape[2] != 1:
                    self.tmp_B = self.B[idx].reshape(self.pool_n, 1)
                else:
                    self.tmp_B = self.B

        if isfunction(self.A):
            self.tmp_A = self.A(t, y)
        else:
            self.tmp_A = self.A

        if isfunction(self.K):
            self.tmp_K = self.K(t, y)
        else:
            self.tmp_K = self.K

        if isfunction(self.xi):
            self.tmp_xi = self.xi(t, y)
        else:
            if type(self.xi) == np.ndarray:
                self.tmp_xi = self.xi[idx]
            else:
                self.tmp_xi = self.xi

    def right_hand_equation(self, t, y):
        self.get_input(t, y)
        dydt = np.multiply(self.tmp_B, self.tmp_input_fluxes) + np.matmul(np.matmul(self.tmp_xi * self.tmp_A, self.tmp_K), y)
        return dydt

    def get_x(self):
        return self.Y.y

    def get_df(self):
        res = self.Y.y.T
        df = pd.DataFrame(res)
        df.columns = self.create_headers()
        return df

    def write_output(self, filename):
        self.df.to_csv(filename, index=False)

    def get_diagnostic_variables(self):

        carbon_storage_capacity = []
        carbon_storage_potential = []
        residence_time = []
        carbon_storage = []
        baseline_residence_time = []

        dimensions = self.Y.y.T.shape
        X = np.zeros(dimensions)
        Xc = np.zeros(dimensions)
        Xp = np.zeros(dimensions)
        
        for i in range(0, len(self.times)):

            t = self.times[i]
            Y = self.Y.y
            y = Y[:, i]
            y = y.reshape([y.shape[0], 1])

            dydt = self.right_hand_equation(t, y)
            matrix_AK = np.matmul(self.tmp_xi * self.tmp_A, self.tmp_K)
            inverse_AK = np.linalg.inv(matrix_AK)
            matrix_Residence = np.matmul(-inverse_AK, self.tmp_B)
            Residence_time = np.sum(matrix_Residence)

            baseline_matrix_AK = np.matmul(self.tmp_A, self.tmp_K)
            baseline_inverse_AK = np.linalg.inv(baseline_matrix_AK)
            matrix_baseline_Residence = np.matmul(-baseline_inverse_AK, self.tmp_B)
            baseline_Residence_time = np.sum(matrix_baseline_Residence)

            Xc[i] = np.multiply(matrix_Residence, self.tmp_input_fluxes).reshape(dimensions[1])
            X[i] = y.reshape(dimensions[1])
            Xp[i] = -np.matmul(inverse_AK, dydt).reshape(dimensions[1])

            carbon_storage_capacity.append(np.sum(np.multiply(matrix_Residence, self.tmp_input_fluxes)))
            carbon_storage_potential.append(-np.sum(np.matmul(inverse_AK, dydt)))
            residence_time.append(Residence_time)
            carbon_storage.append(np.sum(y))
            baseline_residence_time.append(np.sum(baseline_Residence_time))

        self.df["Tres"] = residence_time
        self.df["X"] = carbon_storage
        self.df["Xc"] = carbon_storage_capacity
        self.df["Xp"] = carbon_storage_potential
        self.df["Tbres"] = baseline_residence_time
        self.df["xi"] = self.xi

        self.Xc = pd.DataFrame(Xc)
        self.X = pd.DataFrame(X)
        self.Xp = pd.DataFrame(Xp)

    def sasu_spinup(self):
        #Xss = -(AK)^(-1)BU
        dimensions = self.Y.y.T.shape
        X = np.zeros(dimensions)

        for i in range(0, len(self.times)):
            t = self.times[i]
            Y = self.Y.y
            y = Y[:, i]
            y = y.reshape([y.shape[0], 1])
            self.get_input(t, y)
            dydt = 0
            matrix_AK = np.matmul(self.tmp_xi * self.tmp_A, self.tmp_K)
            inverse_AK = np.linalg.inv(matrix_AK)
            matrix_BU = np.multiply(self.tmp_B, self.tmp_input_fluxes)
            X[i] = np.matmul(-inverse_AK, matrix_BU).reshape(dimensions[1])

        col_headers = self.create_headers()
        for i in range(0, dimensions[1]):
            self.df[col_headers[i]] = X[:, i]

    def create_headers(self):
        col_headers = []
        rows = self.Y.y.shape[0]
        for i in range(0, rows):
            col_headers.append("X" + str(i+1))
        return col_headers

    def get_x_df(self):
        return self.df

    def get_pool_n(self):
        return len(self.iv_list)

    def get_c_input(self):
        c_input = []
        if type(self.input_fluxes) == np.ndarray or type(self.input_fluxes) == list:
            c_input = self.input_fluxes
        if isfunction(self.input_fluxes):
            for i in range(0, len(self.times)):
                t = self.times[i]
                Y = self.Y.y
                y = Y[:, i]
                y = y.reshape([y.shape[0], 1])
                c_input.append(self.input_fluxes(t, y)[1])
        else:
            c_input = self.input_fluxes
        self.df["C_input"] = c_input
        headers = ["C_input"] + self.create_headers()
        self.df = self.df[headers]