from GeneralModel import GeneralModel
import numpy as np
import matplotlib.pyplot as plt

B = np.array([0.45, 0.55, 0, 0, 0, 0, 0]).reshape([7,1])

f31 = 0.72; f41 = 0.28; f42 = 1; f53 = 0.45; f54 = 0.275; f64 = 0.275;
f65 = 0.296; f75 = 0.004; f56 = 0.42; f76 = 0.03; f57 = 0.45;

A = np.array([-1, 0, 0, 0, 0, 0, 0,
             0, -1, 0, 0, 0, 0, 0,
             f31, 0, -1, 0, 0, 0, 0,
             f41, f42, 0, -1, 0, 0, 0,
             0, 0, f53, f54, -1, f56, f57,
             0, 0, 0, f64, f65, -1, 0,
             0, 0, 0, 0, f75, f76, -1]).reshape([7,7])

temp = [0.00176, 0.000100104, 0.021468, 0.000845, 0.008534, 8.976e-005, 0.00000154782]

K = np.zeros(49).reshape([7, 7])

for i in range(0, 7):
    K[i][i] = temp[i]

K = np.multiply(K, 1/86400)

test = np.array([0.00002245, 0,0,0,0,0,0]).reshape([7,1])

def input_fluxes(t, y):
    tmp = (0.00002245 / 2 * (0.2 + (1 - np.exp( -0.5 * y[0] * 0.008))) / 0.5)
    return tmp

def fun_K(t, y):
    K[6][6] = temp[6] / 86400 * (y[6] / 1000.0 + 0.1)
    return K

input_fluxes = 0.00002245

times = np.arange(0, 10000*365*86400, 365*86400)

iv_list = [0,0,0,0,0,0,0]

mod = GeneralModel(times, B, A, fun_K, iv_list, input_fluxes)

res = mod.get_x()

fig = plt.figure(6*2, figsize=(14, 7.68))
plt.subplots_adjust(left  = 0.1, right = 0.95, bottom = 0.10, top = 0.9, wspace =0.2, hspace =0)
x = list(range(1,10001, 1))

for i in range(1, 4):
    for j in range(1, 4):
        if ((i-1) * 3 + j) > 7 :
            break
        ax = plt.subplot(3, 3, (i-1) * 3 + j)
        ax.plot(x, res[(i-1) * 3 + j - 1,:])
# plt.savefig(r"C:\Users\yg336\OneDrive - Northern Arizona University\Fall_2020\LandPy\test" + ".png", dpi = 500)
plt.show()

print(res[:,1])

mod.sasu_spinup() # without calling this function, we will do ND spin up or simulation
print(res[:,1])

mod.write_output("./output.csv")
