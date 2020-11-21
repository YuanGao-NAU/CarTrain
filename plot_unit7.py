import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from linecache import getline
import seaborn as sns
import time
import threading

def plot_unit7_e1(input_folder, output_folder):

        print("-----------------------------------Plotting started!------------------------------------")

        df = pd.read_csv(output_folder + "/Simu_dailyflux14001.csv")

        list1 = ['sdoy', 'GPP_d', 'NEE_d', 'Reco_d', 'NPP_d', 'Ra_d', 'QC1', 'QC2', 'QC3', 
                'QC4', 'QC5', 'QC6', 'Rh_d', 'QC7', 'QC8']
        df.columns = list1

        df1 = pd.read_csv(input_folder + "SPRUCE_cflux.txt", sep = "\t")
        df2 = pd.read_csv(input_folder + "SPRUCE_cpool.txt", sep = "\t")
        df2[df2 < -999] = np.nan

        fig = plt.figure(3*2, figsize = (19.20, 10.80))
        plt.subplots_adjust(left  = 0.1, right = 0.95, bottom = 0.05, top = 0.9, wspace =0.2, hspace =0.4)

        year_ticks = [1, 365, 365*2, 365*3, 365*4, 365*5, 365*6]
        year_names = [2011, 2012, 2013, 2014, 2015, 2016, 2017]

        ax = plt.subplot(3, 2, 1)
        ax.plot(df["sdoy"], df["GPP_d"], color = "black")
        ax.scatter(df1["days"], df1["GPP"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("GPP (g C $m^{-2}d^{-1}$)", fontsize = 18)

        print("Plotting GPP!")

        ax = plt.subplot(3, 2, 2)
        ax.plot(df["sdoy"], df["Reco_d"], color = "black")
        ax.scatter(df1["days"], df1["Reco"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("NEE (g C $m^{-2}d^{-1}$)", fontsize = 18)

        print("Plotting NEE!")

        ax = plt.subplot(3, 2, 3)
        ax.plot(df["sdoy"], df["NEE_d"], color = "black")
        ax.scatter(df1["days"], df1["NEE"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("ER (g C $m^{-2}d^{-1}$)", fontsize = 18)

        print("Plotting Reco!")

        ax = plt.subplot(3, 2, 4)
        ax.plot(df["sdoy"], df["Rh_d"], color = "black")
        #ax.scatter(df1["days"], df1["NEE"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("Rh (g C $m^{-2}d^{-1}$)", fontsize = 18)

        print("Plotting Rh!")

        ax = plt.subplot(3, 2, 5)
        ax.plot(df["sdoy"], df["Ra_d"], color = "black")
        #ax.scatter(df1["days"], df1["NEE"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("Ra (g C $m^{-2}d^{-1}$)", fontsize = 18)

        print("Plotting Ra!")

        ax = plt.subplot(3, 2, 6)
        ax.plot(df["sdoy"], df["NPP_d"], color = "black")
        #ax.scatter(df1["days"], df1["NEE"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("NPP (g C $m^{-2}d^{-1}$)", fontsize = 18)
        plt.savefig(output_folder + "/daily_fluxes.png", dpi=500)

        print("Plotting NPP!")

        plt.clf()
        plt.cla()
        plt.close()


        fig = plt.figure(2*2, figsize = (19.20, 10.80))
        plt.subplots_adjust(left  = 0.1, right = 0.95, bottom = 0.05, top = 0.9, wspace =0.2, hspace =0.4)

        year_ticks = [1, 365, 365*2, 365*3, 365*4, 365*5, 365*6]
        year_names = [2011, 2012, 2013, 2014, 2015, 2016, 2017]

        ax = plt.subplot(2, 2, 1)
        ax.plot(df["sdoy"], df["QC1"], color = "black")
        ax.scatter(df2["days"], df2["Foliage"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("Leaf pool ($g m^{-2}$)", fontsize = 18)
        print("Plotting leaf carbon pool!")

        ax = plt.subplot(2, 2, 2)
        ax.plot(df["sdoy"], df["QC2"], color = "black")
        ax.scatter(df2["days"], df2["Wood"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("Wood pool ($g m^{-2}$)", fontsize = 18)
        print("Plotting wood carbon pool!")

        ax = plt.subplot(2, 2, 3)
        ax.plot(df["sdoy"], df["QC3"], color = "black")
        ax.scatter(df2["days"], df2["Root"], color = "red")
        ax.set_xticks(year_ticks)
        ax.set_xticklabels(year_names)
        plt.xlabel("year", fontsize = 18)
        plt.ylabel("Root pool ($g m^{-2}$)", fontsize = 18)
        print("Plotting root carbon pool!")
        plt.savefig(output_folder + "/daily_cpools.png", dpi=500)
        plt.clf()
        plt.cla()
        plt.close()

        print("-----------------------------------Plotting finished!------------------------------------")

        return 0

def plot_unit7_e2(input_folder, output_folder, year_ticks, year_names, par = 1, repn = 100):
    plt.clf()
    plt.cla()
    plt.close()

    print("-----------------------------------Plotting started!------------------------------------")

    if par == 1:
        df = pd.read_csv(output_folder + "/DA/Paraest.txt", header = None)
        df.drop(df.columns[len(df.columns)-1], axis=1, inplace=True)

        list1 = []
        list2 = []
        list3 = []
        list4 = []
        list5 = ["isimu", "upgraded"]
        str1 = ""
        str2 = ""
        for i in range(1, 21):
            if i % 2 != 0:
                str1 += getline(output_folder + "/SPRUCE_da_pars.txt", i).strip() + "\t"
            else:
                str2 += getline(output_folder + "/SPRUCE_da_pars.txt", i).strip() + "\t"
        list1 = str1.split("\t")
        list2 = str2.split("\t")
        for item in list1:
            list3 += item.split()
        for item in list2:
            list4 += item.split()
        len(list3)
        len(list4)

        for i in range(len(list4)):
            if list4[i] == "1":
                list5.append(list3[i])
        df.columns = list5

        fig = plt.figure(3 * 6, figsize=(19.20, 10.80))
        plt.subplots_adjust(left=0.05, right=0.99, bottom=0.12, top=0.92, wspace=0.3, hspace=0.4)

        for i in range(len(df.columns) - 2):
            ax = plt.subplot(3, 6, i + 1)
            ax.plot(df[list5[i + 2]])
            #plt.xticks([])
            plt.xlabel("times", fontsize=16)
        plt.ylabel(list5[i + 2], fontsize=16)
        plt.savefig(output_folder + "/DA/Paraest.png", dpi=500)
        plt.clf()
        plt.cla()
        plt.close()
        print("Paraest.png plotted!")

        fig = plt.figure(3*6, figsize=(19.20, 10.80))
        plt.subplots_adjust(left  = 0.05, right = 0.99, bottom = 0.12, top = 0.92, wspace = 0.3, hspace = 0.2)

        for i in range(len(df.columns) - 2):
            ax = plt.subplot(3, 6, i+1)
            sns.kdeplot(df[list5[i+2]], cumulative=False, shade=True, ax = ax)
        plt.savefig(output_folder + "/DA/Paraest_PDF.png", dpi = 500)
        plt.clf()
        plt.cla()
        plt.close()
        print("Paraest_PDF.png plotted!")

        fig = plt.figure(3*6, figsize=(19.20, 10.80))
        plt.subplots_adjust(left  = 0.05, right = 0.99, bottom = 0.12, top = 0.92, wspace = 0.3, hspace = 0.2)

        for i in range(len(df.columns) - 2):
            ax = plt.subplot(3, 6, i+1)
            sns.kdeplot(df[list5[i+2]], cumulative=True, shade=True, ax = ax)
        plt.savefig(output_folder + "/DA/Paraest_CDF.png", dpi = 500)
        plt.clf()
        plt.cla()
        plt.close()
        print("Paraest_CDF.png plotted!")
    
    # def child_thread():
    #         while True:
    #                 print("Still running -", end = "\r")
    #                 time.sleep(0.5)
    #                 print("Still running \\", end = "\r")
    #                 time.sleep(0.5)
    #                 print("Still running /", end = "\r")
    #                 time.sleep(0.5)
    # thread1 = threading.Thread(target = child_thread)
    # thread1.setDaemon(True)
    # thread1.start()

    # plot forecasting

    list_df = []
    columns = ['sdoy', 'GPP_d', 'NEE_d', 'Reco_d', 'QC1', 'GL_yr', 'QC2',
        'GW_yr', 'QC3', 'GR_yr', 'QC678', 'pheno', 'LAI']

    for i in range(1, repn):
        s = str(i).zfill(3)
        df_tmp = pd.read_csv(output_folder + "/forecasting/Simu_dailyflux{0}.txt".format(s))
        df_tmp.columns = columns
        try:
            for item in columns:
                pd.to_numeric(df_tmp[item])
        except ValueError:
            pass
        else:
            list_df.append(df_tmp)
        
    df_forecasting = pd.concat(list_df)
    df_observation = pd.read_csv("./Source_code/TECO_2.3/input/SPRUCE_cflux.txt", sep = "\t")
    df_obs_cpool = pd.read_csv("./Source_code/TECO_2.3/input/SPRUCE_cpool.txt", sep = "\t")
    df_obs_cpool[df_obs_cpool < -999] = np.nan

    fig = plt.figure(3*2, figsize=(19.20, 10.80))
    plt.subplots_adjust(left  = 0.05, right = 0.99, bottom = 0.12, top = 0.92, wspace = 0.3, hspace = 0.2)

    ax = plt.subplot(3, 2, 1)
    sns_plot = sns.lineplot(data = df_forecasting, x = "sdoy", y = "QC1", color="red", label = "forecasting", ax = ax)
    sns.scatterplot(data = df_obs_cpool, x = "days", y = "Foliage", color = "blue", label = "observation")
    ax.set_xticks(year_ticks)
    ax.set_xticklabels(year_names)
    sns_plot.set_xlabel("year")
    sns_plot.set_ylabel("Leaf pool (g C $m^{-2})")
    fig = sns_plot.get_figure()
    #fig.savefig(output_folder + "/forecasting/GPP.png", dpi = 500)
    print("Plotting leaf carbon pool!")

    ax = plt.subplot(3, 2, 2)
    sns_plot = sns.lineplot(data = df_forecasting, x = "sdoy", y = "QC2", color="red", label = "forecasting", ax = ax)
    sns.scatterplot(data = df_obs_cpool, x = "days", y = "Wood", color = "blue", label = "observation")
    ax.set_xticks(year_ticks)
    ax.set_xticklabels(year_names)
    sns_plot.set_xlabel("year")
    sns_plot.set_ylabel("Wood pool (g C $m^{-2})")
    fig = sns_plot.get_figure()
    #fig.savefig(output_folder + "/forecasting/GPP.png", dpi = 500)
    print("Plotting wood carbon pool!")

    ax = plt.subplot(3, 2, 3)
    sns_plot = sns.lineplot(data = df_forecasting, x = "sdoy", y = "QC3", color="red", label = "forecasting", ax = ax)
    sns.scatterplot(data = df_obs_cpool, x = "days", y = "Root", color = "blue", label = "observation")
    ax.set_xticks(year_ticks)
    ax.set_xticklabels(year_names)
    sns_plot.set_xlabel("year")
    sns_plot.set_ylabel("Root pool (g C $m^{-2})")
    fig = sns_plot.get_figure()
    #fig.savefig(output_folder + "/forecasting/GPP.png", dpi = 500)
    print("Plotting root carbon pool!")
    
    ax = plt.subplot(3, 2, 4)
    sns_plot = sns.lineplot(data = df_forecasting, x = "sdoy", y = "GPP_d", color="red", label = "forecasting", ax = ax)
    sns.scatterplot(data = df_observation, x = "days", y = "GPP", color = "blue", label = "observation")
    ax.set_xticks(year_ticks)
    ax.set_xticklabels(year_names)
    sns_plot.set_xlabel("year")
    sns_plot.set_ylabel("GPP ($g C m^{-2} s^{-1}$)")
    fig = sns_plot.get_figure()
    #fig.savefig(output_folder + "/forecasting/GPP.png", dpi = 500)
    print("Plotting GPP!")

    ax = plt.subplot(3, 2, 5)
    sns_plot = sns.lineplot(data = df_forecasting, x = "sdoy", y = "NEE_d", color="red", label = "forecasting", ax = ax)
    sns.scatterplot(data = df_observation, x = "days", y = "NEE", color = "blue", label = "observation")
    ax.set_xticks(year_ticks)
    ax.set_xticklabels(year_names)
    sns_plot.set_xlabel("year")
    sns_plot.set_ylabel("NEE ($g C m^{-2} s^{-1}$)")
    fig = sns_plot.get_figure()
    #fig.savefig(output_folder + "/forecasting/NEE.png", dpi = 500)
    print("Plotting NEE!")

    ax = plt.subplot(3, 2, 6)
    sns_plot = sns.lineplot(data = df_forecasting, x = "sdoy", y = "Reco_d", color="red", label = "forecasting", ax = ax)
    sns.scatterplot(data = df_observation, x = "days", y = "Reco", color = "blue", label = "observation")
    ax.set_xticks(year_ticks)
    ax.set_xticklabels(year_names)
    sns_plot.set_xlabel("year")
    sns_plot.set_ylabel("Reco ($g C m^{-2} s^{-1}$)")
    fig = sns_plot.get_figure()
    print("Plotting Reco!")

    fig.savefig(output_folder + "/forecasting/pool_and_flux.png", dpi = 500)
    plt.clf()
    plt.cla()
    plt.close()

    print("-----------------------------------Plotting finished!------------------------------------")
    
    #thread1._stop()

    return 0


