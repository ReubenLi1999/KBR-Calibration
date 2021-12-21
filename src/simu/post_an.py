import numpy as np
import dask.dataframe as dd
import matplotlib.pyplot as plt
from numpy import poly1d
from scipy.signal import welch
import plotly.graph_objs as go
import dash
import dash_core_components as dcc
import dash_html_components as html


def main():
    dd_kbr1b = dd.read_csv(
        urlpath='..//..//..//..//gracefo_dataset//gracefo_1B_2019-01-01_RL04.ascii.noLRI//KBR1B_2019-01-01_Y_04.txt',
        engine='c',
        header=None,
        sep='\s+',
        skiprows=162,
        names=['gps_time', 'id', 'biased_range', 'range_rate', 'range_accl', 'iono_corr', 'lighttime_err', 'lighttime_rate',
               'lighttime_accl', 'ant_centr_corr', 'ant_centr_rate', 'ant_centr_accl', 'k_a_snr',
               'ka_a_snr', 'k_b_snr', 'ka_b_snr', 'qualflg'])
    gni_c = np.loadtxt(
        '..//..//output//inertial_trajectory_2019-01-01_C_04.txt', skiprows=1, max_rows=200)
    gni_d = np.loadtxt(
        '..//..//output//inertial_trajectory_2019-01-01_D_04.txt', skiprows=1, max_rows=200)
    phase_centre = np.loadtxt('..//..//temp//ant_phase_centre_corr_2degree.txt')
    highpass_0_02hz = np.loadtxt('..//..//output//filter_b_0.02Hz_highpass.txt')
    highpass_0_02hz_150 = np.loadtxt('..//..//output//filter_b1_0.02Hz_highpass.txt')

    # non_simu = dd_kbr1b.biased_range.compute().to_numpy() + dd_kbr1b.lighttime_err.compute().to_numpy() + phase_centre
    # np.savetxt('..//..//output//dis_non_simu.txt', non_simu)
    inter_range = np.zeros(gni_c.__len__())
    for index, (c, d) in enumerate(zip(gni_c, gni_d)):
        inter_range[index] = np.linalg.norm([c[1] - d[1], c[2] - d[2], c[3] - d[3]])

    # coeff = np.polyfit(gni_c[:, 0], inter_range - dd_kbr1b['biased_range'].compute().to_numpy(), 1)

    # plt.plot(inter_range - dd_kbr1b['biased_range'].compute().to_numpy())
    # plt.plot(coeff[0] * gni_c[:, 0] + coeff[1])
    # plt.show()
    # plt.plot(inter_range - dd_kbr1b['biased_range'].compute().to_numpy() - coeff[0] * gni_c[:, 0] - coeff[1])
    # plt.show()

    fig_phase_centr_corr = go.Figure()
    fig_phase_centr_corr.add_trace(go.Scatter(x=dd_kbr1b['gps_time'].compute().to_numpy()[: 200],
                                              y=dd_kbr1b['biased_range'].compute().to_numpy()[: 200] - inter_range[: 200] + phase_centre[: 200] - dd_kbr1b['lighttime_err'].compute().to_numpy()[: 200],
                                              line=dict(color='green', width=2)))
    app = dash.Dash()
    app.layout = html.Div([
        dcc.Graph(figure=fig_phase_centr_corr)
    ])
    app.run_server(debug=True, use_reloader=True)

    freq, psd = welch(
        inter_range - dd_kbr1b['biased_range'].compute().to_numpy() - dd_kbr1b['ant_centr_corr'].compute().to_numpy(),
        0.2, ('kaiser', 160.), 250, scaling='density')
    plt.loglog(freq, np.sqrt(psd))
    plt.title('post-fit')
    plt.ylabel('asd')
    plt.xlabel('freq [Hz]')
    # plt.show()

    # freq_non_simu, psd_non_simu = welch(
    #     non_simu, 0.2, ('kaiser', 30.0), 17280, scaling='density'
    # )
    # plt.xlabel('Frequency [Hz]')
    # plt.ylabel(r'$ASD m/\sqrt{Hz}$')
    # plt.loglog(freq_non_simu, np.sqrt(psd_non_simu))
    # plt.show()
    # freq_002, psd_002 = welch(
    #     highpass_0_02hz, 0.2, ('kaiser', 30.0), highpass_0_02hz.__len__(), scaling='density'
    # )
    # plt.xlabel('Frequency [Hz]')
    # plt.ylabel(r'$ASD m/\sqrt{Hz}$')
    # plt.loglog(freq_002, np.sqrt(psd_002), label='kou')
    # plt.show()


if __name__ == "__main__":
    main()
