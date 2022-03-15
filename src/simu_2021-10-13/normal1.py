import numpy as np
import dask.dataframe as dd
import matplotlib.pyplot as plt
from pyquaternion import Quaternion
import plotly.graph_objects as go
import dash
import dash_core_components as dcc
import dash_html_components as html
from scipy.fft import fft, fftfreq
from plotly.subplots import make_subplots

def main():

    phase_centre_c = np.asarray([1.50, 0.0002, 0.0002])  # unit m
    phase_centre_d = np.asarray([1.50, 0.0002, 0.0002])  # unit m

    dd_kbr1b, dd_sca1b_c, dd_sca1b_d, dd_gni1b_c, dd_gni1b_d = load_data()
    quaternion_c = np.c_[dd_sca1b_c.q1.compute().to_numpy(),
                         dd_sca1b_c.q2.compute().to_numpy(),
                         dd_sca1b_c.q3.compute().to_numpy(),
                         dd_sca1b_c.q4.compute().to_numpy()]
    quaternion_d = np.c_[dd_sca1b_d.q1.compute().to_numpy(),
                         dd_sca1b_d.q2.compute().to_numpy(),
                         dd_sca1b_d.q3.compute().to_numpy(),
                         dd_sca1b_d.q4.compute().to_numpy()]
    rotm_gcrs2srf_c = quater2rotm(quaternion_c)
    rotm_srf2gcrs_c = rotm_gcrs2srf_c.transpose((0, 2, 1))
    rotm_gcrs2srf_d = quater2rotm(quaternion_d)
    rotm_srf2gcrs_d = rotm_gcrs2srf_d.transpose((0, 2, 1))

    # altitude angle from srf_new to srf
    alt_angle_sin = np.deg2rad(2 + 0.97 * np.sin(2. * np.pi / 254. * np.arange(0, 86400, 1)))
    alt_angle_sinx = np.deg2rad(0.1 * np.sin(2. * np.pi / 254. * np.arange(0, 86400, 1)))
    alt_angle_sin1 = np.deg2rad(2 + 0.97 * np.sin(2. * np.pi / 254. * np.arange(0, 86400, 1)) + np.random.randn(1, 86400) * (5/3600)**2)
    alt_angle_sinx1 = np.deg2rad(0.1 * np.sin(2. * np.pi / 254. * np.arange(0, 86400, 1)) + np.random.randn(1, 86400) * (5/3600)**2)
    alt_angle_sin1 = alt_angle_sin1[0, :]
    alt_angle_sinx1 = alt_angle_sinx1[0, :]

    # plt.style.use(['science', 'no-latex', 'high-vis'])
    # fig, ax = plt.subplots(1, 1, figsize=(20, 10))
    # ax.plot(np.arange(0, 86400, 1), np.rad2deg(alt_angle_sinx), linewidth=2)
    # ax.yaxis.get_offset_text().set_fontsize(24)
    # ax.xaxis.get_offset_text().set_fontsize(24)
    # ax.tick_params(labelsize=20, width=2.9)
    # ax.set_xlabel('Sampling Points [5Hz]', fontsize=20)
    # ax.set_ylabel(r'Angle [degree]', fontsize=20)
    # ax.grid(True, which='both', ls='dashed', color='0.5', linewidth=0.6)
    # plt.show()

    # yaw
    yaw_sin = np.c_[np.zeros(alt_angle_sin.__len__()),
                    np.zeros(alt_angle_sin.__len__()),
                    alt_angle_sin]
    pitch_sin = np.c_[np.zeros(alt_angle_sin.__len__()),
                      alt_angle_sin,
                      alt_angle_sinx]

    yaw_sin1 = np.c_[np.zeros(alt_angle_sin1.__len__()),
                     np.zeros(alt_angle_sin1.__len__()),
                     alt_angle_sin1]
    pitch_sin1 = np.c_[np.zeros(alt_angle_sin1.__len__()),
                       alt_angle_sin1,
                       alt_angle_sinx1]
    # rotm_yaw = eul2rotm(yaw_sin)
    rotm_yaw = eul2rotm(pitch_sin).transpose((0, 2, 1))
    rotm_pitch = eul2rotm(pitch_sin).transpose((0, 2, 1))
    rotm_yaw1 = eul2rotm(pitch_sin1).transpose((0, 2, 1))
    rotm_pitch1 = eul2rotm(pitch_sin1).transpose((0, 2, 1))

    # let the leading satellite rotate
    rotm_d = np.matmul(rotm_pitch, rotm_gcrs2srf_d.transpose((0, 2, 1)))
    rotm_d1 = np.matmul(rotm_pitch1, rotm_gcrs2srf_d.transpose((0, 2, 1)))
    # rotm_c = rotm_gcrs2srf_c
    rotm_c = rotm_gcrs2srf_c

    rotm_A_t = rotm_d.transpose((0, 2, 1))
    rotm_A_t1 = rotm_d1.transpose((0, 2, 1))
    rotm_B_t = rotm_c
    quat_c = np.zeros([rotm_d.__len__(), 4])
    for index, rot_m in enumerate(rotm_A_t1):
        temp = Quaternion(matrix=rot_m)
        quat_c[index, :] = temp.elements
    phase_centre_gcrs = np.zeros([rotm_c.__len__(), 3])

    for index, (c, d) in enumerate(zip(rotm_B_t, rotm_A_t)):
        phase_centre_gcrs[index] = -np.matmul(c, phase_centre_c) + np.matmul(d, phase_centre_d)

    intersatellite = np.c_[dd_gni1b_c.xpos.compute().to_numpy()[::5] - dd_gni1b_d.xpos.compute().to_numpy()[::5],
                           dd_gni1b_c.ypos.compute().to_numpy()[::5] - dd_gni1b_d.ypos.compute().to_numpy()[::5],
                           dd_gni1b_c.zpos.compute().to_numpy()[::5] - dd_gni1b_d.zpos.compute().to_numpy()[::5]]
    range = np.linalg.norm(intersatellite, axis=1)

    eqa = np.zeros([intersatellite.__len__(), 6])
    for index, (inter, rotm1, rotm2) in enumerate(zip(intersatellite, rotm_A_t1[::5], rotm_B_t[::5])):
        eqa[index, : 3] = np.matmul(inter / np.linalg.norm(inter), rotm1)
        eqa[index, 3:] = np.matmul(inter / np.linalg.norm(inter), rotm2)

    phase_centre_proj = np.zeros([rotm_c[::5].__len__()])
    temp_rotm_A_t = rotm_A_t[::5]
    temp_quat_c = quat_c[::5]
    for index, (phase_centre, inter) in enumerate(zip(phase_centre_gcrs[::5], intersatellite)):
        phase_centre_proj[index] = np.dot(phase_centre, inter / np.linalg.norm(inter))
    # plt.plot(phase_centre_proj)
    # plt.show()
    # np.savetxt('..//..//temp//simu86400//ant_phase_corr_simu_86400.txt', phase_centre_proj)
    # print(np.linalg.lstsq(eqa, phase_centre_proj))

    np.savetxt('..//..//temp//simu86400//eqb_86400.txt', dd_kbr1b.biased_range.compute().to_numpy() + dd_kbr1b.lighttime_corr.compute().to_numpy() - range)
    np.savetxt("..//..//temp//simu86400//simu_86400.txt", phase_centre_proj + alt_angle_sin[::5] / 1e3 * 1.2)
    np.savetxt("..//..//temp//simu86400//eqa_86400.txt", eqa)

    # fig = go.Figure()
    # fig.add_trace(go.Scatter(x=xf,
    #                          y=yf)
    #               )
    # fig.update_xaxes(type='log')
    # fig.update_yaxes(type='log')
    # app = dash.Dash()
    # app.layout = html.Div([
    #     dcc.Graph(figure=fig)
    # ])
#
    # app.run_server(debug=True, use_reloader=False)
    # np.savetxt(fname='..//..//temp//normal1//KBR1B_2019-01-01_X_04.txt', header="End of YAML header",
    #            X=np.c_[
    #                dd_kbr1b.gps_time.compute().to_numpy()[600: 800],
    #                dd_sca1b_d.GRACEFO_id.compute().to_numpy()[600: 800],
    #                range + phase_centre_proj + dd_kbr1b.lighttime_corr.compute().to_numpy()[600: 800],
    #                dd_kbr1b.range_rate.compute().to_numpy()[600: 800],
    #                dd_kbr1b.range_accl.compute().to_numpy()[600: 800],
    #                dd_kbr1b.iono_corr.compute().to_numpy()[600: 800],
    #                dd_kbr1b.lighttime_corr.compute().to_numpy()[600: 800],
    #                dd_kbr1b.lighttime_rate.compute().to_numpy()[600: 800],
    #                dd_kbr1b.lighttime_accl.compute().to_numpy()[600: 800]
    #            ],
    #            fmt='%s')
# 
    # np.savetxt(fname='..//..//temp//normal1//SCA1B_2019-01-01_B_04.txt', header='End of YAML header',
    #            X=np.c_[
    #                dd_sca1b_d['gps_time'].compute().to_numpy()[3000: 4000],
    #                dd_sca1b_d['sca_id'].compute().to_numpy()[3000: 4000],
    #                dd_sca1b_d['GRACEFO_id'].compute().to_numpy()[3000: 4000],
    #                dd_sca1b_d['sca_id'].compute().to_numpy()[3000: 4000],
    #                quat_c
    #            ],
    #            fmt='%s')
    # np.savetxt(fname='..//..//temp//ant_phase_centre_corr_2degree_d2.txt', fmt='%.18e', X=phase_centre_proj)
    # np.savetxt(fname='..//..//temp//SCA1B_2019-01-01_B_05_d2.txt', header='End of YAML header',
    #            X=np.c_[dd_sca1b_c['gps_time'].compute().to_numpy()[3000: 4000],
    #                    dd_sca1b_c['GRACEFO_id'].compute().to_numpy()[3000: 4000],
    #                    dd_sca1b_d['sca_id'].compute().to_numpy()[3000: 4000],
    #                    quat_c],
    #            fmt='%s')


def quater2rotm(quaternion):
    '''
    Using quaternions offered in GRACE FO 1B data product(SCA1B), the transformation matrix is
    calculated.

    INPUT:
        The quaternions of GRACE FO C/D to rotate from ICRF TO SRF.

    OUTPUT:
        The transformation matrix rotating from ICRF to SF.
    '''
    quaternion = np.array(quaternion, dtype=np.float64)
    row, _ = np.shape(quaternion)
    icrf_sf = np.zeros([row, 3, 3], dtype=np.float64)

    # using the quaternions to calculate the transformation matrix from ICRF to SF
    icrf_sf[:, 0, 0] = quaternion[:, 0]**2 + quaternion[:, 1]**2 - \
        quaternion[:, 2]**2 - quaternion[:, 3]**2
    icrf_sf[:, 0, 1] = 2 * (
        quaternion[:, 1] * quaternion[:, 2] -
        quaternion[:, 0] * quaternion[:, 3]
    )
    icrf_sf[:, 0, 2] = 2 * (
        quaternion[:, 1] * quaternion[:, 3] +
        quaternion[:, 0] * quaternion[:, 2]
    )
    icrf_sf[:, 1, 0] = 2 * (
        quaternion[:, 1] * quaternion[:, 2] +
        quaternion[:, 0] * quaternion[:, 3]
    )
    icrf_sf[:, 1, 1] = quaternion[:, 0]**2 - quaternion[:, 1]**2 + \
        quaternion[:, 2]**2 - quaternion[:, 3]**2
    icrf_sf[:, 1, 2] = 2 * (
        quaternion[:, 2] * quaternion[:, 3] -
        quaternion[:, 0] * quaternion[:, 1]
    )
    icrf_sf[:, 2, 0] = 2 * (
        quaternion[:, 1] * quaternion[:, 3] -
        quaternion[:, 0] * quaternion[:, 2]
    )
    icrf_sf[:, 2, 1] = 2 * (
        quaternion[:, 2] * quaternion[:, 3] +
        quaternion[:, 0] * quaternion[:, 1]
    )
    icrf_sf[:, 2, 2] = quaternion[:, 0]**2 - quaternion[:, 1]**2 - \
        quaternion[:, 2]**2 + quaternion[:, 3]**2

    return icrf_sf


def eul2rotm(eul):
    alpha = eul[:, 0]; beta = eul[:, 1]; gamma = eul[:, 2]
    m = np.zeros([eul.__len__(), 3, 3])
    m[:, 0, 0] =  np.cos(gamma) * np.cos(beta)
    m[:, 0, 1] =  np.cos(gamma) * np.sin(beta) * np.sin(alpha) + np.sin(gamma) * np.cos(alpha)
    m[:, 0, 2] = -np.cos(gamma) * np.sin(beta) * np.cos(alpha) + np.sin(gamma) * np.sin(alpha)
    m[:, 1, 0] = -np.sin(gamma) * np.cos(beta)
    m[:, 1, 1] = -np.sin(gamma) * np.sin(beta) * np.sin(alpha) + np.cos(gamma) * np.cos(alpha)
    m[:, 1, 2] =  np.sin(gamma) * np.sin(beta) * np.cos(alpha) + np.cos(gamma) * np.sin(alpha)
    m[:, 2, 0] =  np.sin(beta)
    m[:, 2, 1] = -np.cos(beta) * np.sin(alpha)
    m[:, 2, 2] =  np.cos(beta) * np.cos(alpha)
    return m


def load_data():
    dd_gni1b_c = dd.read_csv(urlpath='..//..//..//..//gracefo_dataset//gracefo_1B_2019-01-01_RL04.ascii.noLRI//GNI1B_2019-01-01_C_04.txt',
                             sep='\s+',
                             engine='c',
                             header=None,
                             skiprows=148,
                             names=['gps_time', 'q', 'd', 'xpos', 'ypos', 'zpos', 'xpos_err', 'ypos_err', 'zpos_err', 'xvec', 'ypev', 'zvec',
                                    'xvec_err', 'yvec_err', 'zvec_err', 'e'],)
    dd_gni1b_d = dd.read_csv(urlpath='..//..//..//..//gracefo_dataset//gracefo_1B_2019-01-01_RL04.ascii.noLRI//GNI1B_2019-01-01_D_04.txt',
                             sep='\s+',
                             engine='c',
                             header=None,
                             skiprows=148,
                             names=['gps_time', 'q', 'd', 'xpos', 'ypos', 'zpos', 'xpos_err', 'ypos_err', 'zpos_err', 'xvec', 'ypev', 'zvec',
                                    'xvec_err', 'yvec_err', 'zvec_err', 'e'],)
    dd_kbr1b = dd.read_csv(
        urlpath='..//..//..//..//gracefo_dataset//gracefo_1B_2019-01-01_RL04.ascii.noLRI//KBR1B_2019-01-01_Y_04.txt',
        engine='c',
        header=None,
        sep='\s+',
        skiprows=162,
        names=['gps_time', 'biased_range', 'range_rate', 'range_accl', 'iono_corr', 'lighttime_corr',
               'lighttime_rate',
               'lighttime_accl', 'ant_centr_corr', 'ant_centr_rate', 'ant_centr_accl', 'k_a_snr',
               'ka_a_snr', 'k_b_snr', 'ka_b_snr', 'qualflg'])

    dd_sca1b_c = dd.read_csv(urlpath='..//..//..//..//gracefo_dataset//gracefo_1B_2019-01-01_RL04.ascii.noLRI//SCA1B_2019-01-01_C_04.txt',
                             engine='c',
                             header=None,
                             sep='\s+',
                             skiprows=114,
                             names=['gps_time', 'GRACEFO_id', 'sca_id', 'q1', 'q2', 'q3', 'q4',
                                    'qual_rss', 'qualflg'],
                             dtype={'q1': np.longdouble, 'q2': np.longdouble, 'q3': np.longdouble,
                                    'q4': np.longdouble})
    dd_sca1b_d = dd.read_csv(urlpath='..//..//..//..//gracefo_dataset//gracefo_1B_2019-01-01_RL04.ascii.noLRI//SCA1B_2019-01-01_D_04.txt',
                             engine='c',
                             header=None,
                             sep='\s+',
                             skiprows=114,
                             names=['gps_time', 'GRACEFO_id', 'sca_id', 'q1', 'q2', 'q3', 'q4',
                                    'qual_rss', 'qualflg'],
                             dtype={'q1': np.longdouble, 'q2': np.longdouble, 'q3': np.longdouble,
                                    'q4': np.longdouble})

    return dd_kbr1b, dd_sca1b_c, dd_sca1b_d, dd_gni1b_c, dd_gni1b_d


if __name__ == '__main__':
    main()
