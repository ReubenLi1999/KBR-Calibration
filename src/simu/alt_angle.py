import numpy as np
import dask.dataframe as dd
import matplotlib.pyplot as plt
from pyquaternion import Quaternion
from scipy.signal import welch
from scipy.fft import fft, fftfreq


def main():
    phase_centre_c = np.asarray([1.49, 0.02, 0.03])  # unit m
    phase_centre_d = np.asarray([1.50, 0.03, 0.04])  # unit m

    dd_kbr1b, dd_sca1b_c, dd_sca1b_d, dd_gni1b_c, dd_gni1b_d = load_data()
    quaternion_c = np.c_[dd_sca1b_c.q1.compute().to_numpy()[0: 86400],
                         dd_sca1b_c.q2.compute().to_numpy()[0: 86400],
                         dd_sca1b_c.q3.compute().to_numpy()[0: 86400],
                         dd_sca1b_c.q4.compute().to_numpy()[0: 86400]]
    quaternion_d = np.c_[dd_sca1b_d.q1.compute().to_numpy()[0: 86400],
                         dd_sca1b_d.q2.compute().to_numpy()[0: 86400],
                         dd_sca1b_d.q3.compute().to_numpy()[0: 86400],
                         dd_sca1b_d.q4.compute().to_numpy()[0: 86400]]
    biased_range = dd_kbr1b.biased_range.compute().to_numpy()[0: 17280]
    rotm_gcrs2srf_c = quater2rotm(quaternion_c)
    rotm_srf2gcrs_c = rotm_gcrs2srf_c.transpose((0, 2, 1))
    rotm_gcrs2srf_d = quater2rotm(quaternion_d)
    rotm_srf2gcrs_d = rotm_gcrs2srf_d.transpose((0, 2, 1))

    # altitude angle from srf_new to srf
    alt_angle_sin = np.deg2rad(2 + 1 * np.sin(2. * np.pi / 249 * dd_sca1b_c.gps_time.compute().to_numpy()[0: 86400]))

    # yaw
    yaw_sin = np.c_[np.zeros(alt_angle_sin.__len__()), np.zeros(alt_angle_sin.__len__()), alt_angle_sin]
    pitch_sin = np.c_[np.zeros(alt_angle_sin.__len__()), alt_angle_sin, np.zeros(alt_angle_sin.__len__())]
    rotm_yaw = eul2rotm(yaw_sin)
    # rotm_yaw = eul2rotm(pitch_sin).transpose((0, 2, 1))
    rotm_pitch = eul2rotm(pitch_sin)

    # let the leading satellite rotate
    rotm_c = np.matmul(rotm_yaw, rotm_gcrs2srf_c.transpose((0, 2, 1)))
    # rotm_c = rotm_srf2gcrs_c
    rotm_d = rotm_gcrs2srf_d

    rotm_A_t = rotm_c.transpose((0, 2, 1))
    rotm_B_t = rotm_d  # .transpose((0, 2, 1))
    quat_c = np.zeros([rotm_c.__len__(), 4])
    for index, rot_m in enumerate(rotm_A_t):
        temp = Quaternion(matrix=rot_m)
        quat_c[index, :] = temp.elements
    phase_centre_gcrs = np.zeros([rotm_c.__len__(), 3])

    # print(rotm_B_t)
    for index, (c, d) in enumerate(zip(rotm_A_t, rotm_B_t)):
        phase_centre_gcrs[index] = -np.matmul(c, phase_centre_c) + np.matmul(d, phase_centre_d)

    intersatellite = np.c_[
        dd_gni1b_c.xpos.compute().to_numpy()[0: 17280] - dd_gni1b_d.xpos.compute().to_numpy()[0: 17280],
        dd_gni1b_c.ypos.compute().to_numpy()[0: 17280] - dd_gni1b_d.ypos.compute().to_numpy()[0: 17280],
        dd_gni1b_c.zpos.compute().to_numpy()[0: 17280] - dd_gni1b_d.zpos.compute().to_numpy()[0: 17280]]
    range = np.linalg.norm(intersatellite, axis=1)

    eqa = np.zeros([intersatellite.__len__(), 6])
    for index, (inter, rotm1, rotm2) in enumerate(zip(intersatellite, rotm_A_t[::5], rotm_B_t[::5])):
        eqa[index, : 3] = np.matmul(inter / np.linalg.norm(inter), rotm1)
        eqa[index, 3:] = np.matmul(inter / np.linalg.norm(inter), rotm2)

    phase_centre_proj = np.zeros([rotm_c[::5].__len__()])
    temp_rotm_A_t = rotm_A_t[::5]
    temp_quat_c = quat_c[::5]
    for index, (phase_centre, inter) in enumerate(zip(phase_centre_gcrs[::5], intersatellite)):
        phase_centre_proj[index] = np.dot(phase_centre, inter / np.linalg.norm(inter))
    phase_centre_proj1 = phase_centre_proj
    # plt.plot(phase_centre_proj[0: 1000])
    # plt.xlabel('gps time [s]')
    # plt.ylabel('phase_centre_correction[m]')
    # plt.show()
    # np.savetxt(fname='..//..//temp//ant_phase_centre_corr_2degree_c2.txt', fmt='%.18e', X=phase_centre_proj)

    freq, psd = welch(
        dd_kbr1b.biased_range.compute().to_numpy() - range + dd_kbr1b.lighttime_corr.compute().to_numpy() + phase_centre_proj,
        0.2, ('kaiser', 60.), phase_centre_proj.__len__(), scaling='density')
    plt.loglog(freq, np.sqrt(psd))
    plt.title('post-fit')
    plt.ylabel('asd')
    plt.xlabel('freq [Hz]')
    plt.show()

    # altitude angle from srf_new to srf
    alt_angle_sin = np.deg2rad(2 + 1 * np.sin(2. * np.pi / 250 * dd_sca1b_c.gps_time.compute().to_numpy()[0: 86400]))

    # yaw
    yaw_sin = np.c_[np.zeros(alt_angle_sin.__len__()), np.zeros(alt_angle_sin.__len__()), alt_angle_sin]
    pitch_sin = np.c_[np.zeros(alt_angle_sin.__len__()), alt_angle_sin, np.zeros(alt_angle_sin.__len__())]
    rotm_yaw = eul2rotm(yaw_sin)
    # rotm_yaw = eul2rotm(pitch_sin).transpose((0, 2, 1))
    rotm_pitch = eul2rotm(pitch_sin)

    # let the leading satellite rotate
    rotm_c = np.matmul(rotm_yaw, rotm_gcrs2srf_c.transpose((0, 2, 1)))
    # rotm_c = rotm_srf2gcrs_c
    rotm_d = rotm_gcrs2srf_d

    rotm_A_t = rotm_c.transpose((0, 2, 1))
    rotm_B_t = rotm_d  # .transpose((0, 2, 1))
    quat_c = np.zeros([rotm_c.__len__(), 4])
    for index, rot_m in enumerate(rotm_A_t):
        temp = Quaternion(matrix=rot_m)
        quat_c[index, :] = temp.elements
    phase_centre_gcrs = np.zeros([rotm_c.__len__(), 3])

    # print(rotm_B_t)
    for index, (c, d) in enumerate(zip(rotm_A_t, rotm_B_t)):
        phase_centre_gcrs[index] = -np.matmul(c, phase_centre_c) + np.matmul(d, phase_centre_d)

    intersatellite = np.c_[
        dd_gni1b_c.xpos.compute().to_numpy()[0: 17280] - dd_gni1b_d.xpos.compute().to_numpy()[0: 17280],
        dd_gni1b_c.ypos.compute().to_numpy()[0: 17280] - dd_gni1b_d.ypos.compute().to_numpy()[0: 17280],
        dd_gni1b_c.zpos.compute().to_numpy()[0: 17280] - dd_gni1b_d.zpos.compute().to_numpy()[0: 17280]]
    range = np.linalg.norm(intersatellite, axis=1)

    eqa = np.zeros([intersatellite.__len__(), 6])
    for index, (inter, rotm1, rotm2) in enumerate(zip(intersatellite, rotm_A_t[::5], rotm_B_t[::5])):
        eqa[index, : 3] = np.matmul(inter / np.linalg.norm(inter), rotm1)
        eqa[index, 3:] = np.matmul(inter / np.linalg.norm(inter), rotm2)

    phase_centre_proj = np.zeros([rotm_c[::5].__len__()])
    temp_rotm_A_t = rotm_A_t[::5]
    temp_quat_c = quat_c[::5]
    for index, (phase_centre, inter) in enumerate(zip(phase_centre_gcrs[::5], intersatellite)):
        phase_centre_proj[index] = np.dot(phase_centre, inter / np.linalg.norm(inter))

    plt.plot(eqa @ [-1.49, -0.02, -0.03, 1.5, 0.03, 0.04])
    plt.plot(phase_centre_proj)
    plt.xlabel('gps time [s]')
    plt.ylabel('phase_centre_correction[m]')
    plt.show()
    # yf = fft(phase_centre_proj)
    # yf_1 = fft(phase_centre_proj1)
    # yf_plot = 2.0 / 200 * np.abs(yf[0: 200//2])
    # yf_plot1 = 2.0 / 200 * np.abs(yf_1[0: 200//2])
    # xf = fftfreq(200, 5)[:200//2]
    # plt.loglog(xf, yf_plot, label='250s')
    # plt.loglog(xf, yf_plot1, label='249s')
    # plt.ylabel('Amplitude [m]')
    # plt.xlabel('Frequency [Hz]')
    # plt.legend()
    # plt.show()
    # print(yf_plot[np.isclose(xf, 1 / 250)])
    # print(yf_plot[np.isclose(xf, 2 / 250)])

    # np.savetxt(fname='..//..//temp//SCA1B_2019-01-01_A_05_c2.txt', header='End of YAML header',
    #           X=np.c_[dd_sca1b_c['gps_time'].compute().to_numpy()[1000: 2000],
    #                   dd_sca1b_c['GRACEFO_id'].compute().to_numpy()[1000: 2000],
    #                   dd_sca1b_c['sca_id'].compute().to_numpy()[1000: 2000],
    #                   quat_c],
    #           fmt='%s')


def quater2rotm(quaternion):
    """ Using quaternions offered in GRACE FO 1B data product(SCA1B), the transformation matrix is
    calculated.

    INPUT:
        The quaternions of GRACE FO C/D to rotate from ICRF TO SRF.

    OUTPUT:
        The transformation matrix rotating from ICRF to SF.
    """
    quaternion = np.array(quaternion, dtype=np.float64)
    row, _ = np.shape(quaternion)
    icrf_sf = np.zeros([row, 3, 3], dtype=np.float64)

    # using the quaternions to calculate the transformation matrix from ICRF to SF
    icrf_sf[:, 0, 0] = quaternion[:, 0] ** 2 + quaternion[:, 1] ** 2 - \
                       quaternion[:, 2] ** 2 - quaternion[:, 3] ** 2
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
    icrf_sf[:, 1, 1] = quaternion[:, 0] ** 2 - quaternion[:, 1] ** 2 + \
                       quaternion[:, 2] ** 2 - quaternion[:, 3] ** 2
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
    icrf_sf[:, 2, 2] = quaternion[:, 0] ** 2 - quaternion[:, 1] ** 2 - \
                       quaternion[:, 2] ** 2 + quaternion[:, 3] ** 2

    return icrf_sf


def eul2rotm(eul):
    alpha = eul[:, 0]
    beta = eul[:, 1]
    gamma = eul[:, 2]
    m = np.zeros([eul.__len__(), 3, 3])
    m[:, 0, 0] = np.cos(gamma) * np.cos(beta)
    m[:, 0, 1] = np.cos(gamma) * np.sin(beta) * np.sin(alpha) + np.sin(gamma) * np.cos(alpha)
    m[:, 0, 2] = -np.cos(gamma) * np.sin(beta) * np.cos(alpha) + np.sin(gamma) * np.sin(alpha)
    m[:, 1, 0] = -np.sin(gamma) * np.cos(beta)
    m[:, 1, 1] = -np.sin(gamma) * np.sin(beta) * np.sin(alpha) + np.cos(gamma) * np.cos(alpha)
    m[:, 1, 2] = np.sin(gamma) * np.sin(beta) * np.cos(alpha) + np.cos(gamma) * np.sin(alpha)
    m[:, 2, 0] = np.sin(beta)
    m[:, 2, 1] = -np.cos(beta) * np.sin(alpha)
    m[:, 2, 2] = np.cos(beta) * np.cos(alpha)
    return m


def load_data():
    dd_gni1b_c = dd.read_csv(urlpath='..//..//output/A-report.txt',
                             sep='\s+',
                             engine='c',
                             header=None,
                             skiprows=1,
                             names=['gps_time', 'xpos', 'ypos', 'zpos', 'vx', 'vy', 'vz'],
                             dtype=np.longdouble)
    dd_gni1b_d = dd.read_csv(urlpath='..//..//output/B-report.txt',
                             sep='\s+',
                             engine='c',
                             header=None,
                             skiprows=1,
                             names=['gps_time', 'xpos', 'ypos', 'zpos', 'vx', 'vy', 'vz'],
                             dtype=np.longdouble)
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
    a = np.asarray([1])
    main()
