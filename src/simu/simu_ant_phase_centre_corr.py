import numpy as np
import dask.dataframe as dd
import matplotlib.pyplot as plt
from pyquaternion import Quaternion
from scipy.signal import welch


def main():
    # phase centre vector in SRF
    phase_centre_c = np.asarray([1.49, 0.02, 0.03])  # unit m
    phase_centre_d = np.asarray([1.50, 0.03, 0.04])  # unit m

    # motivation signal
    alt_angle_sin = np.deg2rad(2 + 1 * np.sin(2. * np.pi / 250. * np.arange(0, 21600, 1)))
    plt.plot(alt_angle_sin)
    plt.show()

    # phase centre correction
    phase_centre_corr_1_pitch = phase_centre_c[0] + phase_centre_d[0] * np.cos(alt_angle_sin) - phase_centre_d[2] * np.sin(alt_angle_sin)
    # phase_centre_corr_1_pitch = phase_centre_corr_1_pitch - np.mean(phase_centre_corr_1_pitch)

    plt.plot(phase_centre_corr_1_pitch)
    plt.show()
    freq, psd = welch(
        phase_centre_corr_1_pitch,
        0.2, ('kaiser', 60.), 1000, scaling='density')
    plt.loglog(freq, np.sqrt(psd))
    plt.title('post-fit')
    plt.ylabel('asd')
    plt.xlabel('freq [Hz]')
    plt.show()


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


def psd_ant_phase():
    phase_centre_corr = np.loadtxt('..//..//input//ant_phase_centre_corr_2degree.txt')
    plt.plot(phase_centre_corr)
    plt.show()
    freq, psd = welch(
        phase_centre_corr[0: 1000],
        0.2, ('kaiser', 60.), 1000, scaling='density')
    plt.loglog(freq, np.sqrt(psd))
    plt.title('post-fit')
    plt.ylabel('asd')
    plt.xlabel('freq [Hz]')
    plt.show()


if __name__ == "__main__":
    main()
    # psd_ant_phase()

