import numpy as np
import dask.dataframe as dd


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


def main():
    acc_c_srf = np.loadtxt(fname="..//..//input//Satellite_A(1).txt", dtype=np.longdouble, skiprows=1,
                       usecols=[1, 2, 3])
    acc_d_srf = np.loadtxt(fname="..//..//input//Satellite_B(1).txt", dtype=np.longdouble, skiprows=1,
                       usecols=[1, 2, 3])
    dd_sca1b_c = dd.read_csv(urlpath='..//..//input///SCA1B_2019-01-01_C_05.txt',
                             engine='c',
                             header=None,
                             sep='\s+',
                             skiprows=1,
                             names=['gps_time', 'GRACEFO_id', 'sca_id', 'q1', 'q2', 'q3', 'q4',
                                    'qual_rss', 'qualflg'],
                             dtype={'q1': np.longdouble, 'q2': np.longdouble, 'q3': np.longdouble,
                                    'q4': np.longdouble})
    dd_sca1b_d = dd.read_csv(urlpath='..//..//input///SCA1B_2019-01-01_D_04.txt',
                             engine='c',
                             header=None,
                             sep='\s+',
                             skiprows=114,
                             names=['gps_time', 'GRACEFO_id', 'sca_id', 'q1', 'q2', 'q3', 'q4',
                                    'qual_rss', 'qualflg'],
                             dtype={'q1': np.longdouble, 'q2': np.longdouble, 'q3': np.longdouble,
                                    'q4': np.longdouble})
    quaternion_c = np.c_[dd_sca1b_c.q1.compute().to_numpy()[::5],
                         dd_sca1b_c.q2.compute().to_numpy()[::5],
                         dd_sca1b_c.q3.compute().to_numpy()[::5],
                         dd_sca1b_c.q4.compute().to_numpy()[::5]]
    quaternion_d = np.c_[dd_sca1b_d.q1.compute().to_numpy()[::5],
                         dd_sca1b_d.q2.compute().to_numpy()[::5],
                         dd_sca1b_d.q3.compute().to_numpy()[::5],
                         dd_sca1b_d.q4.compute().to_numpy()[::5]]
    rotm_c_gcrs2srf = quater2rotm(quaternion_c)
    rotm_d_gcrs2srf = quater2rotm(quaternion_d)
    rotm_c_srf2gcrs = rotm_c_gcrs2srf.transpose((0, 2, 1))
    rotm_d_srf2gcrs = rotm_d_gcrs2srf.transpose((0, 2, 1))
    acc_c_gcrs = np.zeros([acc_c_srf.__len__(), 3])
    acc_d_gcrs = np.zeros([acc_d_srf.__len__(), 3])
    for index, (c_srf, d_srf) in enumerate(zip(acc_c_srf, acc_d_srf)):
        acc_c_gcrs[index] = np.matmul(rotm_c_gcrs2srf[index], c_srf)
        acc_d_gcrs[index] = np.matmul(rotm_d_gcrs2srf[index], d_srf)
    np.savetxt(fname='..//..//input//ACC1B_2019-01-01_C_04.txt', X=np.c_[dd_sca1b_c.gps_time.compute().to_numpy()[::5],
                                                                         acc_c_gcrs],
               header='End of YAML header')
    np.savetxt(fname='..//..//input//ACC1B_2019-01-01_D_04.txt', X=np.c_[dd_sca1b_d.gps_time.compute().to_numpy()[::5],
                                                                         acc_d_gcrs],
               header='End of YAML header')


if __name__ == '__main__':
    main()
