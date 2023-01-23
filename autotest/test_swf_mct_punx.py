"""

This test uses the SWF MCT Package to simulate flow in the
Mahoning Creek using the Punxsutawney river cross section.  
Data used for this test comes from a HEC-HMS example problem 
described at:

https://www.hec.usace.army.mil/confluence/hmsdocs/hmsguides/applying-reach-routing-methods-within-hec-hms/applying-the-muskingum-cunge-routing-method

The main purpose of this test is to exercise the multi-point
cross section capability.  The Punxsutawney cross section has
28 points, and the simulation does not work with a regular
Newton-Raphson iteration; instead, a slower and more robust
bisection method was added in order to calculate flow from 
the simulated reach depth.

The HEC-HMS example compares HEC-HMS results with simulated
results using HEC-RAS.  This test compares the mf6 SWF model
with the HEC-RAS output.  While the results are similar, the
fit could probably be improved.

"""

import os

import flopy
import numpy as np
import pytest
from framework import TestFramework
from simulation import TestSimulation

ex = [f"swf-mct-punx"]

hec_hms_results = """11Apr1994 12:00 0.5 0.5 248.7
11Apr1994 12:15 17.2 0.5 247.5
11Apr1994 12:30 33.9 0.5 246.2
11Apr1994 12:45 50.5 0.5 245.0
11Apr1994 13:00 67.2 0.5 243.8
11Apr1994 13:15 148.8 0.5 239.3
11Apr1994 13:30 230.3 0.5 234.9
11Apr1994 13:45 311.9 0.5 230.5
11Apr1994 14:00 393.4 0.5 226.0
11Apr1994 14:15 617.7 0.5 218.6
11Apr1994 14:30 841.9 0.5 211.1
11Apr1994 14:45 1066.1 0.5 203.6
11Apr1994 15:00 1290.3 0.5 196.2
11Apr1994 15:15 1800.5 0.5 187.6
11Apr1994 15:30 2310.6 0.5 179.1
11Apr1994 15:45 2820.8 0.5 170.6
11Apr1994 16:00 3331.0 0.5 162.0
11Apr1994 16:15 4139.1 0.5 154.4
11Apr1994 16:30 4947.2 0.5 146.7
11Apr1994 16:45 5755.3 0.7 139.0
11Apr1994 17:00 6563.4 1.4 131.4
11Apr1994 17:15 7371.9 3.2 210.5
11Apr1994 17:30 8180.3 7.5 289.7
11Apr1994 17:45 8988.7 18.4 368.8
11Apr1994 18:00 9797.1 244.1 448.0
11Apr1994 18:15 10415.0 2019.5 1729.1
11Apr1994 18:30 11033.0 3711.9 3010.3
11Apr1994 18:45 11650.9 5059.6 4291.4
11Apr1994 19:00 12268.9 6179.0 5572.6
11Apr1994 19:15 12425.2 7086.4 6212.1
11Apr1994 19:30 12581.5 7762.3 6851.7
11Apr1994 19:45 12737.8 8207.5 7491.2
11Apr1994 20:00 12894.2 8462.1 8130.7
11Apr1994 20:15 12708.5 8599.2 8378.0
11Apr1994 20:30 12522.9 8741.3 8625.3
11Apr1994 20:45 12337.3 8912.1 8872.6
11Apr1994 21:00 12151.7 9121.2 9119.9
11Apr1994 21:15 11822.8 9366.0 9433.7
11Apr1994 21:30 11493.9 9645.5 9747.5
11Apr1994 21:45 11165.0 9960.1 10061.3
11Apr1994 22:00 10836.1 10311.8 10375.0
11Apr1994 22:15 10489.3 10679.4 10581.8
11Apr1994 22:30 10142.5 11034.8 10788.5
11Apr1994 22:45 9795.7 11352.9 10995.3
11Apr1994 23:00 9448.9 11614.5 11202.0
11Apr1994 23:15 9133.5 11808.7 11224.8
11Apr1994 23:30 8818.1 11931.7 11247.5
11Apr1994 23:45 8502.7 11985.8 11270.3
12Apr1994 00:00 8187.4 11976.9 11293.0
12Apr1994 00:15 7842.3 11912.9 11168.0
12Apr1994 00:30 7497.3 11802.6 11043.0
12Apr1994 00:45 7152.2 11654.0 10918.0
12Apr1994 01:00 6807.1 11473.5 10793.0
12Apr1994 01:15 6546.2 11264.4 10539.6
12Apr1994 01:30 6285.2 11023.9 10286.3
12Apr1994 01:45 6024.2 10734.7 10033.0
12Apr1994 02:00 5763.3 10351.3 9779.6
12Apr1994 02:15 5558.1 9735.4 9409.9
12Apr1994 02:30 5353.0 8728.7 9040.1
12Apr1994 02:45 5147.8 7488.3 8670.3
12Apr1994 03:00 4942.7 6958.2 8300.6
12Apr1994 03:15 4769.0 6621.1 7814.1
12Apr1994 03:30 4595.4 6349.5 7327.6
12Apr1994 03:45 4421.7 6106.4 6841.1
12Apr1994 04:00 4248.1 5880.4 6354.6
12Apr1994 04:15 4098.0 5667.4 6092.0
12Apr1994 04:30 3948.0 5465.5 5829.4
12Apr1994 04:45 3797.9 5273.4 5566.9
12Apr1994 05:00 3647.8 5089.9 5304.3
12Apr1994 05:15 3519.1 4914.2 5117.9
12Apr1994 05:30 3390.3 4745.5 4931.6
12Apr1994 05:45 3261.6 4583.2 4745.2
12Apr1994 06:00 3132.8 4426.8 4558.9
12Apr1994 06:15 3022.3 4276.0 4408.9
12Apr1994 06:30 2911.9 4130.5 4258.9
12Apr1994 06:45 2801.4 3990.1 4108.9
12Apr1994 07:00 2690.9 3854.5 3959.0
12Apr1994 07:15 2596.2 3723.6 3831.8
12Apr1994 07:30 2501.4 3597.3 3704.7
12Apr1994 07:45 2406.6 3475.5 3577.6
12Apr1994 08:00 2311.8 3357.9 3450.5
12Apr1994 08:15 2230.5 3244.5 3340.6
12Apr1994 08:30 2149.2 3135.1 3230.6
12Apr1994 08:45 2067.9 3029.5 3120.7
12Apr1994 09:00 1986.6 2927.7 3010.7
12Apr1994 09:15 1916.8 2829.4 2917.2
12Apr1994 09:30 1847.1 2734.6 2823.7
12Apr1994 09:45 1777.3 2643.1 2730.2
12Apr1994 10:00 1707.5 2554.8 2636.6
12Apr1994 10:15 1647.7 2469.6 2554.1
12Apr1994 10:30 1587.8 2387.4 2471.5
12Apr1994 10:45 1527.9 2308.1 2389.0
12Apr1994 11:00 1468.0 2231.5 2306.4
12Apr1994 11:15 1416.6 2157.6 2235.0
12Apr1994 11:30 1365.1 2086.3 2163.6
12Apr1994 11:45 1313.7 2017.5 2092.2
12Apr1994 12:00 1262.3 1951.1 2020.8
12Apr1994 12:15 1218.1 1886.9 1959.6
12Apr1994 12:30 1174.0 1825.0 1898.3
12Apr1994 12:45 1129.8 1765.3 1837.0
12Apr1994 13:00 1085.6 1707.6 1775.8
12Apr1994 13:15 1047.7 1651.9 1721.4
12Apr1994 13:30 1009.8 1598.1 1667.1
12Apr1994 13:45 971.8 1546.2 1612.8
12Apr1994 14:00 933.9 1496.1 1558.4
12Apr1994 14:15 901.3 1447.6 1510.8
12Apr1994 14:30 868.7 1400.9 1463.1
12Apr1994 14:45 836.2 1355.7 1415.5
12Apr1994 15:00 803.6 1312.1 1367.8
12Apr1994 15:15 775.3 1270.0 1328.6
12Apr1994 15:30 747.1 1229.3 1289.4
12Apr1994 15:45 718.8 1190.0 1250.2
12Apr1994 16:00 690.6 1152.0 1211.0
12Apr1994 16:15 665.9 1115.4 1173.3
12Apr1994 16:30 641.2 1079.9 1135.6
12Apr1994 16:45 616.5 1045.7 1097.9
12Apr1994 17:00 591.7 1012.6 1060.2
12Apr1994 17:15 570.5 980.7 1029.8
12Apr1994 17:30 549.2 949.8 999.5
12Apr1994 17:45 528.0 919.9 969.1
12Apr1994 18:00 506.7 891.1 938.7
12Apr1994 18:15 488.5 863.2 913.4
12Apr1994 18:30 470.3 836.2 888.2
12Apr1994 18:45 452.1 810.1 862.9
12Apr1994 19:00 433.9 784.8 837.6
12Apr1994 19:15 418.4 760.3 811.4
12Apr1994 19:30 402.8 736.6 785.1
12Apr1994 19:45 387.2 713.6 758.8
12Apr1994 20:00 371.7 691.3 732.5
12Apr1994 20:15 358.4 669.8 709.3
12Apr1994 20:30 345.1 648.8 686.1
12Apr1994 20:45 331.7 628.6 662.9
12Apr1994 21:00 318.4 609.0 639.7
12Apr1994 21:15 307.0 590.0 624.0
12Apr1994 21:30 295.6 571.6 608.2
12Apr1994 21:45 284.2 553.8 592.5
12Apr1994 22:00 272.8 536.7 576.7
12Apr1994 22:15 263.1 520.0 562.3
12Apr1994 22:30 253.3 504.0 548.0
12Apr1994 22:45 243.6 488.4 533.6
12Apr1994 23:00 233.8 473.4 519.2
12Apr1994 23:15 225.5 458.9 504.0
12Apr1994 23:30 217.1 444.9 488.8
12Apr1994 23:45 208.8 431.3 473.6
13Apr1994 00:00 200.4 418.2 458.4"""

cxs_data = """0 35.1
7.5 26.2
14.1 19.7
20 15.4
46.3 15.4
69.6 13.1
88.3 14.1
106.6 16.1
122.7 15.1
124.3 7.9
128.9 4.3
127.6 1
134.8 0
158.1 1
178.8 1.6
201.4 2
211 3.6
219.2 6.6
223.1 13.1
225.1 15.1
244.8 15.4
255.9 15.1
273.3 14.4
305.1 15.1
315.9 16.4
321.9 19.7
338.9 26.2
345.1 35.1"""


def get_flow_data():
    s = [line for line in hec_hms_results.split("\n")]
    s = [line.split(" ") for line in s]
    s = [(date, time, float(inflow), float(outflow), float(obs)) for date, time, inflow, outflow, obs in s]
    date, time, inflow, hec_hms_outflow, obs = zip(*s)
    inflow_hydrograph = np.array(inflow)
    hec_hms_outflow = np.array(hec_hms_outflow)
    outflow_hydrograph = np.array(obs)
    dt = 15 * 60 # 15 mins converted to seconds
    total_time = inflow_hydrograph.shape[0] * dt
    sample_times = np.arange(0, total_time, dt)
    return sample_times, inflow_hydrograph, outflow_hydrograph


def get_cross_section_data():
    s = [line for line in cxs_data.split("\n")]
    s = [line.split(" ") for line in s]
    s = [(float(x), float(y)) for x, y in s]
    x, h = zip(*s)
    x = np.array(x)
    h = np.array(h)
    channel_n = 0.035
    bank_n = 0.15
    r = np.array(8 * [bank_n] + 11 * [channel_n] + 8 * [bank_n] + [0])
    cross_section_data = {
    "x": x,  # feet
    "h": h,  # feet
    "r": r,  # mannings n in feet units
    }
    return cross_section_data


def build_model(idx, dir):

    sim_ws = dir
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=sim_ws,
        memory_print_option='all',
    )

    # get data
    sample_times, inflow_hydrograph, outflow_hydrograph = get_flow_data()
    cross_section_data = get_cross_section_data()

    # spatial discretization
    stream_length = 68861.
    nreach = 8
    dx = stream_length / nreach

    # temporal discretization
    total_time = sample_times[-1]
    nstp = 72
    dt = total_time / nstp

    tdis = flopy.mf6.ModflowTdis(sim, 
        nper=1, 
        perioddata=[(total_time, nstp, 1.0)], 
        time_units="seconds")
    ems = flopy.mf6.ModflowEms(sim)
    swf = flopy.mf6.ModflowSwf(sim, modelname=name, save_flows=True)

    vertices = []
    vertices = [[j, j * dx, 0., 0.] for j in range(nreach + 1)]
    cell2d = []
    for j in range(nreach):
        cell2d.append([j, 0.5, 2, j, j + 1])
    toreach = [j + 1 for j in range(nreach - 1)] + [-1]
    nodes = nreach
    nvert = nreach + 1

    disl = flopy.mf6.ModflowSwfdisl(
        swf, 
        nodes=nodes, 
        nvert=nvert,
        reach_length=dx,
        toreach=toreach,   # -1 gives 0 in one-based, which means outflow cell
        reach_bottom=0.,
        idomain=1, 
        vertices=vertices, 
        cell2d=cell2d,
    )
    
    # note: for specifying zero-based reach number, put reach number in tuple
    fname = f"{name}.mmr.obs.csv"
    mmr_obs = {
        fname: [
            ("OUTFLOW", "EXT-OUTFLOW", (nodes - 1,)),
        ],
        "digits": 10,
    }

    mct = flopy.mf6.ModflowSwfmct(
        swf,
        unitconv=1.49,
        observations=mmr_obs,
        print_flows=True,
        save_flows=True,
        icalc_order=list(range(nreach)),
        qoutflow0=inflow_hydrograph[0],
        width=1.,
        manningsn=1.0,
        elevation=0.,
        slope=0.0012,
        idcxs=0,
    )

    xfraction = cross_section_data["x"]
    height = cross_section_data["h"]
    mannfraction = cross_section_data["r"]
    npoints = xfraction.shape[0]
    cxsdata = list(zip(xfraction, height, mannfraction))
    cxs = flopy.mf6.ModflowSwfcxs(
        swf,
        nsections=1,
        npoints=npoints,
        packagedata=[(0, npoints)],
        crosssectiondata=cxsdata,
    )

    # output control
    oc = flopy.mf6.ModflowSwfoc(
        swf,
        budget_filerecord=f"{name}.bud",
        qoutflow_filerecord=f"{name}.qoutflow",
        saverecord=[("QOUTFLOW", "ALL"), ("BUDGET", "ALL"), ],
        printrecord=[("QOUTFLOW", "LAST"),("BUDGET", "ALL"), ],
    )

    # Create flw package with time series input
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=1,
        print_input=True,
        print_flows=True,
        stress_period_data=[(0, "inflow")],
    )
    fname = f"{name}.flw.ts"
    flw.ts.initialize(
        filename=fname,
        timeseries=list(zip(sample_times, inflow_hydrograph)),
        time_series_namerecord=["inflow"],
        interpolation_methodrecord=["linearend"],
    )

    return sim, None


def eval_model(sim):
    print("evaluating model...")

    # get data
    sample_times, inflow_hydrograph, outflow_hydrograph = get_flow_data()
    outflow_hydrograph = outflow_hydrograph[1:]  # skip time zero

    # read the observation output and compare max outflow with hec-ras max outflow
    name = ex[sim.idxsim]
    fpth = os.path.join(sim.simpath, f"{name}.mmr.obs.csv")
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")
    qoutflow = -obsvals['OUTFLOW']
    dtol = 800.
    diff = qoutflow.max() - outflow_hydrograph.max()
    assert abs(diff) < dtol, f"Sim and reported max outflow are too different: {diff}"

    # read the binary grid file
    fpth = os.path.join(sim.simpath, f"{name}.disl.grb")
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    ja = grb.ja
    assert ia.shape[0] == grb.nodes + 1, "ia in grb file is not correct size"

    # read qoutflow file
    fpth = os.path.join(sim.simpath, f"{name}.qoutflow")
    qobj = flopy.utils.HeadFile(fpth, precision="double", text="QOUTFLOW")
    qoutflow = qobj.get_alldata()

    # read the budget file
    fpth = os.path.join(sim.simpath, f"{name}.bud")
    budobj = flopy.utils.binaryfile.CellBudgetFile(fpth)
    flowja = budobj.get_data(text="FLOW-JA-FACE")
    qstorage = budobj.get_data(text="STORAGE")
    qflw = budobj.get_data(text="FLW")
    qextoutflow = budobj.get_data(text="EXT-OUTFLOW")
    qresidual = np.zeros(grb.nodes)

    # check budget terms
    for itime in range(len(flowja)):
        print (f"evaluating timestep {itime}")

        fja = flowja[itime].flatten()
        for n in range(grb.nodes):
            ipos = ia[n]
            qresidual[n] = fja[ipos]
        assert np.allclose(qresidual, 0.), "residual in flowja diagonal is not zero"

        for n in range(grb.nodes):
            qs = qstorage[itime].flatten()[n]
            if n + 1 in qflw[itime]["node"]:
                idx, = np.where(qflw[itime]["node"] == n + 1)
                idx = idx[0]
                qf = qflw[itime].flatten()["q"][idx]
            else:
                qf = 0.
            qe = qextoutflow[itime].flatten()[n]
            qdiag = fja[ia[n]]
            print(f"{n=} {qs=} {qf=} {qe=} {qdiag=}")
            for ipos in range(ia[n] + 1, ia[n + 1]):
                j = ja[ipos]
                q = fja[ipos]
                print(f"  {ipos=} {j=} {q=}")        

    return


@pytest.mark.parametrize(
    "idx, name",
    list(enumerate(ex)),
)
def test_mf6model(idx, name, function_tmpdir, targets):
    ws = str(function_tmpdir)
    test = TestFramework()
    test.build(build_model, idx, ws)
    test.run(
        TestSimulation(
            name=name, exe_dict=targets, exfunc=eval_model, idxsim=idx
        ),
        ws,
    )
